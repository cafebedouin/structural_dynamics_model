% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__expansive_construction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: unclos_maritime_sovereignty__expansive_construction_reading
 *   human_readable: Expansive Construction Reading — Artificial Island Territorial Sea Claims
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This story instantiates the expansive-construction reading of the UNCLOS
 *   maritime sovereignty kernel: the claim that building permanent structures
 *   on submerged reefs or low-tide elevations, followed by sustained
 *   administrative control and garrisoning, generates de facto territorial
 *   waters and EEZ entitlements functionally equivalent to natural islands.
 *   This reading is held and actively pursued by island-constructing states
 *   as the legal basis for their construction programs. It is distinct from
 *   the strict geographic reading (Article 121(3) denies artificial/submerged
 *   features any entitlement) and the hybrid effective-control reading
 *   (natural features get full entitlement, artificial features get only a
 *   500m safety zone unless effective control matures over time). Each
 *   reading is authored as its own constraint with its own ε; this file's ε
 *   describes the arrangement as this reading's proponents actually operate
 *   it — an aggressive, actively enforced expansion of maritime jurisdiction
 *   via construction and occupation — not the reading's own self-justifying
 *   rhetoric of legitimate coastal state prerogative.
 *
 * KEY AGENTS:
 *   - island_constructing_states: primary beneficiary and agenda-setter (institutional/arbitrage) — controls physical facts and administrative narrative
 *   - neighboring_claimant_states: primary target (moderate/constrained) — loses adjacent maritime jurisdiction
 *   - freedom_of_navigation_states: secondary target (powerful/constrained) — bears contested-transit costs to preserve navigational commons
 *   - regional_fishing_communities: diffuse powerless victim (powerless/trapped) — excluded from traditional fishing grounds with no recourse
 *   - international_maritime_tribunals: analytical observer with excluded enforcement — can rule but cannot compel compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.79).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.72).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, snare).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Expansive Construction Reading — Artificial Island Territorial Sea Claims").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, 'f6f68a35-2b09-44a2-9f6d-70c74adbbaf1').
narrative_ontology:cs_kernel_codification('f6f68a35-2b09-44a2-9f6d-70c74adbbaf1', fixed_text).
narrative_ontology:cs_authority_grounding('f6f68a35-2b09-44a2-9f6d-70c74adbbaf1', extraction).
narrative_ontology:cs_interpretation_layer_present('f6f68a35-2b09-44a2-9f6d-70c74adbbaf1').
narrative_ontology:cs_reading_relation('f6f68a35-2b09-44a2-9f6d-70c74adbbaf1', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('f6f68a35-2b09-44a2-9f6d-70c74adbbaf1', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('f6f68a35-2b09-44a2-9f6d-70c74adbbaf1', foundational, effective_control_generates_entitlement).
narrative_ontology:cs_axiom_status(effective_control_generates_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('f6f68a35-2b09-44a2-9f6d-70c74adbbaf1', effective_control_generates_entitlement, conventional).
narrative_ontology:cs_axiom('f6f68a35-2b09-44a2-9f6d-70c74adbbaf1', secondary, administrative_presence_substitutes_for_natural_formation).
narrative_ontology:cs_axiom_status(administrative_presence_substitutes_for_natural_formation, holdable).
narrative_ontology:cs_axiom_grounding('f6f68a35-2b09-44a2-9f6d-70c74adbbaf1', administrative_presence_substitutes_for_natural_formation, instrumental).
narrative_ontology:cs_reference_frame('f6f68a35-2b09-44a2-9f6d-70c74adbbaf1', customary_effective_occupation_doctrine).
narrative_ontology:cs_drift_state('f6f68a35-2b09-44a2-9f6d-70c74adbbaf1', post_2016_pca_south_china_sea_award, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('f6f68a35-2b09-44a2-9f6d-70c74adbbaf1', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, regional_fishing_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, commercial_shipping_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dredges sand and rock onto submerged reefs and low-tide elevations to build permanent structures, then garrisons them, builds runways and administrative facilities, and asserts that effective occupation and administrative control generate territorial sea and exclusive economic zone entitlements identical to natural islands. Controls the physical fact on the ground and the narrative of continuous, uncontested administration used to justify the claim under customary international law's effective-occupation doctrine.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, beneficiary).

% Hold overlapping or adjacent maritime claims that the constructed features now encroach upon. Lack the naval and economic capacity to physically contest the construction or dislodge the administrative presence, and must choose between costly arbitration with no enforcement mechanism, diplomatic protest that goes unanswered, or quiet accommodation that cedes ground by default.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    moderate, biographical, constrained, regional).

% Rely on high-seas and innocent-passage rights through the contested waters for commercial and military transit. Conduct periodic transit operations to contest the expanded territorial sea claim without formally litigating it, bearing escalation risk and diplomatic cost each time, since accepting the claim silently would cede a navigational commons to a unilateral construction fact.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, excluded).

% Have fished the surrounding waters for generations under traditional or nationally recognized access. Once the constructed feature is administratively fortified and patrolled, they are excluded or harassed within the newly asserted territorial sea, with no forum to contest the boundary redefinition and no practical way to relocate their livelihood.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, regional_fishing_communities, payer,
    powerless, biographical, trapped, local).

% Can issue rulings interpreting UNCLOS Article 121 on the legal status of artificial islands and low-tide elevations, but have no enforcement power over a state that declines to appear, rejects jurisdiction, or simply disregards an adverse award. Their interpretive authority is real but structurally excluded from altering the physical and administrative facts already established.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_maritime_tribunals, excluded,
    institutional, generational, analytical, global).

% Route vessels through or near the contested waters and must factor in expanded claimed territorial seas, potential detentions, insurance premium increases, and rerouting costs, none of which they had any voice in creating.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, commercial_shipping_operators, payer,
    organized, immediate, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__expansive_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the constructing state with a stable, defensible administrative presence in a contested maritime zone, ostensibly enabling search-and-rescue, navigational safety infrastructure, and resource management coordination in waters that would otherwise be unpoliced.
% TRANSFER_FUNCTION: Moves navigational rights, fishing access, and resource-extraction jurisdiction from neighboring claimant states, regional fishing communities, and the international navigational commons to the constructing state, converted from a submerged feature with no inherent maritime entitlement into a asserted territorial sea and EEZ base point.
% ABSENT_VOICES: Regional fishing communities have no seat in any bilateral or tribunal process; international maritime tribunals are structurally excluded once a state declines compulsory jurisdiction; smaller claimant states lack the capacity to litigate or physically contest and are effectively priced out of the dispute.
% DISAPPEARANCE_RATIONALE: If the expansive-construction reading were abandoned and courts/states uniformly reverted to the strict geographic reading, the constructed features would revert to having no independent territorial sea or EEZ entitlement, the administrative and naval presence would lose its legal cover, fishing access would reopen to prior claimants, and freedom-of-navigation transits would no longer be contested passages.
% FOUNDING_PROBLEM: States sought a legal basis to secure strategic and resource access in contested maritime regions where natural insular features were scarce, ambiguous, or absent, and where effective presence was seen as the only durable form of control in the absence of settled boundaries.
% FOUNDING_PROBLEM_CORROBORATION: The constructing states themselves attest the problem (regional security and resource access) remains live and justifies continued construction and garrisoning. Independent international law scholars, the Permanent Court of Arbitration's 2016 South China Sea award, and neighboring claimant governments attest that UNCLOS Article 121(3) already resolves the founding problem by denying rocks/artificial features full entitlements — corroboration from outside the beneficiary states holds that this reading is a constructed extraction dressed as legal interpretation, not a genuine gap-filling solution.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply over the interval (0.42 to 0.79) as construction matures from initial land reclamation to fortified, garrisoned administrative presence — each stage hardens the practical claim regardless of its legal merit. Suppression climbs in tandem (0.40 to 0.72) reflecting increasing naval patrol density, exclusion zones, and diplomatic/military pressure applied to deter contestation. Theater ratio is substantial and rising (0.30 to 0.58) because much of the administrative infrastructure (civilian facilities, lighthouses, weather stations) is publicly framed as humanitarian or navigational-safety coordination while its structural function is to manufacture the 'effective occupation' record the legal claim depends on — the coordination story is performance layered over the extraction. Accessibility collapse (0.62) and resistance (0.74) reflect that alternatives (arbitration, joint development, status quo ante) remain nominally available but are practically foreclosed by the fact-on-the-ground strategy, while resistance remains active and vocal from claimant states and FON operators alike — this is not a settled mountain, it is a contested extraction under active dispute.
 *
 * PERSPECTIVAL GAP:
 *   From the constructing state's seat, this reading is straightforward exercise of coastal state sovereignty and administrative capacity-building, structurally indistinguishable from legitimate territorial development. From the payer seats — claimant states, fishing communities, navigation states — the identical structure computes as coercive extraction: unilateral conversion of shared or contested maritime space into exclusive jurisdiction, defended by military presence rather than settled law. The engine's per-seat computation is expected to diverge sharply here precisely because this reading's coordination story (safety, administration, security) is real cover for its beneficiary but experienced as pure extraction by every payer seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Island-constructing states sit at the full-beneficiary end: they control the constructed asset, administer it, and capture the jurisdictional expansion directly — d near 0. Neighboring claimant states and regional fishing communities sit near the full-target end: their pre-existing access and adjacent claims are the substance being converted into the constructing state's jurisdiction, and their exit options (arbitration without enforcement, quiet accommodation) are structurally weak — d near 1, amplified for fishing communities by their powerless/trapped position. Freedom-of-navigation states are targets of a different kind: not losing territory but bearing recurring operational and diplomatic costs to prevent the claim from hardening into accepted customary practice through their own silence — their institutional power provides some mobility but their exit (accepting the claim) is unacceptable, so directionality sits high despite their formal power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (securing presence and resource access in genuinely ambiguous or unpoliced maritime zones) may have been live in an earlier period of weak regional governance, but the 2016 Permanent Court of Arbitration ruling and decades of accumulated UNCLOS jurisprudence have substantially resolved the interpretive ambiguity in favor of the strict/hybrid readings — yet construction, garrisoning, and the expansive claim persist and intensify. This is the classic mandatrophy signature: a claimed coordination function (safety infrastructure, administrative order) persisting and hardening well past the point its founding justification held, sustained now primarily by the sunk cost of construction and the strategic value of the territorial fait accompli rather than by any live coordination need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effective_occupation_vs_article_121,
    'Does customary international law''s effective-occupation doctrine (developed for terra nullius land acquisition) apply at all to submerged maritime features governed by UNCLOS Article 121, or is this reading applying a category of law to a domain it was never designed for?',
    'Comparative doctrinal analysis of whether any recognized tribunal has extended terrestrial effective-occupation doctrine to submerged/low-tide maritime features post-UNCLOS ratification, and whether the 2016 PCA award forecloses this move.',
    'If effective occupation categorically does not transfer to this domain, the expansive reading has no doctrinal foundation independent of raw power projection, and the claimed_type of rope/coordination collapses entirely into snare with no residual coordination defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_occupation_vs_article_121, conceptual, 'Whether the doctrinal borrowing from land-based effective occupation is legally coherent for submerged maritime features.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the expansive_construction_reading a genuine alternative legal interpretation held in good faith, or a strategically selected reading chosen because it is the only one of the three kernel readings that legitimizes an already-underway construction program?',
    'Trace whether the constructing states'' legal justifications were articulated before or after construction began; a reading developed post-hoc to justify a fait accompli is weaker evidence of genuine interpretive commitment than one held independently of the construction program.',
    'If the reading is reverse-engineered from the construction fact rather than independently derived from treaty text, this materially strengthens the snare classification and weakens any rope/coordination framing this reading''s proponents offer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, empirical, 'Whether this reading was selected to fit pre-existing construction activity rather than independently derived.').

omega_variable(
    tribunal_enforcement_gap,
    'Given that international maritime tribunals have no independent enforcement mechanism against a non-appearing or non-compliant state, does the persistence of this reading depend structurally on that enforcement gap, such that closing the gap (e.g., through Security Council-backed enforcement) would collapse the reading regardless of its legal merits?',
    'Model counterfactual scenarios where binding enforcement existed and assess whether construction programs would have proceeded at observed scale.',
    'If persistence is enforcement-gap-dependent rather than merit-dependent, this confirms the reading operates as a snare exploiting an institutional design weakness rather than as a live doctrinal position with independent standing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tribunal_enforcement_gap, conceptual, 'Whether the reading''s persistence depends on the absence of enforceable tribunal authority rather than on the strength of its legal claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(uncl_tr_t4, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement(uncl_tr_t8, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 8, 0.44).
narrative_ontology:measurement(uncl_tr_t12, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(uncl_tr_t16, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 16, 0.55).
narrative_ontology:measurement(uncl_tr_t20, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(uncl_be_t4, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(uncl_be_t8, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(uncl_be_t12, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 12, 0.71).
narrative_ontology:measurement(uncl_be_t16, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 16, 0.76).
narrative_ontology:measurement(uncl_be_t20, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 20, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(uncl_su_t4, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(uncl_su_t8, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(uncl_su_t12, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(uncl_su_t16, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(uncl_su_t20, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__expansive_construction_reading, 0.05).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, strict_geographic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the unclos_maritime_sovereignty kernel, each authored as a separate story per the ε-invariance principle. expansive_construction_reading (this file) authors high ε (0.79) reflecting active, escalating extraction as practiced by constructing states. hybrid_effective_control_reading authors a moderate ε reflecting a conditional, time-dependent maturation of claims. strict_geographic_reading authors near-zero ε reflecting the Article 121(3) plain-text position under which artificial construction confers no entitlement at all — closer to a rope/mountain reading defending the existing treaty text against erosion. The three are linked bidirectionally: expansive_construction_reading's practical success or failure structurally influences whether hybrid_effective_control_reading's maturation-through-time theory gains traction, and both create downstream pressure on whether strict_geographic_reading's plain-text position can hold as customary practice accumulates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
