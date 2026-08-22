% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__strict_geographic_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: UNCLOS Article 121 — Strict Natural-Formation Reading of Island Status
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint instantiates the strict geographic reading of UNCLOS
 *   Article 121: a maritime feature generates territorial sea and an
 *   exclusive economic zone only if it is naturally formed and remains above
 *   water at high tide in its natural state; dredging, land reclamation, or
 *   built structures on a reef, shoal, or low-tide elevation do not confer or
 *   upgrade maritime entitlements no matter how substantial the construction.
 *   This reading was substantially vindicated by the 2016 South China Sea
 *   Arbitration tribunal ruling, which held that none of the constructed
 *   features at issue in the dispute qualified as naturally formed islands.
 *   The rule functions as a coordination device — a predictable, verifiable,
 *   administrable test that lets naval powers, shipping states, and
 *   non-claimant states rely on stable maritime boundaries rather than a
 *   shifting patchwork of engineering-based claims — but its operation
 *   systematically disfavors states pursuing island-building as a strategy to
 *   expand sovereign maritime space, and enforcing it against a determined,
 *   resourced claimant requires sustained diplomatic and naval pressure since
 *   the tribunal itself has no compulsory enforcement mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.28).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.42).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Article 121 — Strict Natural-Formation Reading of Island Status").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, '69d4b0ea-f9ea-4196-b58f-8348f906530b').
narrative_ontology:cs_kernel_codification('69d4b0ea-f9ea-4196-b58f-8348f906530b', formalized).
narrative_ontology:cs_authority_grounding('69d4b0ea-f9ea-4196-b58f-8348f906530b', expertise).
narrative_ontology:cs_interpretation_layer_present('69d4b0ea-f9ea-4196-b58f-8348f906530b').
narrative_ontology:cs_reading_relation('69d4b0ea-f9ea-4196-b58f-8348f906530b', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('69d4b0ea-f9ea-4196-b58f-8348f906530b', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('69d4b0ea-f9ea-4196-b58f-8348f906530b', foundational, construction_cannot_manufacture_entitlement).
narrative_ontology:cs_axiom_status(construction_cannot_manufacture_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('69d4b0ea-f9ea-4196-b58f-8348f906530b', construction_cannot_manufacture_entitlement, conventional).
narrative_ontology:cs_axiom('69d4b0ea-f9ea-4196-b58f-8348f906530b', secondary, natural_baseline_is_the_sole_legally_relevant_fact).
narrative_ontology:cs_axiom_status(natural_baseline_is_the_sole_legally_relevant_fact, holdable).
narrative_ontology:cs_axiom_grounding('69d4b0ea-f9ea-4196-b58f-8348f906530b', natural_baseline_is_the_sole_legally_relevant_fact, conventional).
narrative_ontology:cs_reference_frame('69d4b0ea-f9ea-4196-b58f-8348f906530b', treaty_text_natural_formation_baseline).
narrative_ontology:cs_drift_state('69d4b0ea-f9ea-4196-b58f-8348f906530b', post_south_china_sea_arbitration, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('69d4b0ea-f9ea-4196-b58f-8348f906530b', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_interests).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, island_building_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, small_island_developing_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, fishing_fleets_and_coastal_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate blue-water fleets that depend on maximal high-seas and EEZ transit rights. A narrow reading of what counts as an island-generating feature keeps more water classified as high seas or ordinary EEZ (with navigational freedoms) rather than territorial sea subject to coastal-state control. They invoke this reading in freedom-of-navigation operations and diplomatic protests against reclaimed reefs.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    institutional, generational, arbitrage, global).

% States with no competing sovereignty claim in contested waters benefit from a rule that prevents any single claimant from converting engineering effort into expanded maritime zones, preserving open fishing grounds and seabed access for all parties under ordinary high-seas or EEZ rules.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states, beneficiary,
    moderate, generational, mobile, regional).

% States that have dredged sand onto reefs and low-tide elevations to build permanent installations bear the cost of this reading directly: their constructed features are denied territorial sea, contiguous zone, and EEZ generation regardless of physical presence, size, or civilian population. Their exit is limited to non-recognition of the tribunal's authority or continued construction in defiance of the ruling, both of which carry diplomatic and legal costs.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    powerful, generational, constrained, regional).

% Government agencies and state enterprises that invested capital and years into land reclamation on submerged or low-tide features find the entire strategic rationale for the construction program nullified by this legal reading — the built feature remains, but the maritime entitlement they sought never attaches. There is no exit once the sunk cost is committed; only continued construction or acceptance of the legal loss.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, island_building_claimants, payer,
    powerful, biographical, trapped, regional).

% Low-lying island nations benefit from a rule that ties legal status to natural formation because it protects small genuine islands' EEZ claims from being diluted by rival construction elsewhere, but they are largely absent from the tribunals and negotiations where the rule's boundaries (e.g., rocks vs. islands under 121(3)) are actually litigated by larger powers.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, small_island_developing_states, beneficiary,
    powerless, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__strict_geographic_reading, small_island_developing_states, excluded).

% Bodies such as the Permanent Court of Arbitration apply and interpret Article 121, issuing binding-in-theory rulings (e.g. the South China Sea Arbitration) that determine which features are naturally formed and above water at high tide. They administer the rule but possess no independent enforcement power to compel compliance from claimant states.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_arbitral_tribunals, agenda_setter,
    institutional, civilizational, analytical, global).

% Fishers whose traditional grounds sit within waters that would become another state's EEZ under an expansive reading benefit from the narrow rule preserving open or shared access, but have no seat at the tribunals and experience the rule's effects only through altered patrol patterns and access restrictions on the water.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, fishing_fleets_and_coastal_communities, observer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__strict_geographic_reading, fishing_fleets_and_coastal_communities, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__strict_geographic_reading, diffuse).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__strict_geographic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable, geography-based test for which features generate maritime zones, so that competing coastal states, naval powers, and shipping interests can predict where territorial sea, EEZ, and high seas boundaries lie without a costly case-by-case adjudication of each state's engineering investment or effective-control claims.
% TRANSFER_FUNCTION: Denies expansionist coastal states the maritime zone entitlements (territorial sea, EEZ, resource rights) that would otherwise attach to constructed or elevated features, effectively transferring open-access rights over the surrounding waters and seabed to naval powers, non-claimant states, and the international commons rather than to the constructing state.
% ABSENT_VOICES: Local fishing communities and small island states whose interests are served by the rule rarely appear directly in the arbitral proceedings that define its boundaries; expansionist claimant states participate but frequently reject tribunal jurisdiction outright, leaving their strongest counter-arguments unadjudicated rather than absent from the record.
% DISAPPEARANCE_RATIONALE: If the natural-formation test disappeared and any built-up feature could generate full maritime zones, states with dredging capacity could unilaterally expand territorial sea and EEZ claims across contested regions (most visibly the South China Sea), converting engineering budgets directly into sovereign maritime territory and forcing renegotiation of fishing rights, seabed resource access, and naval transit corridors across multiple regional seas.
% FOUNDING_PROBLEM: UNCLOS Article 121 was drafted to prevent maritime entitlements from being manufactured through artificial means — to close a loophole where a state could build a structure on a reef or shoal and claim the same 200nm EEZ as a genuine, naturally sustaining island, which would let capital and engineering substitute for the natural-formation basis the treaty otherwise uses to allocate ocean space.
% FOUNDING_PROBLEM_CORROBORATION: The 2016 South China Sea Arbitration tribunal (an adjudicative body independent of any claimant or naval-power party) found that none of the reef-based constructions at issue qualified as naturally formed islands generating an EEZ, corroborating that the founding concern — engineered features substituting for natural ones — remains an active, contested problem rather than a settled or obsolete one; the claimant state involved rejected the tribunal's jurisdiction, which is itself evidence the problem is live rather than resolved by consensus.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).
:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 at interval end) because the rule's primary operation is negative — it withholds an entitlement rather than seizing an existing asset — and because the states it burdens (expansionist coastal states) are themselves powerful actors, not the classic powerless victim profile; but it is not zero because billions in state construction investment are rendered strategically void by the rule's application, which is a real transfer of expected value away from the constructing state toward the beneficiaries of a stable, narrow entitlement regime. Suppression rises over the measured interval (0.20 to 0.42) as the reading hardened from a mostly theoretical treaty provision into an actively litigated and diplomatically enforced doctrine following large-scale reclamation activity beginning around 2013-2014 and the 2016 arbitral ruling. Theater ratio stays low (0.20) because the coordination function — a predictable, geography-based test — remains substantively operative; the rule is not primarily performative, though enforcement absent a compulsory mechanism carries some symbolic component (freedom-of-navigation transits, diplomatic statements) alongside genuine legal effect.
 *
 * PERSPECTIVAL GAP:
 *   From the naval-power seat, this reading is straightforwardly a rope: a minimal, verifiable, low-coercion-overhead coordination rule that lets all parties predict maritime boundaries without an arms race in land reclamation. From the island-building claimant's seat, the same rule can appear closer to a tangled_rope or even a snare: coordination cover for what is experienced as a foreign-imposed test designed and enforced by non-claimant naval powers to specifically deny the claimant strategic gains its resources were spent to secure. The engine should be expected to compute these seats differently given the same structural data — the claim (rope) is authored from the naval-power/tribunal vantage, not reconciled to the claimant's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers and non-claimant states are declared beneficiaries: the rule keeps more water classified as high seas or ordinary EEZ, preserving navigational freedom and resource access they would otherwise have to negotiate or contest feature-by-feature. Expansionist coastal states and island-building claimants are declared victims: their investment in construction does not purchase the maritime entitlement they sought, and the rule directly voids the strategic premise of their program. Small island developing states are an unusual beneficiary class — genuinely served by the rule's protection of their own natural islands' entitlements, but structurally excluded from the fora where the rule's edges are actually adjudicated, since arbitration in this domain is dominated by major claimant and naval-power parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing engineered substitution for natural formation in allocating ocean space) remains live and substantially corroborated by an arms-length tribunal, which cuts against treating this as an atrophied or purely inertial rule — it is not a piton. But the rising suppression trajectory alongside a persistently low theater ratio indicates the rule has moved from largely dormant treaty text toward an actively contested, actively enforced doctrine, which is the pattern of a genuine coordination rule meeting real resistance rather than a rule whose function has hollowed out.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_formation_test_verifiability,
    'Can ''naturally formed'' and ''above water at high tide in its natural state'' be verified reliably once a feature has already been extensively built upon, given that pre-construction surveys are often absent, contested, or produced only by the interested claimant state?',
    'Independent satellite and historical hydrographic record review predating the construction activity, cross-referenced by a neutral technical body rather than by claimant-supplied survey data.',
    'If the natural baseline cannot be reliably reconstructed, the strict reading''s administrability advantage over the hybrid and expansive readings weakens, since the test depends on a fact (original natural state) that becomes progressively harder to establish the longer construction has proceeded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_formation_test_verifiability, empirical, 'Whether the naturally-formed baseline is verifiable after extensive construction has already occurred.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the strict geographic reading the correct interpretation of Article 121''s original drafting intent, or is it itself a reading favored disproportionately by states with the naval reach to enforce it, relative to the hybrid effective-control reading which more closely tracks customary state practice on prolonged occupation elsewhere in international law?',
    'Comparative analysis of UNCLOS III negotiating history (travaux préparatoires) against subsequent state practice and opinio juris on effective control doctrines in other territorial contexts (e.g. terra nullius, prescription).',
    'If customary practice on effective control is found to be a stronger general international-law principle than the treaty text''s plain natural-formation language, tribunals could drift toward the hybrid reading over time, which would reduce this constraint''s beneficiary set''s advantage and validate the sibling reading''s claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the strict reading reflects treaty intent or reflects the interpretive preference of the states best positioned to enforce it.').

omega_variable(
    beneficiary_naturalness_ambiguity,
    'Is the strict reading a neutral, naturally-derived legal test, or a constructed rule whose narrow scope was substantially shaped by, and disproportionately serves, the interests of states with pre-existing naval dominance over contested maritime regions?',
    'Historical analysis of which delegations pushed for the strict natural-formation language during UNCLOS III drafting and cross-reference with those states'' contemporaneous naval posture and territorial interests.',
    'If the strict reading was substantially shaped by naval-power drafting influence rather than emerging from a neutral geographic principle, its coordination framing (rope) is weaker than claimed and elements of the arrangement shift toward tangled_rope, since a genuine beneficiary class helped author the rule it now benefits from.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_naturalness_ambiguity, conceptual, 'Whether the strict-reading rule is a neutral natural-law-like test or a constructed rule authored partly by its own beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1994, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1994, 0.1).
narrative_ontology:measurement(uncl_tr_t2000, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(uncl_tr_t2008, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2008, 0.14).
narrative_ontology:measurement(uncl_tr_t2014, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2014, 0.17).
narrative_ontology:measurement(uncl_tr_t2016, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2016, 0.19).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1994, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1994, 0.15).
narrative_ontology:measurement(uncl_be_t2000, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2000, 0.17).
narrative_ontology:measurement(uncl_be_t2008, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2008, 0.2).
narrative_ontology:measurement(uncl_be_t2014, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2014, 0.25).
narrative_ontology:measurement(uncl_be_t2016, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2016, 0.27).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1994, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1994, 0.2).
narrative_ontology:measurement(uncl_su_t2000, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2000, 0.24).
narrative_ontology:measurement(uncl_su_t2008, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2008, 0.3).
narrative_ontology:measurement(uncl_su_t2014, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2014, 0.38).
narrative_ontology:measurement(uncl_su_t2016, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2016, 0.42).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the unclos_maritime_sovereignty kernel decomposition. Each reading is authored as a separate constraint with its own ε, beneficiary/victim structure, and claimed type, per the ε-invariance principle: the natural-language label 'the Article 121 island rule' conflates structurally distinct legal claims about what artificial construction does to maritime entitlement. strict_geographic_reading (this story) authors low-moderate ε and a rope claim, reflecting the naval-power/tribunal vantage that the rule is minimal-coercion coordination; expansive_construction_reading and hybrid_effective_control_reading are expected to author higher or differently-structured ε reflecting the claimant-state vantage that effective control should generate entitlement. The three are linked bidirectionally via affects_constraints to preserve the network's family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
