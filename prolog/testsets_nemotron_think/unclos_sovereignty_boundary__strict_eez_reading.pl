% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__strict_eez_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: UNCLOS Article 57 EEZ Exclusivity — Strict Reading
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   The strict EEZ reading asserts that UNCLOS Article 57 creates exclusive,
 *   enforceable 200-nautical-mile economic zones with no valid overlay
 *   claims. Coastal states are structural beneficiaries gaining exclusive
 *   resource control; overlapping claimants (China in South China Sea, Arctic
 *   coastal states, Mediterranean states) and non-ratifiers (USA, Turkey) are
 *   structural victims losing access or constrained by a regime they did not
 *   fully consent to. The constraint coordinates by replacing chaotic claims
 *   with a universal standard, but extracts by transferring resource rents
 *   from the global commons to coastal states. Active enforcement via navies,
 *   coast guards, and tribunals sustains the boundary regime against
 *   persistent challenges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.55).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.75).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "UNCLOS Article 57 EEZ Exclusivity — Strict Reading").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, 'e1481afd-a1de-48d6-949e-5bdf5e39579e').
narrative_ontology:cs_kernel_codification('e1481afd-a1de-48d6-949e-5bdf5e39579e', formalized).
narrative_ontology:cs_authority_grounding('e1481afd-a1de-48d6-949e-5bdf5e39579e', lineage).
narrative_ontology:cs_interpretation_layer_present('e1481afd-a1de-48d6-949e-5bdf5e39579e').
narrative_ontology:cs_reading_relation('e1481afd-a1de-48d6-949e-5bdf5e39579e', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('e1481afd-a1de-48d6-949e-5bdf5e39579e', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('e1481afd-a1de-48d6-949e-5bdf5e39579e', foundational, eez_exclusivity_absolute).
narrative_ontology:cs_axiom_status(eez_exclusivity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('e1481afd-a1de-48d6-949e-5bdf5e39579e', eez_exclusivity_absolute, conventional).
narrative_ontology:cs_axiom('e1481afd-a1de-48d6-949e-5bdf5e39579e', foundational, unclos_article_57_self_executing).
narrative_ontology:cs_axiom_status(unclos_article_57_self_executing, holdable).
narrative_ontology:cs_axiom_grounding('e1481afd-a1de-48d6-949e-5bdf5e39579e', unclos_article_57_self_executing, conventional).
narrative_ontology:cs_reference_frame('e1481afd-a1de-48d6-949e-5bdf5e39579e', unclos_eez_exclusivity_framework).
narrative_ontology:cs_drift_state('e1481afd-a1de-48d6-949e-5bdf5e39579e', contemporary_maritime_disputes, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e1481afd-a1de-48d6-949e-5bdf5e39579e', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimants).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_fleets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, commercial_shipping).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__strict_eez_reading, unclos_article_57_exclusivity).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__strict_eez_reading, maritime_boundary_certainty).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__strict_eez_reading, coastal_state_resource_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim and enforce exclusive resource rights within 200nm EEZ per UNCLOS Article 57; administer licensing for fisheries, seabed mining, and energy development; enforce against incursions by coast guard and navy; benefit directly from resource revenues and licensing fees.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, coastal_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold historical or geographical claims overlapping others' EEZs (e.g., South China Sea, Arctic, Mediterranean); lose access to resources in disputed areas; must choose between diplomatic negotiation, legal adjudication, or force projection to maintain claims; bear costs of displaced fishing, foregone energy development, and military presence.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimants, payer,
    powerful, biographical, constrained, regional).

% Have not ratified UNCLOS (e.g., USA, Turkey, Venezuela) but operate under its customary law provisions; conduct freedom of navigation operations challenging excessive maritime claims; extract navigation rights without accepting EEZ resource exclusivity as treaty obligation; bear diplomatic costs of non-participation in treaty bodies.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_states, payer,
    powerful, biographical, mobile, global).

% Adjudicate maritime boundary disputes under UNCLOS Annex VII arbitration, ITLOS, and ICJ; interpret Article 57 and delimitation principles; their rulings shape the constraint's enforcement trajectory but they do not directly collect revenues or pay costs.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, international_tribunals, observer,
    institutional, generational, analytical, global).

% Benefit from clear navigation rights and stable maritime boundaries under EEZ regime; rely on UNCLOS for predictable transit passage and innocent passage rights; pay no direct tribute but depend on the legal order the constraint creates for commercial certainty.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, commercial_shipping, beneficiary,
    organized, biographical, mobile, global).

% Lose access to high seas fishing grounds converted to coastal state EEZs after 1982; must negotiate access agreements paying licensing fees or fish illegally; bear costs of coastal state enforcement, vessel monitoring, and reduced catch access; primarily flagged to China, EU, Taiwan, Japan, Korea.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, distant_water_fishing_fleets, payer,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear maritime boundaries (200nm EEZ) to replace ambiguous historical claims, reducing interstate conflict over ocean resources and enabling orderly resource development under a universal legal framework.
% TRANSFER_FUNCTION: Moves exclusive resource rights (fisheries, seabed minerals, energy) within 200nm from the global commons/open access to coastal states; overlapping claimants and distant water users lose access unless they negotiate access agreements on coastal states' terms.
% ABSENT_VOICES: Indigenous coastal communities with traditional maritime territories not recognized by state-centric EEZ regime; landlocked states with no EEZ access (37 UN members); environmental advocates arguing EEZ exclusivity enables overexploitation by coastal states without high seas conservation obligations.
% DISAPPEARANCE_RATIONALE: If EEZ exclusivity vanished overnight, coastal states would lose exclusive resource rights worth trillions annually, overlapping claims would erupt into open competition, distant water fleets would return to formerly exclusive zones, and the legal order governing 40% of ocean surface would collapse into customary law uncertainty.
% FOUNDING_PROBLEM: Pre-UNCLOS ocean governance was chaotic: competing claims up to 200nm, no universal standard for resource jurisdiction, frequent conflicts over fisheries and seabed resources, no binding dispute resolution mechanism.
% FOUNDING_PROBLEM_CORROBORATION: UNCLOS negotiating history (1973-1982) documents the founding problem from multiple state perspectives; the 1994 Implementation Agreement for Part XI addresses developing state concerns about seabed mining equity, corroborating that the founding problem was recognized as contested even at adoption; overlapping claimants today argue UNCLOS created new inequities by freezing historical claims.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__strict_eez_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects the resource transfer from global commons to coastal states — significant but not total since coastal states bear management costs. Suppression (0.75) is high because the regime's persistence depends on active enforcement: naval patrols, coast guard interceptions, arbitration proceedings, and diplomatic pressure against alternative claims. Theater ratio (0.2) is low because enforcement is substantively functional, not performative — tribunals issue binding rulings, navies physically interdict. Accessibility collapse (0.8) is high for overlapping claimants: once EEZ lines are drawn, alternative sovereignty frameworks (historical rights, sectoral claims) are structurally excluded. Resistance (0.7) is high: overlapping claimants actively resist through island-building, gray-zone operations, and legal counter-arguments.
 *
 * PERSPECTIVAL GAP:
 *   From the coastal state seat, the constraint appears as genuine coordination: a universal standard replacing chaos, with enforcement protecting their sovereign rights. From the overlapping claimant seat, the same structure operates as enforced extraction: their historical claims are suppressed, resources are transferred to neighbors, and resistance is met with superior force. The engine computes this seat divergence from the structural data — the claimed_type (tangled_rope) captures the hybrid nature, but the per-seat experience ranges from rope-like (coastal states, shipping) to snare-like (overlapping claimants, distant water fleets).
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal states sit at d≈0.1 (strong beneficiaries: they collect resource rents, set licensing terms, control enforcement). Overlapping claimants sit at d≈0.85 (targets: they lose access, bear enforcement costs, constrained exit via negotiation or force). Non-ratifiers sit at d≈0.6 (partial targets: bound by customary law but retain naval freedom of action — mobile exit). Distant water fleets sit at d≈0.75 (targets: pay licensing fees, constrained by coastal state enforcement). Commercial shipping sits at d≈0.4 (near-symmetric: genuine coordination benefit, minimal direct cost). Tribunals sit at d=0.5 (analytical observer). The engine computes per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (chaotic pre-UNCLOS claims) is contested as live: coastal states argue resource competition and new technologies (deep seabed mining, genetic resources) make EEZ exclusivity more necessary than ever; overlapping claimants argue UNCLOS froze inequitable boundaries and the mandate has atrophied into a tool for coastal state rent-seeking. The constraint is not a piton — enforcement is intensifying (suppression rising), not decaying — but the coordination-extraction balance is contested. Mandatrophy is unresolved: the coordination function persists but the extraction asymmetry grows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_strict_eez,
    'How does this reading''s classification change if the kernel framing (unclos_sovereignty_boundary) is contested rather than accepted?',
    'Compare classification outputs across all three declared readings of the kernel; if classifications diverge, the kernel itself is the contested structure, not any single reading.',
    'If sibling readings produce different constraint types (e.g., historical_rights_reading computes as snare for coastal states), the kernel is a constraint family requiring cross-reading contamination analysis, not a single constraint with measurement variance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_strict_eez, conceptual, 'This constraint is one reading of a contested kernel; classification may be kernel-relative.').

omega_variable(
    historical_rights_ambiguity,
    'Can historical usage rights coexist with UNCLOS EEZ exclusivity, or does the strict reading logically foreclose them?',
    'Track tribunal jurisprudence: if ITLOS/ICJ recognize historical rights as carve-outs within EEZs (e.g., South China Sea Award para 278), the strict reading''s foreclosure claim weakens; if tribunals consistently reject historical rights within EEZs, foreclosure holds.',
    'If historical rights are recognized as compatible, the strict reading''s extraction decreases (coastal state exclusivity is qualified) and overlapping claimants'' victim status is mitigated; if foreclosure holds, extraction and suppression remain at authored levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_rights_ambiguity, empirical, 'Whether historical rights constitute a structural exception to EEZ exclusivity or are fully suppressed.').

omega_variable(
    non_ratifier_customary_binding,
    'Are non-ratifier states bound by EEZ resource exclusivity as customary international law, or only by navigation provisions?',
    'Analyze state practice and opinio juris: if non-ratifiers (USA) consistently respect other states'' EEZ resource claims while asserting navigation rights, customary binding is partial; if they challenge resource exclusivity, the strict reading''s victim set shrinks.',
    'If non-ratifiers are bound by resource exclusivity as custom, they remain victims; if only navigation provisions are customary, non-ratifiers extract navigation benefits without paying resource costs — altering their directionality and the constraint''s net extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_ratifier_customary_binding, empirical, 'Whether EEZ resource exclusivity binds non-parties as customary law.').

omega_variable(
    enforcement_effectiveness_vs_claims,
    'Does active enforcement actually suppress alternative sovereignty frameworks, or do overlapping claims persist de facto despite de jure suppression?',
    'Measure de facto control in disputed zones: if overlapping claimants effectively control resources despite EEZ lines (e.g., Chinese fishing in Philippine EEZ, Turkish drilling in Cypriot EEZ), suppression is lower than authored; if coastal states effectively exclude, suppression holds.',
    'If suppression is de facto lower, the constraint''s effective extraction for overlapping claimants decreases and resistance metrics should be re-evaluated; the constraint may operate as a weaker tangled_rope or approach rope for some seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_effectiveness_vs_claims, empirical, 'Gap between de jure suppression (legal regime) and de facto control (on-water reality).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1994, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1994, 0.15).
narrative_ontology:measurement(uncl_tr_t2000, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(uncl_tr_t2006, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2006, 0.19).
narrative_ontology:measurement(uncl_tr_t2012, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2012, 0.2).
narrative_ontology:measurement(uncl_tr_t2018, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1994, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1994, 0.35).
narrative_ontology:measurement(uncl_be_t2000, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(uncl_be_t2006, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2006, 0.48).
narrative_ontology:measurement(uncl_be_t2012, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2012, 0.52).
narrative_ontology:measurement(uncl_be_t2018, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2018, 0.54).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1994, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1994, 0.55).
narrative_ontology:measurement(uncl_su_t2000, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(uncl_su_t2006, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2006, 0.65).
narrative_ontology:measurement(uncl_su_t2012, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2012, 0.7).
narrative_ontology:measurement(uncl_su_t2018, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2018, 0.73).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__strict_eez_reading, 0.12).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, freedom_of_navigation_regime).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, seabed_mining_regime_part_xi).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, regional_fisheries_management_organizations).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, arctic_ocean_governance).

% DUAL FORMULATION NOTE:
% This constraint is the strict_eez_reading of the unclos_sovereignty_boundary kernel. The historical_rights_reading and non_ratifier_enforcement_reading are sibling constraints with different ε values and beneficiary/victim structures. The kernel decomposition follows the BGS pattern: a single colloquial label ('UNCLOS EEZ') covers structurally distinct claims with different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
