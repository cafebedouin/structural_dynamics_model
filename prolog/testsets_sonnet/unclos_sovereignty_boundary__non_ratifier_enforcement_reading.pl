% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__non_ratifier_enforcement_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Customary Freedom of Navigation Enforced by Naval Presence (Non-Ratifier Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This story instantiates the non_ratifier_enforcement_reading of the
 *   unclos_sovereignty_boundary kernel: freedom-of-navigation principles are
 *   treated as customary international law binding on all states regardless
 *   of UNCLOS ratification, and this customary status is enforced not through
 *   tribunal adjudication but through naval presence — most visibly by a
 *   major non-ratifying naval power conducting FONOPs through waters that
 *   ratifying coastal states claim as exclusive under UNCLOS Article 57. This
 *   reading structurally decouples navigation rights from the treaty text:
 *   the enforcing power invokes UNCLOS's substantive navigation provisions as
 *   reflecting customary law while declining UNCLOS's dispute-resolution and
 *   other obligations. The sibling readings — strict_eez_reading (treaty text
 *   is exclusive and controlling) and historical_rights_reading (pre-treaty
 *   historical usage overrides EEZ boundaries) — are separate constraints
 *   with their own ε, beneficiary/victim structures, and stakeholder sets;
 *   they are not blended into this one. Where those readings ground
 *   legitimacy in codified boundaries or historical occupation, this reading
 *   grounds legitimacy in a claimed customary norm whose only real
 *   enforcement mechanism is warship presence — which is precisely why naval
 *   powers, not coastal states, occupy the beneficiary seat here.
 *
 * KEY AGENTS:
 *   - major_naval_powers: primary agenda_setter and beneficiary (institutional/arbitrage) — enforces the customary-law reading via naval presence
 *   - coastal_states_asserting_eez_exclusivity: primary payer (moderate/constrained) — ratified UNCLOS, expects treaty text to control, finds naval presence overriding it
 *   - small_littoral_states_without_naval_capacity: secondary payer (powerless/trapped) — cannot generate any counter-presence
 *   - international_law_scholars: analytical observer — assesses whether opinio juris and state practice genuinely establish the customary norm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.62).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.58).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Customary Freedom of Navigation Enforced by Naval Presence (Non-Ratifier Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '02a919a0-1ac9-42f8-837c-cdc554e4a000').
narrative_ontology:cs_kernel_codification('02a919a0-1ac9-42f8-837c-cdc554e4a000', distributed).
narrative_ontology:cs_authority_grounding('02a919a0-1ac9-42f8-837c-cdc554e4a000', extraction).
narrative_ontology:cs_interpretation_layer_present('02a919a0-1ac9-42f8-837c-cdc554e4a000').
narrative_ontology:cs_reading_relation('02a919a0-1ac9-42f8-837c-cdc554e4a000', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('02a919a0-1ac9-42f8-837c-cdc554e4a000', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('02a919a0-1ac9-42f8-837c-cdc554e4a000', foundational, customary_navigation_right_predates_treaty).
narrative_ontology:cs_axiom_status(customary_navigation_right_predates_treaty, holdable).
narrative_ontology:cs_axiom_grounding('02a919a0-1ac9-42f8-837c-cdc554e4a000', customary_navigation_right_predates_treaty, conventional).
narrative_ontology:cs_axiom('02a919a0-1ac9-42f8-837c-cdc554e4a000', secondary, naval_presence_constitutes_valid_state_practice_evidence).
narrative_ontology:cs_axiom_status(naval_presence_constitutes_valid_state_practice_evidence, holdable).
narrative_ontology:cs_axiom_grounding('02a919a0-1ac9-42f8-837c-cdc554e4a000', naval_presence_constitutes_valid_state_practice_evidence, instrumental).
narrative_ontology:cs_reference_frame('02a919a0-1ac9-42f8-837c-cdc554e4a000', pre_unclos_customary_navigation_norm).
narrative_ontology:cs_drift_state('02a919a0-1ac9-42f8-837c-cdc554e4a000', post_unclos_ratification_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('02a919a0-1ac9-42f8-837c-cdc554e4a000', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_shipping_industry).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, non_ratifying_states_with_blue_water_navies).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, small_littoral_states_without_naval_capacity).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, customary_international_law_doctrine).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, high_seas_freedom_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conduct freedom of navigation operations (FONOPs) through waters that coastal states claim as exclusive, asserting that the right to transit predates and stands independent of UNCLOS ratification. They enforce the customary-law reading with warship presence rather than treaty accession, which lets them claim the benefit of the navigation regime while declining the dispute-resolution obligations UNCLOS ratifiers accept. Their capacity to project naval power globally is what makes the 'customary law' claim operationally real rather than merely rhetorical.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers, beneficiary).

% Relies on predictable, low-friction transit through straits and EEZs worldwide. Benefits directly from naval enforcement keeping shipping lanes open regardless of individual coastal states' treaty status, without bearing any of the political or military cost of that enforcement.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_shipping_industry, beneficiary,
    organized, generational, mobile, global).

% Have not ratified UNCLOS but invoke its navigation provisions as reflecting binding customary law when convenient, while treating other UNCLOS provisions (dispute resolution, seabed regimes) as merely treaty-based and non-binding on themselves. This selective invocation is possible only because naval presence, not treaty membership, does the enforcing.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, non_ratifying_states_with_blue_water_navies, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, non_ratifying_states_with_blue_water_navies, agenda_setter).

% Claim exclusive rights over resources and security in their 200-nautical-mile EEZ per UNCLOS Article 57, and object to foreign warship transit or survey activity within those zones as a violation of sovereignty. They ratified UNCLOS and expect its full package of rights and obligations to govern; when a non-ratifier invokes 'customary law' navigation rights enforced by warships, the coastal state's legal victory under the treaty text is overridden by the practical fact of naval presence. Diplomatic protest is their primary remaining tool.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity, payer,
    moderate, generational, constrained, regional).

% Lack any naval or diplomatic leverage to contest transits through waters they consider sovereign. Their formal legal rights under UNCLOS are functionally unenforceable against a state that both declines ratification and possesses the naval capacity to make its reading of customary law the operative one in practice.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, small_littoral_states_without_naval_capacity, payer,
    powerless, biographical, trapped, regional).

% Bodies like ITLOS and UNCLOS Annex VII arbitral tribunals exist to adjudicate exactly these disputes, but a non-ratifying naval power is not bound by their jurisdiction and can simply decline to appear or comply. Their rulings on EEZ boundary disputes are structurally sidelined whenever enforcement is by warship rather than by treaty compliance.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_maritime_tribunals, excluded,
    institutional, generational, analytical, global).

% Debate whether freedom-of-navigation customary law genuinely exists independent of UNCLOS codification (opinio juris plus consistent state practice) or whether the 'customary law' label is being used post hoc to legitimate what is actually naval power projection. Their analysis does not bind any party.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps sea lanes and straits open for global commerce and naval transit by asserting a navigation right that does not depend on any individual state's ratification status, preventing a patchwork of unilateral coastal claims from fragmenting global shipping routes.
% TRANSFER_FUNCTION: Moves de facto control over EEZ transit rights from ratifying coastal states (who accepted UNCLOS's full bargain, including its obligations) to non-ratifying naval powers who can invoke navigation rights while avoiding treaty obligations — enforcement capacity substitutes for treaty membership.
% ABSENT_VOICES: International maritime tribunals with jurisdiction over these disputes are structurally excluded because the enforcing state has not consented to their jurisdiction; small littoral states are absent from the naval calculus entirely because they cannot generate a counter-presence.
% DISAPPEARANCE_RATIONALE: If naval powers stopped conducting freedom-of-navigation operations and instead treated UNCLOS ratification as a precondition for navigation rights, coastal states' EEZ claims would harden into de facto exclusive zones, shipping routes through contested straits would require case-by-case negotiation or coastal-state permission, and the customary-law claim would collapse into an unenforced legal theory.
% FOUNDING_PROBLEM: In the mid-20th century, a wave of unilateral coastal-state claims over expanding maritime zones threatened to fragment freedom of the seas into a checkerboard of national permission regimes, undermining global naval mobility and commercial shipping.
% FOUNDING_PROBLEM_CORROBORATION: Naval powers and shipping industry groups attest the problem remains live — coastal states continue to assert expansive claims that would fragment sea lanes if unchallenged. Independent international law scholars and coastal-state governments counter that UNCLOS itself resolved the founding problem by codifying EEZ boundaries with agreed limits, and that continued non-ratifier enforcement now serves naval power projection rather than the original coordination problem; several UN General Assembly statements from non-aligned coastal states corroborate this outside-the-beneficiary reading.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that the enforcing power captures the substantive benefit of a favorable navigation regime while shedding the correlative treaty obligations ratifiers accepted — a real asymmetry, not merely a difference of legal opinion. Suppression (0.58) is set below extremes because coastal states retain diplomatic and rhetorical avenues (protest notes, UN statements) even though those avenues rarely change outcomes on the water. Theater ratio (0.40) captures that a meaningful share of FONOP activity is symbolic assertion of the customary-law claim itself, performed for legal-record purposes as much as for genuine transit necessity — the operations function partly as ongoing evidence-generation for the customary-law argument. Accessibility collapse is moderate (0.45): coastal states still have the formal UNCLOS Annex VII arbitration path even if it cannot bind the non-ratifier. Resistance is high (0.70) because coastal states actively and continuously contest this reading diplomatically and in international fora.
 *
 * PERSPECTIVAL GAP:
 *   From the naval power's seat, this is customary international law that predates and survives any single treaty, enforced the way custom has always been enforced — through consistent state practice backed by capability. From the coastal ratifier's seat, this is a treaty regime being selectively honored by a state that never joined it, with naval presence substituting for legal argument. The engine computes these as structurally different seat outcomes from the same base data; neither seat's self-description settles the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Major naval powers and non-ratifying blue-water states sit at the beneficiary end: they receive the practical benefit of open transit while incurring none of the treaty-based reciprocal obligations, and their exit options (arbitrage — able to invoke or disclaim treaty status situationally) are the clearest sign of directional asymmetry. Coastal states asserting EEZ exclusivity, especially those with limited naval capacity, sit at the target end: they bear a transfer of practical control despite formal legal entitlement under a treaty they ratified in good faith. The shipping industry is a genuine beneficiary of the coordination function (open lanes) with no offsetting cost, which is why the coordination half of this tangled rope is real and not merely cover.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing fragmentation of the high seas into a patchwork of unilateral coastal permission regimes — remains partially live: unresolved territorial disputes (South China Sea, Arctic passages) show coastal overreach is a real, ongoing risk, not a manufactured pretext. But the mechanism used to solve it (naval-enforced customary law claimed by non-ratifiers) has drifted from a coordination solution into a standing asymmetric enforcement structure that lets powerful non-ratifiers extract the treaty's benefits without its burdens. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (open sea lanes benefit global commerce broadly) while still registering the asymmetric extraction (coastal ratifiers pay a cost non-ratifying enforcers do not bear) that a pure-rope or pure-mountain reading would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_genuineness,
    'Does freedom-of-navigation customary international law genuinely exist as a binding norm independent of UNCLOS (satisfying the traditional opinio juris plus consistent state practice test), or is ''customary law'' a legitimating label applied after the fact to naval power projection that would occur regardless of any legal theory?',
    'Systematic review of state practice and diplomatic statements predating UNCLOS''s 1982 signing and 1994 entry into force, to establish whether the customary norm''s content and acceptance existed independently of the treaty negotiation process, versus being retroactively constructed from treaty-era practice.',
    'If the customary norm is genuinely pre-treaty and independently established, this reading is closer to a legitimate coordination mechanism operating alongside treaty law. If it is a retroactive construction from post-treaty state practice by the same naval powers who benefit from it, the ''customary law'' framing is closer to cover for capability-based extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_genuineness, conceptual, 'Whether the customary-law claim has independent legal genealogy or is a post hoc legitimation of naval capability.').

omega_variable(
    selective_invocation_asymmetry,
    'Is it structurally coherent for a state to invoke UNCLOS''s navigation provisions as customary law binding on others while treating UNCLOS''s dispute-resolution and seabed provisions as merely contractual and non-binding on itself absent ratification?',
    'Comparative analysis of which UNCLOS provisions non-ratifying naval powers treat as customary versus contractual, cross-referenced against which provisions favor versus burden those powers.',
    'A high correlation between ''provisions treated as customary'' and ''provisions that favor the non-ratifier'' would support the tangled_rope/extraction reading; a low correlation (i.e., principled rather than self-serving selectivity) would support treating this as a good-faith legal position rather than asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_invocation_asymmetry, empirical, 'Whether selective invocation of UNCLOS provisions tracks legal principle or self-interest.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading diverge from the sibling strict_eez_reading and historical_rights_reading, structurally?',
    'This is not resolvable by further data — it is the committer-axis structure itself. Documented here per Rule 2: the non_ratifier_enforcement_reading locates legitimacy in enforced custom (naval presence as evidence and enforcement of a binding norm independent of treaty text); strict_eez_reading locates legitimacy in the codified treaty boundary itself, making non-ratifier naval enforcement of contrary claims illegitimate by definition; historical_rights_reading locates legitimacy in pre-existing historical usage/occupation, a source neither treaty text nor naval custom addresses. A sibling reading adopting strict_eez would treat this constraint''s entire beneficiary set (naval powers) as the violating party instead.',
    'Adopting the strict_eez_reading instead of this reading would flip the beneficiary/victim structure entirely: naval powers become violators, coastal ratifiers become the wronged party whose treaty rights are being overridden by extra-legal force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Documents the located structural disagreement between this reading and its kernel siblings, per Rule 2 (committer content routed to omega, not folded into classification).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 1982, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1982, 0.2).
narrative_ontology:measurement(uncl_tr_t1990, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(uncl_tr_t2000, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(uncl_tr_t2010, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2010, 0.34).
narrative_ontology:measurement(uncl_tr_t2018, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(uncl_tr_t2025, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1982, 0.35).
narrative_ontology:measurement(uncl_be_t1990, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(uncl_be_t2000, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(uncl_be_t2010, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(uncl_be_t2018, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(uncl_be_t2025, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1982, 0.4).
narrative_ontology:measurement(uncl_su_t1990, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(uncl_su_t2000, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(uncl_su_t2010, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement(uncl_su_t2018, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2018, 0.56).
narrative_ontology:measurement(uncl_su_t2025, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.12).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'the UNCLOS sovereignty boundary dispute,' per the ε-invariance principle: the label conflates three structurally distinct legitimating claims (codified treaty boundary, enforced custom, historical occupation) that produce different beneficiary/victim sets and different ε values. Each reading is authored as its own constraint story with its own stakeholders and metrics; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
