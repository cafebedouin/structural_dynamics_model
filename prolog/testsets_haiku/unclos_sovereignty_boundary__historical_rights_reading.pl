% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical Rights Override of UNCLOS EEZ Provisions
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   In international maritime law, UNCLOS Article 57 defines the Exclusive
 *   Economic Zone as extending 200 nautical miles from the baseline of
 *   coastal states, conferring exclusive sovereign rights over resource
 *   exploitation and economic activity. However, some states—particularly in
 *   the South China Sea, the Mediterranean, and the Arctic—assert that
 *   historical occupation, long-term usage, and pre-UNCLOS customary
 *   practices create sovereign or quasi-sovereign rights that override or
 *   coexist with UNCLOS boundaries. This constraint story models the
 *   historical-rights reading: the claim that ancient or colonial-era
 *   occupation and continuous exercise of maritime control establish
 *   legitimate interests that predate and supersede the 1982 treaty
 *   framework. This reading benefits states with contested historical claims
 *   (China, Vietnam, Philippines claimants; Russia in Arctic regions) and
 *   extracts from states that depend on UNCLOS-defined EEZ certainty. The
 *   constraint is claimed as tangled_rope (coordinating historical customary
 *   law with modern treaty obligations while extracting navigational control
 *   and resource rights); the authored metrics describe substantial
 *   extraction (0.68), rising suppression requirement (0.76), and rising
 *   theater ratio (0.42) as enforcement machinery—naval patrols,
 *   administrative claims, diplomatic assertions—does not recede over the
 *   interval but grows. The claim/metric gap is intentional: the
 *   historical-rights reading frames itself as coordination (preserving
 *   legitimate historical interests), but the structure operates as enforced
 *   extraction (overriding treaty boundaries through superior
 *   military/administrative presence). This is exactly the contested framing
 *   the engine measures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.68).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.76).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Rights Override of UNCLOS EEZ Provisions").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, 'b4e8f005-6b82-4720-8551-91e3c6369fcf').
narrative_ontology:cs_kernel_codification('b4e8f005-6b82-4720-8551-91e3c6369fcf', fixed_text).
narrative_ontology:cs_authority_grounding('b4e8f005-6b82-4720-8551-91e3c6369fcf', extraction).
narrative_ontology:cs_interpretation_layer_present('b4e8f005-6b82-4720-8551-91e3c6369fcf').
narrative_ontology:cs_reading_relation('b4e8f005-6b82-4720-8551-91e3c6369fcf', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('b4e8f005-6b82-4720-8551-91e3c6369fcf', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, influences).
narrative_ontology:cs_axiom('b4e8f005-6b82-4720-8551-91e3c6369fcf', foundational, pre_unclos_customary_supremacy).
narrative_ontology:cs_axiom_status(pre_unclos_customary_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b4e8f005-6b82-4720-8551-91e3c6369fcf', pre_unclos_customary_supremacy, empirically_contingent).
narrative_ontology:cs_axiom('b4e8f005-6b82-4720-8551-91e3c6369fcf', foundational, historical_occupation_creates_sovereign_right).
narrative_ontology:cs_axiom_status(historical_occupation_creates_sovereign_right, holdable).
narrative_ontology:cs_axiom_grounding('b4e8f005-6b82-4720-8551-91e3c6369fcf', historical_occupation_creates_sovereign_right, deontological).
narrative_ontology:cs_reference_frame('b4e8f005-6b82-4720-8551-91e3c6369fcf', pre_unclos_maritime_occupation_doctrine).
narrative_ontology:cs_drift_state('b4e8f005-6b82-4720-8551-91e3c6369fcf', contemporary_post_2010_enforcement_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b4e8f005-6b82-4720-8551-91e3c6369fcf', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, regional_hegemonic_powers).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, strict_eez_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, non_claimant_navigational_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, military_hegemonic_powers).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, military_hegemonic_powers).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, historical_possession_doctrine).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, pre_unclos_customary_law_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with long histories of occupation, settlement, or de facto control over maritime zones beyond or overlapping UNCLOS EEZ boundaries (e.g., South China Sea claimants, Mediterranean regional powers). They assert that historical usage—fishing, navigation, military patrols, colonial inheritance—creates sovereign or quasi-sovereign rights predating the 1982 UNCLOS convention. They set enforcement rules through naval presence, administrative claims, and diplomatic assertion. They benefit directly by maintaining expanded exclusive zones and controlling resource extraction.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, agenda_setter,
    institutional, generational, arbitrage, regional).

% States that ratified UNCLOS and claim only the 200-nautical-mile EEZ, expecting that boundary to be exclusive and enforceable. They bear the cost of overlapping historical claims through reduced resource control, challenged maritime sovereignty, and military confrontation risk when defending their UNCLOS-recognized zones. Their exit (withdrawing from UNCLOS) is costly and carries diplomatic isolation; accepting the historical-rights reading erodes their legal certainty.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, strict_eez_coastal_states, payer,
    institutional, generational, constrained, regional).

% Commercial shipping, fishing fleets, research vessels, and non-aligned naval forces that depend on freedom of navigation beyond 12 nautical miles per UNCLOS Article 87. Historical-rights claims increase the zones where their passage, fishing, or operations are challenged or forbidden. They must navigate around expanding claimed zones or negotiate passage—a diffuse cost imposed without a benefiting return.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, non_claimant_navigational_actors, payer,
    moderate, biographical, constrained, global).

% UN agencies (IMO, UNCLOS dispute bodies), regional organizations, and international courts tasked with interpreting maritime law. They observe competing readings of UNCLOS and customary law, mediate disputes, and have limited enforcement power. They see the constraint as a test of whether UNCLOS' written text governs or whether it is subordinate to asserted historical rights.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_maritime_governance_bodies, observer,
    institutional, generational, analytical, global).

% Major naval powers (some ratifying UNCLOS, some not) that assert historical rights to navigate, project power, and defend military interests globally. They benefit from the indeterminacy of historical-rights claims by using them opportunistically—accepting them when convenient, rejecting them when asserting their own freedom of navigation. They also pay by facing reciprocal historical claims from regional powers.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, military_hegemonic_powers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__historical_rights_reading, military_hegemonic_powers, payer).

% Small island nations and coastal states without deep-water naval forces that lack standing to contest expansive historical claims. They would benefit from a clear UNCLOS EEZ boundary but are structurally unable to enforce it or contest claimant-state assertions. They are excluded from the negotiation over what historical rights mean.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, excluded_smaller_maritime_states, excluded,
    powerless, biographical, trapped, regional).

% Legal scholars, policy analysts, and diplomatic observers who assess the constraint's operation and legitimacy. They document the gap between UNCLOS text and historical-rights assertions, track military incidents, and analyze whether the constraint is coordinating genuine customary-law principles or functioning as pure maritime extraction.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, academic_and_diplomatic_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__historical_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Harmonizes overlapping maritime claims and de facto occupation with international law by recognizing that historical possession and long-term usage patterns create legitimate interests that written treaty boundaries cannot fully erase. Provides a mechanism for regional powers to assert continuity with pre-UNCLOS maritime practice.
% TRANSFER_FUNCTION: Transfers exclusive resource-control rights and navigational authority from UNCLOS-recognizing coastal states and non-aligned maritime actors to expansive claimant states. The constraint moves: fishing rights, seabed resource claims, hydrocarbon extraction zones, military dominance, and navigational exclusivity from the victim set to the beneficiary set.
% ABSENT_VOICES: Smaller maritime states, non-state fishing communities, and future generations dependent on open ocean access have no seat at the table. Academic challengers to historical-rights doctrine exist but lack enforcement machinery. Indigenous communities with pre-colonial maritime heritage in some regions are enrolled selectively (when convenient to claimant states) but not systematically recognized as rights-bearing entities.
% DISAPPEARANCE_RATIONALE: If historical-rights claims were invalidated overnight and UNCLOS Article 57 200-nautical-mile EEZ boundaries were globally enforced, maritime resource allocation would shift dramatically: disputed zones would revert to established coastal-state control, non-claimant navigators would face lower passage barriers, hydrocarbon claims would be redrawn, and military positioning in contested waters would be constrained by clearer legal boundaries. Regional power distributions would shift and decades of administrative claims would be unmade.
% FOUNDING_PROBLEM: UNCLOS (1982) created a new maritime boundary regime that did not address pre-existing occupational claims, fishing grounds, and colonial-era maritime practices. States with historical stakes in regions now claimed by others lacked a mechanism to preserve those interests within the treaty framework, risking the wholesale erasure of long-established maritime presence and resource use.
% FOUNDING_PROBLEM_CORROBORATION: Expansive claimant states attest the founding problem remains live—historical practices and customary law must be honored. UNCLOS-strict coastal states and navigational actors attest the problem was solved by the treaty itself and that historical claims are revisionism undermining legal order. Academic historians and international law scholars outside the claimant-beneficiary set document that historical occupation in many disputed zones is either contested, fragmentary, or began decades after UNCLOS signature, suggesting the founding-problem framing overstates the legitimacy of asserted historical rights.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).

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
 *   Extractiveness rises from 0.35 (1982, treaty signature) to 0.68 (2024) because claimant states have progressively asserted administrative control, resource claims, and military enforcement over zones UNCLOS assigned to other states or to commons. The rising trajectory reflects increasing enforcement (more naval assets devoted, more administrative claims lodged, more fishing-ground closures) despite UNCLOS text being static. Suppression rises faster (0.50→0.76) than extractiveness (0.35→0.68) because the constraint's persistence depends on actively preventing alternative navigation and resource access—enforcement must suppress alternatives that UNCLOS would permit. Theater ratio (0.42) is moderate because the constraint combines real historical narratives (long-term fishing practices, settlement heritage) with opportunistic claim-expansion; not purely performative, but performance has grown (diplomatic theater, military posturing) as the empirical historical bases have been questioned. Accessibility collapse (0.72) is high because once the historical-rights reading is accepted, navigators and non-claimant coastal states face nearly-complete closure of alternatives: they cannot navigate these zones freely, cannot access resources, and cannot appeal to UNCLOS without directly challenging the claimant's historical narrative (which itself is suppressed from scrutiny). Resistance (0.64) is substantial: UNCLOS-strict coastal states, navigational actors, and smaller maritime nations resist the reading through legal arguments, dispute mechanisms, and sometimes military presence (U.S. freedom-of-navigation operations). The one shared time grid ensures every metric is authored at every time point examined.
 *
 * PERSPECTIVAL GAP:
 *   From the expansive-claimant-state seat, this is genuine coordination: recognizing long-standing occupation and maritime practice as sources of legitimate rights. The constraint is natural law (customary practice creating obligation). From the UNCLOS-strict coastal state and navigational actor seats, the same structure is enforced extraction: powerful states overriding a written treaty through military presence and administrative fait accompli. The constraint is constructed, arbitrary, and suppressed from scrutiny. The engine should compute different type classifications per seat: the agenda-setter may compute as rope (coordination it built), while payer seats compute as snare (extraction without choice). This divergence is the measurement the corpus is designed to capture—the same constraint looks radically different depending on where one sits.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansive claimant states (institutional power) are the agenda-setters and beneficiaries: they set the rules through military and administrative presence, collect resource-extraction rights and navigational control, and have exit options via arbitrage (accepting UNCLOS-strict reading when it favors them, reasserting historical claims when it does not). They sit near d=0.0 (full beneficiary). Strict EEZ coastal states (institutional power) are payers: they lose exclusive control of their UNCLOS zones, face resource competition, and must tolerate intrusions. They have high d (near 0.9), approaching target status. Non-claimant navigational actors (moderate power) are payers with constrained exit: they cannot navigate contested zones freely and have limited recourse. They sit around d=0.85. Military hegemonic powers are dual-positioned (d≈0.5): they benefit from indeterminacy by using historical claims opportunistically, but they also pay by facing reciprocal claims in their own maritime zones. Smaller maritime states (powerless) are excluded and trapped; they would benefit from UNCLOS clarity but have no standing to challenge claimants. No directionality override is needed; the structural derivation chain captures the asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving historical maritime rights in a post-UNCLOS world) was genuine and live in 1982—states with pre-treaty stakes needed a mechanism. But the founding-problem status has shifted to contested/dead: by 2024, many asserted historical claims are either fragmentary, interrupted by post-UNCLOS practice, or manufactured ex post facto to justify territorial expansion. The constraint persists not because the founding problem requires it, but because claimant states continue to benefit from the indeterminacy and military leverage it provides. This is a mandatrophy signature: the constraint's original coordination function (accommodating legitimate historical stakes) has atrophied, but the extraction machinery persists and has hardened. The suppression requirement rising faster than extractiveness (Δ0.26 vs. Δ0.33) indicates enforcement hardening—more resources devoted to suppressing alternatives (freedom-of-navigation transit, non-claimant resource access) even as the legitimacy basis erodes. The theater ratio (0.42) is the secondary symptom: increasing shares of enforcement activity are devoted to maintaining the historical-narrative framing (diplomatic assertion, academic articles, legal arguments) rather than defending a real coordination mechanism. A true coordination function would not require rising theatrical maintenance as the empirical historical bases are questioned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_occupation_authenticity,
    'Are the asserted historical occupations and maritime practices genuine, continuous, and predating UNCLOS by a meaningful margin, or are they fragmentary, interrupted, or retrospectively constructed to justify territorial expansion?',
    'Independent historical analysis of archival records, archaeological evidence, and maritime charts from the pre-UNCLOS period. International court or tribunal examination of historical documentation offered by claimant states.',
    'If genuinely continuous and pre-1982, the historical-rights claim has stronger customary-law grounding and the constraint''s classification may shift toward tangled_rope with stronger coordination justification. If fragmentary or post-1982, the claim is revealed as opportunistic expansion and the constraint reclassifies toward snare (pure extraction with cover narrative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_occupation_authenticity, empirical, 'Whether historical claims rest on authentic long-standing occupation or manufactured retrospective narratives.').

omega_variable(
    customary_law_vs_treaty_hierarchy,
    'Does customary international law—as opposed to UNCLOS—genuinely establish a hierarchy where pre-treaty occupation supersedes post-treaty written boundaries, or is this a reading imposed by powerful states for their benefit?',
    'Analysis by non-claimant legal scholars of customary law doctrine across international law treatises and precedent. Examination of how customary law has been invoked in other maritime disputes and whether it is applied consistently across all parties.',
    'If customary law genuinely establishes pre-treaty supremacy, the historical-rights reading is a legitimate (if contested) legal position. If customary law is selectively invoked by powerful states while denied to weaker parties, the constraint operates as pure extraction masked by legal rhetoric—reclassification to snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_vs_treaty_hierarchy, conceptual, 'Whether the legal hierarchy invoked (customary law > written treaty) is a genuine principle of international law or an imposed standard serving claimant interests.').

omega_variable(
    suppression_mechanism_structural_or_internalized,
    'Is the non-claimant states'' acceptance of historical-rights claims the result of structural suppression (military power, economic leverage) or internalized acceptance of the historical-rights legal framing?',
    'Post-suppression trajectory: if suppression recedes (military pressure lowers, economic sanctions ease), do the weaker states continue accepting the historical-rights reading, or do they revert to UNCLOS-strict positions? Examination of private diplomatic discourse versus public positions.',
    'If structural, the constraint is enforced extraction sustained by external force. If internalized, the reading has achieved some legitimacy even among those harmed by it—the suppression mechanism is stronger and more durable. Mixed mechanism suggests a constraint in transition from pure force to partial norm acceptance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_or_internalized, empirical, 'Whether suppression of alternatives is structural (external barriers) or internalized (targets believe the reading is legitimate).').

omega_variable(
    kernel_reading_underspecification,
    'This constraint is one reading of the unclos_sovereignty_boundary kernel. Does this reading''s foundational axiom (pre_unclos_customary_supremacy) necessarily exclude a coexistence position where historical rights complement rather than override UNCLOS, or only exclude strict UNCLOS exclusivity?',
    'Logical analysis of the foundational axiom: does it entail foreclosure of coexistence, or only opposition to strict-UNCLOS reading? Examination of whether any state legal position genuinely holds coexistence while also endorsing pre-treaty customary supremacy.',
    'If a true coexistence position exists but this reading forecloses it, the binary choice (historical-rights vs. strict-UNCLOS) is artificial. If foreclosure is genuine (supremacy entails override), the reading''s structure is justified and the strict_eez_reading truly forecloses this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underspecification, conceptual, 'Whether the foundational axiom of this reading necessarily forecloses a coexistence position between historical rights and UNCLOS.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1982, 0.25).
narrative_ontology:measurement_basis(uncl_tr_t1982, observed).
narrative_ontology:measurement(uncl_tr_t1995, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement_basis(uncl_tr_t1995, observed).
narrative_ontology:measurement(uncl_tr_t2005, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement_basis(uncl_tr_t2005, observed).
narrative_ontology:measurement(uncl_tr_t2015, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2015, 0.39).
narrative_ontology:measurement_basis(uncl_tr_t2015, observed).
narrative_ontology:measurement(uncl_tr_t2020, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement_basis(uncl_tr_t2020, observed).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(uncl_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1982, 0.35).
narrative_ontology:measurement_basis(uncl_be_t1982, observed).
narrative_ontology:measurement(uncl_be_t1995, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement_basis(uncl_be_t1995, observed).
narrative_ontology:measurement(uncl_be_t2005, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement_basis(uncl_be_t2005, observed).
narrative_ontology:measurement(uncl_be_t2015, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement_basis(uncl_be_t2015, observed).
narrative_ontology:measurement(uncl_be_t2020, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(uncl_be_t2020, observed).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(uncl_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement_basis(uncl_su_t1982, observed).
narrative_ontology:measurement(uncl_su_t1995, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement_basis(uncl_su_t1995, observed).
narrative_ontology:measurement(uncl_su_t2005, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement_basis(uncl_su_t2005, observed).
narrative_ontology:measurement(uncl_su_t2015, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement_basis(uncl_su_t2015, observed).
narrative_ontology:measurement(uncl_su_t2020, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2020, 0.74).
narrative_ontology:measurement_basis(uncl_su_t2020, observed).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2024, 0.76).
narrative_ontology:measurement_basis(uncl_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__historical_rights_reading, 0.12).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, arctic_continental_shelf_extended_claims).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, south_china_sea_multi_claimant_overlap).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the unclos_sovereignty_boundary kernel. The strict_eez_reading models UNCLOS Article 57 as exclusive and dispositive; the non_ratifier_enforcement_reading models freedom-of-navigation as customary law independent of UNCLOS ratification. This reading (historical_rights_reading) asserts that pre-1982 occupation and usage create sovereign rights that override both the strict EEZ and the non-ratifier reading's freedom-of-navigation principle. The three readings coexist as competing legal positions across different state coalitions. Each reading has its own ε, beneficiary/victim structure, and claim—they are not perspectives on a single constraint, but structurally distinct claims about the same kernel. All three should be compiled and linked via network.affects_constraints so the contamination engine can model their mutual influence and compute per-seat divergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
