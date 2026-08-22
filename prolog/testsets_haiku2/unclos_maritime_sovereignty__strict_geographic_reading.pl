% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: UNCLOS Strict Geographic Reading: Natural Islands Only Generate Territorial Sea
 *   domain: international_law/maritime_governance
 *
 * SUMMARY:
 *   The strict geographic reading of UNCLOS Article 121 holds that only
 *   naturally formed features that are above water at high tide qualify as
 *   islands capable of generating territorial sea and exclusive economic zone
 *   (EEZ). Artificial islands, man-made islands constructed on submerged
 *   features, and low-tide elevations do not qualify — they are installations
 *   without sovereign water rights. This reading directly benefits naval
 *   powers and non-claimant states by keeping international straits and ocean
 *   resources outside expansionist territorial claims. It extracts from
 *   coastal states (especially China) that have constructed artificial
 *   features in the South China Sea and adjacent zones to claim EEZ control.
 *   The rule's enforcement depends on continuous insistence by the
 *   international legal apparatus (UNCLOS Commission, ICJ, arbitral
 *   tribunals) that artificial features do not count, despite persistent
 *   pressure from expansionist states to recognize them. The measurement
 *   series traces rising extractiveness (from 0.55 to 0.68 at interval end)
 *   as the rule's cost to violators increases: more artificial features are
 *   constructed, more are denied recognition, and the compliance burden on
 *   expansionist states rises. Suppression requirement also rises modestly
 *   (0.62 to 0.71) as the rule's enforcement machinery must work harder to
 *   maintain the boundary against state non-compliance. Theater rises
 *   slightly (0.18 to 0.28) as the UNCLOS apparatus increasingly performs the
 *   rule's legitimacy through adjudication and advisory opinions rather than
 *   passive compliance.
 *
 * KEY AGENTS:
 *   - UNCLOS institutional authority (agenda setter, maintains rule through adjudication)
 *   - Naval powers (beneficiary, maintain freedom of navigation)
 *   - Non-claimant maritime states (beneficiary, protect access to commons)
 *   - Expansionist coastal states (payer, constrained from claiming artificial features)
 *   - Artificial island constructors (payer, identity-locked into a business model the rule denies)
 *   - Island-adjacent fishing communities (payer, face unpredictable EEZ boundaries)
 *   - Environmental advocates (excluded, harmed by construction but outside decision-making)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.68).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.71).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Strict Geographic Reading: Natural Islands Only Generate Territorial Sea").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, 'ce0285c3-6f49-4145-8660-a3818fcd1371').
narrative_ontology:cs_kernel_codification('ce0285c3-6f49-4145-8660-a3818fcd1371', fixed_text).
narrative_ontology:cs_authority_grounding('ce0285c3-6f49-4145-8660-a3818fcd1371', lineage).
narrative_ontology:cs_interpretation_layer_present('ce0285c3-6f49-4145-8660-a3818fcd1371').
narrative_ontology:cs_reading_relation('ce0285c3-6f49-4145-8660-a3818fcd1371', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('ce0285c3-6f49-4145-8660-a3818fcd1371', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('ce0285c3-6f49-4145-8660-a3818fcd1371', foundational, island_definition_geography_only).
narrative_ontology:cs_axiom_status(island_definition_geography_only, holdable).
narrative_ontology:cs_axiom_grounding('ce0285c3-6f49-4145-8660-a3818fcd1371', island_definition_geography_only, conventional).
narrative_ontology:cs_axiom('ce0285c3-6f49-4145-8660-a3818fcd1371', foundational, artificial_construction_does_not_alter_status).
narrative_ontology:cs_axiom_status(artificial_construction_does_not_alter_status, holdable).
narrative_ontology:cs_axiom_grounding('ce0285c3-6f49-4145-8660-a3818fcd1371', artificial_construction_does_not_alter_status, conventional).
narrative_ontology:cs_reference_frame('ce0285c3-6f49-4145-8660-a3818fcd1371', unclos_article_121_literal_text).
narrative_ontology:cs_drift_state('ce0285c3-6f49-4145-8660-a3818fcd1371', contemporary_south_china_sea_construction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ce0285c3-6f49-4145-8660-a3818fcd1371', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_maritime_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, artificial_island_constructors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, island_adjacent_fishing_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The International Court of Justice, UNCLOS Commission on the Limits of the Continental Shelf (CLCS), and the de facto consensus of maritime law scholars and treaty signatories enforce and interpret the geographic criterion. This reading — natural features only — is the baseline formal position. The authority maintains the rule through adjudication, advisory opinions, and technical screening of continental shelf claims.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, unclos_institutional_authority, agenda_setter,
    institutional, generational, analytical, universal).

% Military and commercial fleets benefit from restricted territorial sea claims: the narrower the artificial-island-generated sovereignty, the wider the high seas and international straits remain open to freedom of navigation. They benefit from a rule that does not allow artificial features to claim EEZ control of waterways or resources. Their power comes from naval capacity and the dependence of global trade on open waterways.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    powerful, generational, arbitrage, universal).

% Landlocked and small-island states, plus non-claimant maritime nations, benefit from a rule that limits artificial expansion of territorial claims. The rule protects their access to international straits and common ocean resources. They have limited naval power to enforce alternative rules, so they depend on the UNCLOS framework's geometric rigor.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_maritime_states, beneficiary,
    moderate, generational, constrained, universal).

% Primarily China, but also Vietnam, Philippines, and other claimants in contested zones: bear the cost of restricted artificial-island sovereignty. They construct artificial features (e.g., artificial islands in the South China Sea) to claim EEZ control of submerged features, but the strict reading denies them territorial sea and EEZ status unless the underlying feature is naturally formed and above water at high tide. Their exit is trapped: accepting the rule means abandoning claims; rejecting it means operating outside the treaty framework and facing reputational and enforcement costs.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    powerful, generational, trapped, regional).

% Engineering firms, dredging companies, and state construction agencies that build artificial islands and man-made features. The strict rule treats their work product as installations, not islands, denying the constructed feature any sovereign water rights. They are identity-locked: their business model assumes that constructed features can mature into territorial claims; the strict reading forecloses that path entirely. Exit means abandoning the construction project or reframing it as a non-sovereign facility.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, artificial_island_constructors, payer,
    moderate, biographical, identity_locked, regional).

% Small-scale fishers and communities near natural island clusters or contested artificial features: face unpredictable EEZ boundaries based on whether newly constructed features are recognized as islands or not. The strict reading narrows their access to fishing grounds if artificial features that they thought expanded their coastal state's EEZ are reclassified as mere installations. They are trapped by geographic location and lack the exit option of relocating their fishery.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, island_adjacent_fishing_communities, payer,
    powerless, biographical, trapped, local).

% International environmental organizations and ocean conservation movements are not seated in the UNCLOS apparatus itself but have clear interests: they argue that artificial island construction is ecologically destructive (dredging, reef damage, habitat loss) and that the strict rule, by denying sovereignty to artificial features, should dis-incentivize such construction. They are excluded from formal UNCLOS decision-making but their advocacy shapes the political cost of artificial expansion.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, environmental_advocates, excluded,
    organized, generational, constrained, universal).

% The scholarly community (international law journals, law school curricula, expert witnesses in arbitration) treats the strict geographic reading as the default rule. Academic consensus is not a party but a non-agent entity that stabilizes the rule through teaching and publication. This consensus is a symptom of the rule's institutional entrenchment, not a driver of it.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, academic_legal_consensus, observer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(unclos_maritime_sovereignty__strict_geographic_reading, academic_legal_consensus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__strict_geographic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The UNCLOS geographic criterion coordinates a boundary-drawing problem: when does a feature count as land for the purpose of claiming sovereignty? A uniform rule (natural formation + above water at high tide = island) solves the coordination problem of avoiding conflicting maritime claims based on subjective assessments of artificial construction intent or technical sophistication. It replaces ad-hoc negotiation with a bright-line test.
% TRANSFER_FUNCTION: Moves sovereignty-claim authority (and the EEZ/territorial sea resource access it confers) from expansionist states that construct artificial features to states whose coasts already bear natural islands, and to naval powers that benefit from narrower territorial waters. The rule transfers the capacity to claim new maritime zones by artificial construction from coastal engineers and dredging industries to states that already possess natural geography.
% ABSENT_VOICES: Environmental organizations that are harmed by artificial-island dredging are excluded from UNCLOS decision-making; their interests are not formally represented. Fishing communities adjacent to disputed artificial features have no seat at the treaty table. Artificial-island construction industries have no formal advisory role. These parties would argue for a different rule but are structurally outside the apparatus.
% DISAPPEARANCE_RATIONALE: If the strict geographic reading vanished and were replaced by a rule allowing artificial features to generate territorial claims, the spatial organization of the South China Sea (and similar zones) would reorganize within years: currently disputed artificial islands would become recognized maritime zones, EEZ boundaries would shift, naval transit routes might face restrictions, and coastal states would have incentive to accelerate artificial-island construction. The disappearance would not be neutral — it would unlock a round of claimed-zone expansion and corresponding closure of international waters.
% FOUNDING_PROBLEM: Mid-20th-century maritime law needed a rule to distinguish genuine islands (which generate sovereignty) from mere rocks, low-tide elevations, and human structures. The problem was not purely technical but legal and political: states had incentive to inflate what counted as an island to expand their maritime claims. A bright-line rule (natural formation, above water at high tide) solved the boundary-drawing problem by removing subjective judgment about construction intent.
% FOUNDING_PROBLEM_CORROBORATION: The UNCLOS institution and naval powers attest the founding problem remains live: artificial-island construction in the South China Sea is presented as evidence that states still have strong incentive to inflate island claims, proving the rule's continued necessity. Expansionist coastal states attest the founding problem is obsolete: construction technology and international observation have improved to the point where artificial features can be reliably distinguished and administered under different rules. Arbitral decisions and the 2016 South China Sea tribunal ruling support the strict geographic reading as the binding standard; the tribunal's opinion on artificial features is cited by the UNCLOS institution as external corroboration outside the benefiting parties.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the magnitude of the transfer: expansionist states lose the capacity to claim vast EEZ zones by artificial construction, a substantial economic and strategic asset. Suppression (0.71) is high because the rule's persistence depends on active denial of artificial features' legal status despite state non-compliance in the South China Sea. Formal UNCLOS mechanisms do not have enforcement power (no military capacity); the suppression is primarily normative — the collective assertion by treaty signatories and the ICJ that artificial features remain installations. Theater (0.28) is moderate: the rule is genuinely enforced through adjudication and CLCS screening, but the UNCLOS apparatus increasingly conducts performative reaffirmation of the rule (advisory opinions, statements of principles) as constructors persist. The temporal trajectory shows rising extractiveness because the rule's cost to violators accumulates as they invest in artificial features that remain unrecognized. Suppression rises because the apparatus must maintain the boundary more actively. Theater rises modestly because formal reaffirmation of the rule becomes necessary as state practice diverges from the treaty's text.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional authority and naval-power seats, the constraint is a genuine coordination solution to a boundary-drawing problem: it supplies a bright-line rule (natural + above water at high tide) that removes subjective judgment and prevents arms races in island inflation. From the expansionist-state seat, the same structure operates as a constraint on strategic geographic claims: the rule deliberately forecloses the path to expanded maritime sovereignty through artificial construction, and enforcement means continuous denial of recognition despite expensive compliance-resistance. From the fishing-community seat, the rule creates unpredictable access because artificial features near natural islands remain unrecognized, making EEZ boundaries ambiguous and subject to revision. The engine computes this divergence from power/exit/beneficiary declarations: the institutional authority has power to define the rule, beneficiaries have exit options (maritime trade can route around restricted areas), victims are trapped (coastal states cannot opt out of the treaty framework without severe reputational cost) or identity-locked (construction industries depend on the possibility of artificial-to-natural maturation).
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers sit near d = 0.0 (full beneficiary): the rule subsidizes them by keeping straits open and EEZ claims narrow; they have arbitrage-grade exit (they can choose routes and enforce freedom of navigation). Non-claimant states sit near d = 0.25–0.35 (beneficiary with constrained exit): they benefit from the rule but cannot unilaterally change it; their exit is constrained by dependence on the treaty framework. Expansionist coastal states sit near d = 0.75–0.85 (full target): the rule directly constrains their maritime claims; their exit is trapped because accepting the rule means abandoning claims, and rejecting it means violating a binding treaty with severe costs. Artificial-island constructors sit near d = 0.8+ (target with identity-locked exit): they are identity-fused with a business model the rule forecloses; exit means abandoning their professional practice. Fishing communities sit near d = 0.7 (target with trapped exit): they bear the cost of ambiguous boundaries without the power to change the rule.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint sits at the boundary between coordination (solving the island-definition problem) and extraction (denying expansionist states their desired maritime claims). The mandatrophy risk is whether the founding coordination problem — the need to distinguish genuine islands from inflated claims — remains live or has become a pretext for locking in place a specific distribution of maritime zones. The 2016 South China Sea tribunal ruling, issued by a panel of international law experts, found the strict geographic reading binding and the expansionist construction of artificial features to be non-compliant with UNCLOS. That ruling, issued by a non-claimant tribunal, corroborates the founding problem from outside the benefiting parties. However, subsequent state practice shows continued artificial-island construction and compliance resistance, which suggests the founding problem remains contested — expansionist states argue the problem is no longer acute and the rule is now purely extractive. The classification as tangled_rope (not pure rope, not snare) is correct: the rule solves a real coordination problem (bright-line island definition prevents arms races in claim inflation) AND imposes asymmetric extraction (coastal states lose claims, naval powers gain straits). The active enforcement flag is justified: the rule's persistence depends on continuous UNCLOS/ICJ insistence that artificial features remain installations, not passive compliance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geographic_vs_effective_control_reading_contest,
    'Is the island definition a pure geographic criterion independent of effective occupation, or does prolonged occupation and administrative control gradually upgrade an artificial feature toward island status?',
    'A series of arbitral decisions or advisory opinions from the ICJ that explicitly address whether artificial features can acquire island-like legal status through decades of uncontested administrative control. The 2016 South China Sea tribunal touched this; a clearer pronouncement would resolve it.',
    'If geographic criterion alone applies (this reading''s position), artificial features remain installations indefinitely. If effective control matters, artificial features could eventually claim island status, which would shift the constraint toward the hybrid_effective_control_reading. The classification would migrate from tangled_rope toward snare (pure extraction from time-locked states unable to challenge the original construction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_vs_effective_control_reading_contest, conceptual, 'Whether island status is determined by geography alone or by geography + effective control over time.').

omega_variable(
    natural_feature_definition_boundary,
    'Does ''naturally formed'' mean formed without any human intervention, or does a feature count as natural if it has a natural base (submerged rock or reef) even if the above-water portion was artificially augmented?',
    'CLCS (Commission on the Limits of the Continental Shelf) technical criteria and case-by-case adjudication on disputed features. Some naturally formed features have been augmented with artificial walls or landfill; are they still natural?',
    'A narrow reading (no human intervention at all) supports this constraint. A permissive reading (natural base + any above-water completion) would allow many disputed features to claim island status, migrating the constraint toward the hybrid_effective_control_reading. The beneficiary/victim relationship would shift as the category of recognized islands expands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_feature_definition_boundary, empirical, 'Whether augmented features with natural bases count as naturally formed or as artificial.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) structural — enforced by naval power and treaty enforcement mechanisms — or does it reflect internalized norm acceptance by expanding states that increasingly dispute the rule?',
    'Observe whether artificial-island construction accelerates (structural suppression weakening) or ceases (norm internalization holds). Examine state rhetoric: do states defend their artificial islands as legitimate under UNCLOS or do they explicitly challenge UNCLOS''s geographic criterion?',
    'If suppression is structural and eroding, the constraint''s classification may shift toward snare as enforcement falters. If internalized, the suppression remains stable despite state disagreement. The 2024 South China Sea artificial-island construction continues despite treaty norms, suggesting structural suppression is weakening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression persists through enforcement or through norm internalization by compliant states.').

omega_variable(
    unclos_kernel_reading_contest_location,
    'The kernel contest among strict_geographic_reading, expansive_construction_reading, and hybrid_effective_control_reading — is it about how to interpret ''island'' in Article 121 of UNCLOS, or is it about the validity of UNCLOS itself as a binding constraint on maritime claims?',
    'Examine whether states challenging artificial-island denial do so by proposing alternative readings of Article 121 (interpreting ''island'' differently) or by rejecting UNCLOS''s authority. The South China Sea parties have generally accepted UNCLOS as binding; their dispute is interpretive, not hierarchical.',
    'If the contest is interpretive (this reading assumes it is), the three readings coexist within the UNCLOS framework and the engine''s per-reading classification applies. If the contest is hierarchical (rejecting UNCLOS authority), the constraint would shift to snare/capture because the rule persists only by suppression from naval powers, not consensus. This omega documents the assumption that grounds the reading_relations array.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unclos_kernel_reading_contest_location, conceptual, 'Whether the kernel contest is interpretive (readings of UNCLOS) or hierarchical (rejection of UNCLOS authority).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(uncl_tr_t8, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(uncl_tr_t16, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(uncl_tr_t24, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(uncl_tr_t32, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(uncl_tr_t40, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(uncl_tr_t50, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(uncl_be_t8, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(uncl_be_t16, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(uncl_be_t24, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(uncl_be_t32, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(uncl_be_t40, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(uncl_be_t50, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(uncl_su_t8, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(uncl_su_t16, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(uncl_su_t24, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(uncl_su_t32, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(uncl_su_t40, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(uncl_su_t50, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__strict_geographic_reading, 0.12).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, south_china_sea_maritime_claims).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_enforcement).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the UNCLOS maritime-sovereignty kernel into three structurally distinct readings. The strict_geographic_reading is the baseline formal UNCLOS position, enforced by the international legal apparatus. The expansive_construction_reading represents the de facto claim structure of some coastal states (particularly China) that construct artificial features and assert EEZ control. The hybrid_effective_control_reading is an intermediate position held by some arbitral panels and scholars. Each reading has a different ε (extraction profile), beneficiary/victim structure, and type classification. They are linked by network edges because each reading's legal claims reference and constrain the others: the strict reading is cited to deny the expansive reading, the hybrid reading attempts to split the difference. The three-story family enables the corpus to measure the actual distribution of legal claims and the extractive consequences of each reading for different constituencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__strict_geographic_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
