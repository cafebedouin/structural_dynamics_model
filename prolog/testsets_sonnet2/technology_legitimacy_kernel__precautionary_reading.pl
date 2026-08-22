% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__precautionary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__precautionary_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__precautionary_reading
 *   human_readable: Precautionary Reversibility Standard for Climate Mitigation Technology
 *   domain: energy policy / climate mitigation / technology governance
 *
 * SUMMARY:
 *   Climate finance taxonomies, sustainability-linked bond frameworks, and
 *   multilateral development bank criteria increasingly encode a legitimacy
 *   test for mitigation technologies built on reversibility: does the
 *   technology's worst failure mode, and its long-run legacy cost, resolve
 *   within roughly one human generation? Under this reading, renewables and
 *   grid storage pass; nuclear fails on waste half-life and
 *   low-probability/high-consequence accident grounds, regardless of the
 *   empirical safety record of current reactor designs. The reading was
 *   substantially shaped by advocacy coalitions with a pre-existing
 *   anti-nuclear position, which now administer the standard through taxonomy
 *   lobbying and green-finance eligibility litigation.
 *
 * KEY AGENTS:
 *   - renewables_industry: primary beneficiary (organized/mobile) — gains market and finance access under the standard
 *   - anti_nuclear_advocacy_groups: agenda-setter (organized/arbitrage) — authored and polices the reversibility criterion
 *   - nuclear_industry_workers: primary target (moderate/constrained) — bears exclusion from green finance and taxonomy eligibility
 *   - future_generations: dual beneficiary/payer (powerless/trapped) — named intended beneficiary, actual bearer of whichever irreversible cost the standard's empirical mapping misses
 *   - sustainability_taxonomy_regulators: analytical observer (institutional/analytical) — adjudicates competing kernel readings in binding finance rules
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, 0.61).
domain_priors:suppression_score(technology_legitimacy_kernel__precautionary_reading, 0.52).
domain_priors:theater_ratio(technology_legitimacy_kernel__precautionary_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__precautionary_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__precautionary_reading, "Precautionary Reversibility Standard for Climate Mitigation Technology").
narrative_ontology:topic_domain(technology_legitimacy_kernel__precautionary_reading, "energy policy / climate mitigation / technology governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__precautionary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__precautionary_reading, '161fbf2b-95ec-46a1-935e-6965d4d8d779').
narrative_ontology:cs_kernel_codification('161fbf2b-95ec-46a1-935e-6965d4d8d779', distributed).
narrative_ontology:cs_authority_grounding('161fbf2b-95ec-46a1-935e-6965d4d8d779', distributed).
narrative_ontology:cs_reading_relation('161fbf2b-95ec-46a1-935e-6965d4d8d779', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('161fbf2b-95ec-46a1-935e-6965d4d8d779', technology_legitimacy_kernel__velocity_primacy_reading, influences).
narrative_ontology:cs_axiom('161fbf2b-95ec-46a1-935e-6965d4d8d779', foundational, irreversibility_of_legacy_cost_trumps_near_term_capability).
narrative_ontology:cs_axiom_status(irreversibility_of_legacy_cost_trumps_near_term_capability, holdable).
narrative_ontology:cs_axiom_grounding('161fbf2b-95ec-46a1-935e-6965d4d8d779', irreversibility_of_legacy_cost_trumps_near_term_capability, deontological).
narrative_ontology:cs_axiom('161fbf2b-95ec-46a1-935e-6965d4d8d779', secondary, generational_timescale_is_the_correct_bounding_window_for_legitimacy).
narrative_ontology:cs_axiom_status(generational_timescale_is_the_correct_bounding_window_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('161fbf2b-95ec-46a1-935e-6965d4d8d779', generational_timescale_is_the_correct_bounding_window_for_legitimacy, conventional).
narrative_ontology:cs_reference_frame('161fbf2b-95ec-46a1-935e-6965d4d8d779', post_fukushima_precautionary_consensus).
narrative_ontology:cs_drift_state('161fbf2b-95ec-46a1-935e-6965d4d8d779', post_ipcc_ar6_mitigation_pathway_debate, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('161fbf2b-95ec-46a1-935e-6965d4d8d779', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, renewables_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, storage_and_grid_flexibility_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, anti_nuclear_advocacy_groups).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, future_generations).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, nuclear_industry_workers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, grid_regions_dependent_on_baseload_replacement).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, fossil_fuel_dependent_populations_awaiting_bridge_technology).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, future_generations).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, reversibility_as_legitimacy_criterion).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, intergenerational_precaution_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Solar, wind, and storage developers gain a legitimacy standard that structurally excludes their main dispatchable-power competitor (nuclear) by defining decommissioning reversibility as the deciding criterion. Their assets largely satisfy the bounded-and-reversible test, so the kernel reading converts a normative claim into a market-access advantage without their having to compete directly on reliability grounds.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, renewables_industry, beneficiary,
    organized, generational, mobile, global).

% Battery, demand-response, and interconnection firms benefit because the reversibility standard pushes policy and finance toward technologies whose failure modes are containable within existing engineering horizons, which describes most storage assets. They gain procurement priority and subsidy eligibility as the reading gets embedded in green taxonomies.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, storage_and_grid_flexibility_developers, beneficiary,
    organized, generational, mobile, global).

% Long-standing anti-nuclear coalitions helped author and now police the reversibility criterion in green taxonomies, sustainability-linked bond frameworks, and multilateral climate finance rules. They administer the standard through advocacy, litigation, and taxonomy lobbying rather than direct market participation, and they benefit from the legitimacy the standard confers on their prior position without bearing the costs of the technologies excluded.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, anti_nuclear_advocacy_groups, agenda_setter,
    organized, civilizational, arbitrage, global).

% Cannot participate in current standard-setting. Under this reading they are named as the intended beneficiaries of bounded, reversible legacy costs — but they simultaneously bear the deferred cost of whichever irreversible failure mode this reading fails to prevent: either accumulated fossil emissions if decarbonization is slowed by excluding baseload nuclear, or genuine nuclear legacy costs if the reading is wrong about relative risk. Either way, the people paying are not in the room deciding the standard.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, future_generations, payer).

% Engineers, operators, and supply-chain workers in nuclear power face project cancellations, stranded investment, and career disruption as the reversibility standard excludes their technology from green taxonomies and climate-finance eligibility regardless of operational safety record. Their exit is constrained by sector-specific skills and by the concentration of nuclear projects in a small number of national programs.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_industry_workers, payer,
    moderate, biographical, constrained, national).

% Utility customers and regional grid operators in areas that relied on planned nuclear capacity for decarbonized baseload absorb higher costs and reliability risk when nuclear projects are defunded or blocked under the standard, without a fully reversible substitute yet deployed at matching capacity. They have no say in the taxonomy rules that reclassified their planned technology as illegitimate.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, grid_regions_dependent_on_baseload_replacement, payer,
    powerless, biographical, trapped, regional).

% Communities in regions where nuclear was the fastest path off coal or gas continue breathing fossil emissions and bearing associated health costs while their jurisdiction waits for renewables-plus-storage to reach comparable dispatchable capacity, a delay partly caused by nuclear's exclusion under this reading.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, fossil_fuel_dependent_populations_awaiting_bridge_technology, payer,
    powerless, biographical, trapped, regional).

% Bodies such as EU taxonomy committees and multilateral development banks adjudicate which technologies qualify as legitimate climate finance under competing kernel readings, taking testimony from all sides and periodically revising eligibility criteria as evidence and lobbying pressure shift.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, sustainability_taxonomy_regulators, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__precautionary_reading, diffuse).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__precautionary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, auditable eligibility criterion — bounded, generation-scale-reversible failure modes and legacy costs — that lets climate financiers, regulators, and taxonomy bodies decide which technologies qualify for green capital without re-litigating full risk assessments project by project.
% TRANSFER_FUNCTION: Moves capital eligibility, subsidy access, and taxonomy legitimacy away from technologies with long-tail or irreversible failure profiles (principally nuclear) and toward technologies whose failure and decommissioning costs are contained within roughly one generation (principally renewables and storage), while nominally protecting future generations from irreversible harm.
% ABSENT_VOICES: Future generations who would bear either a slower decarbonization trajectory or genuine nuclear legacy costs have no seat in taxonomy design. Nuclear engineering and safety-science communities that dispute the empirical premise that modern reactor designs carry irreversible risk on the relevant timescale are structurally underweighted relative to advocacy coalitions that pre-date the current safety record.
% DISAPPEARANCE_RATIONALE: If the precautionary reading vanished as the operative kernel reading, green taxonomies and climate finance eligibility criteria would have to be rewritten; nuclear projects currently excluded from sustainable-finance classification would regain access to that capital pool, materially changing investment flows, and advocacy coalitions built around the reversibility criterion would lose a key institutional lever.
% FOUNDING_PROBLEM: Climate mitigation technology choices were being made without any binding check against replacing one irreversible harm (unmitigated emissions) with another (long-lived waste, catastrophic accident tail risk, or stranded infrastructure), so a criterion was needed to keep mitigation from creating new intergenerational liabilities.
% FOUNDING_PROBLEM_CORROBORATION: Independent risk analysts and some non-aligned energy economists corroborate that intergenerational irreversibility is a genuine open problem worth a formal criterion. However, nuclear safety engineers and several IPCC mitigation-pathway modelers dispute that this reading's empirical mapping (excluding modern nuclear, including large-scale mining and grid-storage supply chains) tracks actual reversibility risk rather than reflecting the prior commitments of the advocacy coalitions that administer the standard.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__precautionary_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__precautionary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__precautionary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__precautionary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__precautionary_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__precautionary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__precautionary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects a genuine coordination function (a single auditable eligibility test, sparing case-by-case risk litigation) riding alongside asymmetric extraction: nuclear-sector workers and grid regions that planned around nuclear baseload bear concentrated costs while renewables and storage firms capture concentrated benefits, and the criterion was substantially shaped by parties who already held the resulting position. Suppression (0.52) is moderate — the standard operates through taxonomy exclusion and finance-eligibility gates rather than outright prohibition, so alternatives are constrained rather than eliminated; a nuclear project can still be built, just without green-finance access. Theater ratio (0.38) captures that a meaningful share of the standard's apparent rigor (life-cycle waste modeling, generational-timescale risk bounding) doubles as advocacy vocabulary developed prior to and independent of the specific evidentiary question. Accessibility collapse (0.42) is moderate: nuclear proponents can and do contest the standard in taxonomy revision processes, so the collapse of alternatives is partial, not complete. Resistance (0.71) is high because nuclear industry, several national energy ministries, and a subset of IPCC pathway modelers actively contest the reading's empirical premises.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewables and storage developers derive as beneficiaries (low d) because the criterion converts their existing asset profile into a competitive and financial advantage without requiring them to change behavior. Anti-nuclear advocacy groups are the agenda-setting seat — they administer and police the standard but do not directly capture financial rents, so their directionality sits closer to coordinated-beneficiary than pure extractor, though their organized, arbitrage-grade exit keeps them distinct from those who bear costs. Nuclear workers and dependent grid regions derive as targets (high d): they bear concentrated, constrained-exit costs from a rule they did not help write. Future generations get a directionality override — the naive derivation from 'named beneficiary' would place them near the beneficiary end, but their trapped exit option and powerless status mean the actual expected cost distribution (of an incorrectly calibrated reversibility standard, in either direction) falls overwhelmingly on them. This is why they carry both beneficiary and payer roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing mitigation from creating new irreversible intergenerational harm — remains live in the abstract, but this reading's specific empirical mapping (which technologies count as 'reversible') is contested by parties outside the advocacy coalition that authored it, including some IPCC-aligned modelers. The classification as tangled_rope rather than snare or mountain reflects that a genuine coordination problem exists (someone must set a workable legitimacy criterion for green finance) even as the specific criterion's administration shows asymmetric extraction. Treating this as a settled mountain (natural, inevitable) would hide that reasonable, non-captured parties dispute the mapping; treating it as pure snare would understate that a real coordination function is being solved, however imperfectly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_empirical_calibration,
    'Does the reversibility criterion, as currently mapped onto specific technologies, track actual expected intergenerational harm, or does it encode the prior policy commitments of the coalitions that authored the taxonomy language?',
    'Independent comparative life-cycle and tail-risk analysis across nuclear, renewables-plus-storage, and fossil-transition pathways, conducted by parties with no institutional stake in either the nuclear or anti-nuclear position, benchmarked against the specific taxonomy thresholds in use.',
    'If the mapping is well-calibrated to genuine irreversibility risk, the tangled_rope reading holds as a defensible coordination mechanism with acceptable asymmetric cost. If the mapping is substantially miscalibrated (e.g., nuclear waste risk overweighted relative to modern reactor designs, or fossil-delay costs from nuclear exclusion underweighted), the constraint shifts toward snare — a captured standard using precautionary language as cover for pre-existing anti-nuclear advocacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_empirical_calibration, empirical, 'Whether the precautionary reading''s technology mapping reflects genuine risk calibration or advocacy capture.').

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Where exactly does the precautionary reading''s core premise (bounded reversibility within a generation) conflict with the reliability_primacy_reading''s core premise (dispatchable baseload capacity), such that a single financing framework cannot satisfy both for nuclear specifically?',
    'Structural comparison of taxonomy language across jurisdictions that have adopted each reading (EU taxonomy''s partial nuclear inclusion under conditions vs. jurisdictions with blanket nuclear exclusion) to identify whether the readings are logically incompatible for nuclear or merely produce different weightings that could be reconciled.',
    'If the readings genuinely foreclose each other for nuclear technology specifically, financing frameworks must choose one reading and accept the exclusion it implies; if reconcilable, a hybrid criterion (bounded reversibility AND dispatchability, weighted) could resolve the kernel dispute without a strict either/or.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'Whether precautionary and reliability-primacy readings are logically incompatible for nuclear or merely differently weighted.').

omega_variable(
    future_generations_representation,
    'Given that future generations cannot participate in current taxonomy design, what standing should their interests have in resolving the reversibility criterion, and who legitimately speaks for them?',
    'This is fundamentally a values question about intergenerational representation in institutional decision-making, not resolvable by additional data alone — though empirical discount-rate and risk-aversion research can inform (not settle) the underlying preference question.',
    'A strong precautionary-standing view for future generations supports the current reading''s asymmetric caution against nuclear legacy risk; a view privileging near-term emissions reduction for the sake of the same future generations would favor faster nuclear deployment despite legacy costs. The classification of this constraint as coordination-with-extraction versus legitimate precaution partly turns on this unresolved values question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_representation, preference, 'How much institutional standing future generations'' interests should have in resolving the reversibility criterion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__precautionary_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tech_tr_t4, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(tech_tr_t8, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(tech_tr_t16, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(tech_tr_t24, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tech_be_t4, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(tech_be_t8, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(tech_be_t16, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(tech_be_t24, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 24, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tech_su_t4, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(tech_su_t8, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(tech_su_t16, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(tech_su_t24, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__precautionary_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__precautionary_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__velocity_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints instantiating the technology_legitimacy_kernel (precautionary, reliability_primacy, velocity_primacy readings). Each reading produces a different beneficiary/victim structure from the same underlying kernel text ('a technology is legitimate for climate mitigation if and only if...'). The precautionary reading admits renewables/storage and excludes nuclear on legacy-reversibility grounds; the reliability_primacy reading would admit nuclear and penalize intermittent-only renewables on dispatchability grounds; the velocity_primacy reading would favor whichever technology deploys fastest against the carbon-budget clock, potentially admitting both nuclear and renewables while penalizing slower-to-permit options. Per DP-001 (ε-invariance), these are authored as three separate constraint stories rather than one story with a measurement parameter, because the extraction and beneficiary structure differ substantially across readings. All three are linked via affects_constraints; classification divergence across readings is itself the object of study, not an error to be reconciled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__precautionary_reading, powerless, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
