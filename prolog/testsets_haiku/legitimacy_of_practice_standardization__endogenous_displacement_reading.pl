% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Endogenous Practice Legitimacy via Voluntary Adoption
 *   domain: political/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the endogenous-displacement reading of the
 *   contested kernel 'legitimacy_of_practice_standardization': the claim that
 *   practice change (calendar reform, dress innovation, administrative
 *   procedure revision) is legitimate when it emerges from voluntary adoption
 *   driven by perceived utility or cultural evolution. The reading interprets
 *   the diffusion process as endogenous — communities recognize utility and
 *   adopt incrementally — rather than imposed by state decree. The
 *   constraint's legitimacy rests on the narrative that communities choose,
 *   not on suppression or coercion. Resistance from traditionalist
 *   communities is framed as temporary friction that attenuates as network
 *   effects favor the new practice, not as a suppressed minority bearing
 *   extraction.
 *
 * KEY AGENTS:
 *   - early_adopters: innovators and merchants recognizing utility; drive initial diffusion; benefit from first-mover status and efficiency gains
 *   - lagging_traditionalists: communities for whom old practice carries identity/sacred weight; frame resistance as defense of cultural integrity; progressively marginalized by network effects
 *   - state_administrators: facilitate adoption (remove barriers, coordinate timing) without imposing; permissive role
 *   - cultural_theorists_and_clergy: provide legitimacy narratives; argue coherence between innovation and cultural/spiritual tradition
 *   - counternarrative_traditionalists: EXCLUDED; argue process is coercive despite voluntarism rhetoric; attest to suppression
 *   - international_reference_communities: neighbors using the practice; provide proof of viability without driving adoption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.31).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.18).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous Practice Legitimacy via Voluntary Adoption").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '2bb3f387-2212-4803-87e7-64ad81b62a18').
narrative_ontology:cs_kernel_codification('2bb3f387-2212-4803-87e7-64ad81b62a18', distributed).
narrative_ontology:cs_authority_grounding('2bb3f387-2212-4803-87e7-64ad81b62a18', distributed).
narrative_ontology:cs_reading_relation('2bb3f387-2212-4803-87e7-64ad81b62a18', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('2bb3f387-2212-4803-87e7-64ad81b62a18', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('2bb3f387-2212-4803-87e7-64ad81b62a18', foundational, voluntary_utility_recognition_legitimates_practice_change).
narrative_ontology:cs_axiom_status(voluntary_utility_recognition_legitimates_practice_change, holdable).
narrative_ontology:cs_axiom_grounding('2bb3f387-2212-4803-87e7-64ad81b62a18', voluntary_utility_recognition_legitimates_practice_change, conventional).
narrative_ontology:cs_axiom('2bb3f387-2212-4803-87e7-64ad81b62a18', foundational, cultural_evolution_is_endogenous_coordination_mechanism).
narrative_ontology:cs_axiom_status(cultural_evolution_is_endogenous_coordination_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('2bb3f387-2212-4803-87e7-64ad81b62a18', cultural_evolution_is_endogenous_coordination_mechanism, instrumental).
narrative_ontology:cs_reference_frame('2bb3f387-2212-4803-87e7-64ad81b62a18', utility_driven_practice_evolution).
narrative_ontology:cs_drift_state('2bb3f387-2212-4803-87e7-64ad81b62a18', contemporary_adoption_plateau, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2bb3f387-2212-4803-87e7-64ad81b62a18', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, adopting_communities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, utility_recognizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopters).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_theorists_and_clergy).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, lagging_traditionalists).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_evolution_doctrine).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, voluntary_coordination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities or subgroups that recognize functional utility in an innovation (new calendar system, reformed dress code, revised administrative practice) and adopt it voluntarily. They benefit from reduced coordination friction, improved efficiency, or alignment with perceived modernity. Their adoption sets the diffusion curve.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopters, beneficiary,
    moderate, biographical, mobile, regional).

% Communities for whom the old practice carries identity, sacred, or cultural weight. They face pressure to conform as adoption spreads; the reading treats their resistance as temporary friction in a legitimate endogenous process, not as a suppressed minority. Their exit (maintenance of old practice) becomes progressively costlier as network effects favor the new practice.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, lagging_traditionalists, payer,
    moderate, generational, identity_locked, regional).

% Officials who may facilitate adoption (removing legal barriers, coordinating timing) but do not impose it by decree under this reading. They recognize and enable voluntary shift; they do not drive it. Their role is permissive rather than coercive — they allow the practice to change, not mandate it.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Intellectual and religious authorities who argue that the innovation is culturally coherent, spiritually compatible, or intellectually justified. They provide legitimacy narratives for adoption. Under this reading they are the voice of 'cultural evolution' — not imposed rationality, but reasoned justification that helps communities choose.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_theorists_and_clergy, beneficiary,
    organized, generational, mobile, national).

% Communities and authorities who argue the practice change is fundamentally illegitimate, disruptive of sacred order, or imposed despite the rhetoric of voluntarism. They would argue the process is coercive, not freely chosen. Under this reading they are excluded from the narrative of legitimation.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, counternarrative_traditionalists, excluded,
    organized, civilizational, constrained, regional).

% Neighboring states and jurisdictions already using the new practice, whose example provides proof of viability and utility. They do not drive adoption but serve as evidence that the innovation 'works.' Their existence demonstrates the practice is not arbitrary rationalism but proven coordination.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, international_reference_communities, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__endogenous_displacement_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__endogenous_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine multi-community coordination problem: transitions from practices that no longer fit perceived utility (outdated calendars creating fiscal misalignment, dress codes impeding commerce or health, administrative procedures incompatible with scale) to new practices that communities recognize as more efficient, clearer, or better aligned with shared goals.
% TRANSFER_FUNCTION: Under this reading, the transfer is minimal and temporary: early adopters gain efficiency and status as forward-looking; lagging communities eventually pay the cost of coordination fragmentation until adoption normalizes. Over time, the distribution converges as adoption becomes standard, eliminating the asymmetry.
% ABSENT_VOICES: Traditionalist authorities and communities who argue the practice change is illegitimate — not because of cost, but because it violates sacred order, disrupts identity, or proceeds despite rhetoric of voluntarism masking coercion. They would contest the entire reading and are actively excluded from the 'cultural evolution' narrative.
% DISAPPEARANCE_RATIONALE: If this constraint (the legitimacy claim that practice change via voluntary adoption is justified) disappeared and was replaced by the exogenous-override reading (state decree legitimates change), adoption would accelerate but resistance would spike because the legitimacy frame shifted from 'utility evolution' to 'imposed modernization.' The practice itself might standardize faster, but the populations would reorganize around the new legitimacy claim — resistance would harden rather than fade.
% FOUNDING_PROBLEM: Communities face coordination challenges where existing practices no longer align with perceived utility, efficiency, or external alignment (administrative calendars misaligned with trade, dress codes impeding health or commerce, ritual timings misaligned with agricultural cycles). New practices emerge within communities as solutions. The founding problem is: what legitimates their spread when not all communities see the utility equally?
% FOUNDING_PROBLEM_CORROBORATION: Historians of practice standardization (Gregory, Sahlins, Scott) attest that many practice changes DID proceed endogenously in early-modern Europe and Asia, driven by merchant networks recognizing utility, clergy reinterpreting doctrine, and local elites voluntarily adopting visible innovations. The claim is contested by scholars who emphasize coercive state power in standardization (Ozouf, Anderson, Cohn on colonial metrication and calendars) — they argue the voluntarism was rhetoric and the process was imposed. No consensus exists outside the benefiting parties on whether the endogenous mechanism is historically general or restricted to particular domains.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Under this reading, extractiveness is low-to-moderate (0.31 endpoint) because the constraint operates through incentive and narrative alignment, not coercion. Early adopters benefit; lagging communities face coordination costs that decline as adoption normalizes. Suppression is minimal (0.18) because the reading's core claim is that adoption is voluntary — suppression, if present, is framed as external friction (market pressure, network effects), not state enforcement. Theater ratio is minimal (0.12) because the reading's structural claim is that the functional and legitimacy narratives align: practice change genuinely solves coordination problems, so the functionally real activity and the legitimacy rhetoric track together. The measurement series tracks the adoption curve: extractiveness and suppression both rise as adoption spreads (network effects favor the new practice, making lagging costly) but plateau as adoption approaches saturation. The early rise reflects increasing coordination asymmetry; the plateau reflects the reading's claim that the asymmetry is temporary — once adoption is universal, extraction disappears. If the trajectory continued rising (extractiveness or suppression never plateauing), it would indicate the process was coercive and the endogenous reading was false — the exogenous-override reading would better explain persistent enforcement.
 *
 * PERSPECTIVAL GAP:
 *   Early adopters and state administrators should compute as seeing genuine coordination and voluntary choice; lagging traditionalists should compute as bearing extraction costs and experiencing identity suppression, despite the narrative of voluntarism. The reading's core tension is precisely that divergence: from the adopter seat, the process is cultural evolution; from the traditionalist seat, it is coercive displacement dressed as choice. The engine computes this perspectival gap from the structural data — beneficiary/victim declarations and exit options differentiate the seats. Early adopters have mobile exit and perceived utility; traditionalists have identity_locked exit and face rising coordination costs. That structural asymmetry is what a snare or tangled-rope reading would foreground; this reading claims it is temporary and self-resolving via cultural evolution.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and cultural theorists sit near the beneficiary end (d ≈ 0.2–0.3): they benefit from adoption, their exit is mobile, they are not suppressed. Lagging traditionalists sit near the middle (d ≈ 0.5–0.6): they benefit from the coordination efficiency (eventually) but pay the cost of transition; their identity_locked exit makes them highly vulnerable to network effects. State administrators sit near the analytical end (d ≈ 0.1): they facilitate without collecting. Counternarrative traditionalists are excluded from directionality computation because they are excluded from the stakeholder surface — under this reading, they are not a seat in the process, they are a voice arguing the process is illegitimate.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy (founding problem persistent but function atrophied) under this reading because the founding problem — communities' need to coordinate on new practices when old ones no longer serve utility — is ongoing. Whenever practices misalign with utility (new trade routes requiring calendar alignment, health advances requiring dress reform, administrative growth requiring procedure revision), the endogenous mechanism activates. The reading claims the mechanism is self-sustaining: utility recognition drives adoption without state enforcement. If, however, historical evidence showed that state coercion was ALWAYS necessary to sustain practice standardization, the founding problem would have been misidentified and the reading would be false. That would be a mandatrophy signal: the constraint persists theatrically (legitimacy narratives about 'cultural evolution') while the actual mechanism is exogenous override.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_adoption_vs_coercive_framing,
    'Is the measured adoption curve (gradual, elite-to-mass diffusion, regional variation) evidence of voluntary utility recognition, or evidence of coercive pressure dressed in rhetoric of voluntarism?',
    'Post-adoption survey of lagging communities: do they report adoption as voluntary utility recognition, or as forced by state pressure, market exclusion, or social sanctions? Cross-reference with historical testimony and administrative records to identify whether state enforcement was hidden or absent. Compare with jurisdictions where exogenous-override reading explicitly governed — do adoption curves differ?',
    'If adoption is discovered to be involuntary (state-driven), the endogenous reading is false and the exogenous-override reading better explains the constraint. If genuinely voluntary with lagging communities reporting utility recognition eventually, the endogenous reading is confirmed. If genuinely voluntary but driven by market exclusion (not state decree), the mechanism is still endogenous but extractive — interpretation_layer_present might be false even if adoption is voluntary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_adoption_vs_coercive_framing, empirical, 'Whether adoption curves indicate voluntary utility recognition or coercive imposition.').

omega_variable(
    endogenous_vs_dual_equilibrium_boundary,
    'Does the new practice eventually colonize both public and private domains, or does it stabilize as public-domain standard while private/ritual domains retain the old practice?',
    'Long-interval observation: if private/household/ritual domains persistently use the old practice while administrative/commercial domains standardize on the new, the dual-equilibrium reading better explains the constraint. If the new practice eventually penetrates all domains, the endogenous reading''s claim of complete displacement is supported.',
    'If practice change remains domain-partitioned, the legitimacy framework is not purely endogenous (utility-driven) but incorporates domain-specific authority — the dual-equilibrium reading better captures the actual constraint. This would mean state authority legitimates public standardization while traditional authority legitimates private continuity, not endogenous utility recognition alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_vs_dual_equilibrium_boundary, empirical, 'Whether practice standardization remains domain-partitioned or displaces old practices universally.').

omega_variable(
    cultural_evolution_narrative_vs_rationalization,
    'Is the legitimacy narrative provided by cultural theorists and clergy (''cultural evolution,'' ''doctrinal compatibility'') causally driving adoption, or retrospectively rationalizing adoption driven by material utility or market pressure?',
    'Historical sequence analysis: do legitimacy narratives emerge before or after adoption takes hold? Ethnographic documentation of decision-making in early-adopter communities: do they cite utility first and seek cultural justification, or do they cite cultural justification as primary driver? Compare communities where legitimacy narratives are contested or absent — do they still adopt at similar rates?',
    'If narratives drive adoption, the endogenous mechanism is cultural-evolution (as claimed). If adoption drives narrative-seeking, the mechanism is material utility or market pressure, and the endogenous reading still holds but the legitimacy claim rests on utility, not ''cultural evolution.'' If adoption rate is unaffected by narrative presence/absence, narrative is theater and suppression may be higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_evolution_narrative_vs_rationalization, empirical, 'Whether cultural-evolution narratives drive adoption or rationalize adoption driven by utility/market pressure.').

omega_variable(
    kernel_reading_committer_structure,
    'Which reading of the ''legitimacy_of_practice_standardization'' kernel is the true structural description? Does practice standardization proceed endogenously (this reading), exogenously (state-imposed), or in dual equilibrium (domain-partitioned)?',
    'The three readings are not empirically resolvable simultaneously — each is a framing choice that the evidence can support or refute, but the evidence cannot be theory-neutral. This is a conceptual/preference omega: the choice depends on which legitimacy framework the observer (or the historical authority structure) endorses. The empirical omegas above (voluntary vs. coercive, domain-partitioned vs. universal, narrative-driven vs. retrospective) are concrete signals, but their interpretation depends on the reference frame.',
    'If this reading (endogenous) is chosen, legitimacy rests on utility recognition and voluntary adoption — state enforcement and domain partition are deviations from the mechanism. If exogenous-override is chosen, legitimacy rests on state authority — utility and cultural argument are post-hoc justifications. If dual-equilibrium is chosen, legitimacy is split by domain — neither reading''s claim holds universally. The choice determines which institutional arrangements are treated as legitimate and which as corruptions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Which reading of the practice-standardization kernel is the true committer frame for this constraint and adjacent ones.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement_basis(legi_tr_t10, observed).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(legi_tr_t20, observed).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement_basis(legi_tr_t30, observed).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(legi_tr_t40, observed).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement_basis(legi_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement_basis(legi_be_t10, observed).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement_basis(legi_be_t20, observed).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement_basis(legi_be_t30, observed).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(legi_be_t40, observed).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 50, 0.31).
narrative_ontology:measurement_basis(legi_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 10, 0.06).
narrative_ontology:measurement_basis(legi_su_t10, observed).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement_basis(legi_su_t20, observed).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 30, 0.16).
narrative_ontology:measurement_basis(legi_su_t30, observed).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement_basis(legi_su_t40, observed).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 50, 0.18).
narrative_ontology:measurement_basis(legi_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is ONE reading of a three-way contested kernel: legitimacy_of_practice_standardization. The kernel asks what legitimates practice change across populations when utility and authority assessments diverge. Three structurally distinct readings compete: ENDOGENOUS-DISPLACEMENT (this file) — utility-driven voluntary adoption; EXOGENOUS-OVERRIDE — state-decreed modernization; DUAL-EQUILIBRIUM — domain-partitioned authority (state public, tradition private). Each reading has different ε (low for endogenous, high for exogenous, moderate for dual), different beneficiary/victim structure, different persistence mechanism. The readings constrain one another: if endogenous mechanism can be shown to require state coercion, it forecloses this reading and supports exogenous. If adoption stabilizes at public/private boundary, it forecloses endogenous and influences dual-equilibrium. Family links via network.affects_constraints track the logical dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_practice_standardization__endogenous_displacement_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
