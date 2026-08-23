% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty: Capacity-and-Legitimacy Grading of State Standing
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   The graduated-sovereignty reading operates today as an assessment machine
 *   layered over formally equal UN membership: composite
 *   capacity-and-governance indices (CPIA, Worldwide Governance Indicators,
 *   fragility rankings), Security Council practice, post-conflict
 *   transitional administrations, and governance-conditioned lending together
 *   sort states into effective tiers of deference. States scoring high on
 *   externally published measures retain full command of fiscal, security,
 *   and constitutional policy; states scored low find policy authority
 *   migrating to external administrators, conditionality regimes, and
 *   authorized missions. The arrangement solves a real problem - coordinating
 *   protection and reconstruction where territorial governments fail - while
 *   concentrating the right to classify in a handful of seats that never
 *   submit to assessment themselves. This file instantiates ONLY the
 *   graduated_sovereignty reading of the westphalian_sovereignty kernel; the
 *   absolute and conditional readings are separate constraints with their own
 *   victim sets, linked here through the network surface rather than merged.
 *   The claim and the metrics are authored independently: the structural
 *   claim is tangled_rope because protective coordination and hierarchical
 *   extraction demonstrably run through the same grading machinery; the
 *   metrics report that machinery's observed operation, and any divergence
 *   between claim and computed type is the measurement this corpus exists to
 *   take.
 *
 * KEY AGENTS:
 *   - permanent_five_states: agenda-setting beneficiary (institutional/arbitrage) - writes the doctrine, holds the veto, exempts itself from grading; the seat the arrangement's gains demonstrably land on
 *   - international_financial_institutions: administering agenda-setter (institutional/arbitrage) - operates the indices and conditionality that convert assessment into policy control
 *   - non_p5_donor_governments: incidental beneficiary (institutional/mobile) - buys coordination and burden-sharing without holding classification authority
 *   - international_humanitarian_organizations: funded participant (moderate/constrained) - collects program funding while paying independence and neutrality costs
 *   - fragile_and_conflict_affected_states: primary target (powerless/trapped) - bears reclassification risk on every fiscal and security decision
 *   - weak_state_populations: double-bearing target (powerless/trapped) - bears both state failure and the interventions that answer it
 *   - middle_powers_and_regional_powers: excluded challenger (organized/constrained) - contests the right to assess without access to operative decision points
 *   - international_law_academics: analytical observer (analytical/analytical) - maps the standard-of-civilization lineage to the present index regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.66).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.62).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.66).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty: Capacity-and-Legitimacy Grading of State Standing").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, 'c449b119-5e38-4c49-aee3-b494f50878f3').
narrative_ontology:cs_kernel_codification('c449b119-5e38-4c49-aee3-b494f50878f3', fixed_text).
narrative_ontology:cs_authority_grounding('c449b119-5e38-4c49-aee3-b494f50878f3', extraction).
narrative_ontology:cs_interpretation_layer_present('c449b119-5e38-4c49-aee3-b494f50878f3').
narrative_ontology:cs_reading_relation('c449b119-5e38-4c49-aee3-b494f50878f3', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('c449b119-5e38-4c49-aee3-b494f50878f3', westphalian_sovereignty__conditional_sovereignty, influences).
narrative_ontology:cs_axiom('c449b119-5e38-4c49-aee3-b494f50878f3', foundational, sovereign_standing_is_continuously_graded).
narrative_ontology:cs_axiom_status(sovereign_standing_is_continuously_graded, holdable).
narrative_ontology:cs_axiom_grounding('c449b119-5e38-4c49-aee3-b494f50878f3', sovereign_standing_is_continuously_graded, empirically_contingent).
narrative_ontology:cs_axiom('c449b119-5e38-4c49-aee3-b494f50878f3', secondary, external_classifiers_hold_legitimate_grading_authority).
narrative_ontology:cs_axiom_status(external_classifiers_hold_legitimate_grading_authority, holdable).
narrative_ontology:cs_axiom_grounding('c449b119-5e38-4c49-aee3-b494f50878f3', external_classifiers_hold_legitimate_grading_authority, instrumental).
narrative_ontology:cs_reference_frame('c449b119-5e38-4c49-aee3-b494f50878f3', graded_deference_tiered_membership).
narrative_ontology:cs_drift_state('c449b119-5e38-4c49-aee3-b494f50878f3', multipolar_contestation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c449b119-5e38-4c49-aee3-b494f50878f3', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, permanent_five_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, non_p5_donor_governments).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_humanitarian_organizations).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, fragile_and_conflict_affected_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_state_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, international_humanitarian_organizations).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, good_governance_conditionality).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, state_capacity_measurement_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power over Security Council authorization, author intervention doctrine, and decide which graded states attract enforcement attention. Apply capacity-and-legitimacy assessment to others while exempting themselves and close allies from equivalent scrutiny; the veto and charter privileges give them a working exit from the grading they administer. Classification discretion and intervention license accrue to this seat.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, permanent_five_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, permanent_five_states, beneficiary).

% Publish the governance and capacity indices that operationalize grading, attach policy conditions to crisis lending, and administer post-conflict trust funds. Weighted voting insulates board decisions from borrower-state challenge. Lending volume, fee income, and policy leverage flow through the assessment machinery they operate, under governance weighted toward the same states that hold the Security Council pen.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions, beneficiary).

% Fund state-building, peacekeeping, and humanitarian operations channeled through the graded framework. They obtain coordinated allocation and burden-sharing without holding classification authority, and retain the option of funding bilaterally outside the framework when its decisions displease them - an exit most participants lack.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, non_p5_donor_governments, beneficiary,
    institutional, biographical, mobile, continental).

% Receive the bulk of crisis-response funding routed through graded-state designations and implement protection and service programs inside administered or intervened territories. Funding dependence disciplines public criticism, and operating under external administration exposes staff to danger and erodes perceived neutrality - a recurring operating cost that arrives alongside the contracts.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_humanitarian_organizations, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, international_humanitarian_organizations, payer).

% Subject to continuous assessment by indicators they did not design. Reclassification downgrades market access, credit terms, and diplomatic deference, while promised upgrades arrive tied to externally defined reforms. Formal UN membership guarantees procedural equality, but effective command over fiscal, security, and constitutional policy narrows with each downgrade. Leaving the system - defaulting, expelling monitors, rejecting missions - reliably deepens the designation.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, fragile_and_conflict_affected_states, payer,
    powerless, immediate, trapped, national).

% Bear both sides of the arrangement: the state failure that triggers grading, and the interventions, sanctions, and externally administered transitions that follow. Sometimes shielded by protective action; recurrently exposed to embargo pain, mission misconduct, and prolonged administration conducted without their consent or any available exit.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, weak_state_populations, payer,
    powerless, immediate, trapped, national).

% States such as Brazil, India, and South Africa, and leading regional organizations, retain recognized sovereign standing yet contest who holds the right to assess others. They table reform proposals on council composition and index governance that never reach operative decision points, while watching the grading standard extend toward their regions.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, middle_powers_and_regional_powers, excluded,
    organized, generational, constrained, continental).

% Map the doctrinal lineage from the nineteenth-century standard of civilization through mandate and trusteeship systems to contemporary indices, publishing analyses that neither bloc commands. Their seat sees the whole structure without collecting from it or paying into it.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_law_academics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__graduated_sovereignty, permanent_five_states).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__graduated_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international response to state failure: pooled early warning, standardized assessment of which territories lack effective protective government, shared funding channels, and pre-negotiated mandates for peacekeeping and transitional administration - capacities no single weak state or small donor coalition can assemble alone.
% TRANSFER_FUNCTION: Moves policy authority from low-graded states to external administrators, boards, and mission leadership; moves financing and security services from donor treasuries and institutional lending arms into graded territories on externally set terms; and moves positional standing - market access, credit terms, diplomatic deference - downward from graded states to classifier states as a by-product of ranking.
% ABSENT_VOICES: Populations of graded states are represented only through governments the process itself demotes; weak-state officials attend indicator consultations without agenda-setting votes; sovereigntist and South-South framings are heard in General Assembly debate but absent from the operative rooms - Security Council consultations, IFI boards, index editorial panels - where assessment converts into action.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand transitional administrations without warrant, suspend conditionality-linked tranches, void mission mandates pending renewal, and return nominal policy authority to graded governments immediately, while humanitarian response reorganizes around consent-based and regional mechanisms within months. Donor allocation pipelines, index publishers, and intervention doctrine all presuppose the grading machinery.
% FOUNDING_PROBLEM: How an order of formally equal states should respond to territories whose governments cannot or will not protect their populations - posed successively by the incorporation of the Ottoman and Qing worlds under the standard of civilization, the League mandates, the UN trusteeship system, and post-Cold War state collapse.
% FOUNDING_PROBLEM_CORROBORATION: Atrocity-prevention scholarship and frontline humanitarian agencies corroborate from outside the benefiting seats that state failure is real and response capacity scarce. Postcolonial international-law scholarship and the public positions of the African Union and Non-Aligned Movement corroborate the opposite pole: that the assessment side of the arrangement reproduces a standard-of-civilization hierarchy whose categories predate the crises they claim to answer. Both poles attest externally; the self-assessments of the permanent members and the institutions count for nothing under this provenance rule.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.66 reflects the measured wedge between graded deference and equal membership: policy autonomy, market access, and diplomatic standing move with index scores that weak states cannot procedurally contest. Suppression 0.62 is authored as raw structure and is NOT scaled by power or scope anywhere in this story; the scalar reports how little exit the assessment regime leaves (default-risk exclusion, sanction exposure, mission imposition), while the engine separately amplifies effective extraction for trapped targets and large scopes. Theater 0.47 splits the activity: real protection, peacekeeping, and reconstruction on one side; index ritual, periodic review cycles, benchmark ceremony, and compliance performance on the other. Accessibility_collapse 0.42 keeps alternatives partly alive - South-South lending, regional mutual review, bilateral aid - though mainstream discourse treats them as second-best. Resistance 0.58 records organized contestation: caucus voting, abstention blocs, veto blocks, and the post-Libya backlash. The temporal series shares one grid (seven points, 1992-2025, all three metrics authored at every point) so drift detection reads a common timeline: extraction climbs through the Kosovo, Iraq, and Libya sequence, dips at the 2005 codification of protection caveats, and resumes climbing as selection visibly politicizes; theater rises steadily as index production outpaces enforcement; the suppression requirement ratchets with each contested case. Coalition note: the powerless target seats hold a numerical General Assembly majority, but the veto converts that coalition into voice without enforcement power - the structural reason latent coalition potential does not lower their effective extraction. Receipt surface: the gains demonstrably accrue to the permanent-five seat - classification discretion and intervention license - with institutional lending leverage derivative of it, so gain_flow names that seat. Fixing cost is prohibitive: the seats that could rewrite the assessment regime are precisely the seats the regime empowers, and every amendment path runs through their veto.
 *
 * PERSPECTIVAL GAP:
 *   From the permanent-member and institutional seats the arrangement is stewardship: they built the indices, staff the boards, and experience assessment as technical assistance - an institutional identity fused with the managerial role such that questioning the grading feels like abandoning the populations it protects. From the fragile-state seat the identical machinery is standing reclassification risk attached to every sovereign decision. The humanitarian-implementer seat straddles the divide: financed by the machinery and disciplined by it. The engine computes per-seat classifications from the power, exit, and role data; nothing in the authored claim adjudicates between these experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The permanent members and the financial institutions anchor the beneficiary end: declared beneficiaries with arbitrage-grade exit - they assess others and are not assessed. Non-permanent donors sit nearby but hold mobile exit, damping their effective extraction further toward subsidy. Fragile states and their populations anchor the target end: declared victims with trapped exits, sitting nearest the full-target end of the directionality scale. One override corrects the derivation: international_humanitarian_organizations are this story's only moderate-power seat and are declared beneficiaries, which derives a low directionality (roughly 0.1-0.15); but they demonstrably pay independence and neutrality costs under external administration, so directionality is overridden to 0.32. The chain priority places the explicit override above the structural derivation; without it the seat would compute as nearly a pure beneficiary and miss the documented cost side of its dual position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - organizing protection where states fail their populations - is contested rather than dead, and the disappearance verdict is world_rearranges, so the mismatch consumer reads contested x rearranges and raises no zombie flag. Mandatrophy discipline matters here in a different way: collapsing the arrangement into pure coordination would erase the reclassification harms borne by graded states; collapsing it into pure extraction would erase the protective record - regional interventions in Liberia, Sierra Leone, and the Gambia, atrocity-prevention diplomacy in Kenya 2008 - that gives the grading both its cover and its genuine function. The hybrid classification keeps both facts load-bearing and directs scrutiny at the seam: who assesses, and whether the assessors are ever assessed. If the protective function continues to atrophy while index ceremony grows, theater_ratio crossing 0.5 would date the transition toward piton dynamics; the 2025 measurement sits just below that line.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality,
    'This story instantiates only the graduated_sovereignty reading of the westphalian_sovereignty kernel; how would classification shift under the sibling readings?',
    'Author parallel stories for absolute_sovereignty and conditional_sovereignty with the identical standing-arrangement referent and compare computed types, victim sets, and per-seat effective extraction.',
    'Under absolute_sovereignty the licensed-intervention machinery loses its warrant and epsilon collapses toward zero for the norm itself, while the same interventions re-reference to a different constraint; under conditional_sovereignty the victim set narrows to states committing systematic violations, removing low-capacity-but-orderly states from the target class. The readings disagree on where sovereign standing sits: constant, threshold-triggered, or continuously graded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality, conceptual, 'Kernel-reading indexicality: sibling readings instantiate different constraints with different victim sets over the same referent.').

omega_variable(
    classification_discretion_politicization,
    'Is capacity-and-legitimacy assessment applied symmetrically by rule, or selectively according to the classifiers'' strategic interests?',
    'Cross-case comparison of published index trajectories against enforcement outcomes (intervention, sanction, administration) across comparably scoring states aligned and opposed to the classifier states.',
    'Symmetric application would certify the coordination half as dominant and support the tangled_rope reading; systematic selectivity would push the arrangement toward snare and substantiate the neo-colonial extraction pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_discretion_politicization, empirical, 'Whether grading is rule-bound or politically discretionary.').

omega_variable(
    consent_baseline_of_participation,
    'Is graded-state participation consensual - borrowing and monitoring sought under ordinary incentives - or imposed, accepted under threat of exclusion, default cascade, or mission authorization?',
    'Audit of lending and monitoring acceptance episodes, distinguishing voluntary uptake from default-threat sequences and pre-intervention timing.',
    'Consensual uptake lowers attributable suppression; threat-structured uptake raises it and reinforces the trapped-exit reading of the target seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_baseline_of_participation, empirical, 'Consent baseline beneath formally voluntary participation in the grading regime.').

omega_variable(
    alternative_infrastructure_viability,
    'Can alternative financing and review infrastructures - South-South lending, regional mutual assessment, bilateral aid - give graded states durable exit from the dominant assessment system?',
    'Track substitution rates: the share of fragile-state external financing and assessment relationships migrating to non-Western-led instruments across successive five-year windows.',
    'Viable substitutes would lower accessibility_collapse, soften trapped exits, and erode the arrangement''s persistence; demonstrated failure of substitutes confirms closure and supports higher effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_infrastructure_viability, empirical, 'Whether real exit infrastructure exists outside the dominant grading system.').

omega_variable(
    welfare_referent_ambiguity,
    'Whose welfare fixes the extraction measure - weak-state governing elites losing policy autonomy, or populations gaining protection and services under external administration?',
    'Not resolvable by data alone; resolved by an explicit weighting choice declared per seat before comparing classifications.',
    'An elite-autonomy referent maximizes measured extraction; a population-welfare referent credits protective transfers and can invert directionality for some target seats. Classification is referent-relative until the weighting is declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_referent_ambiguity, preference, 'Value-dependent choice of whose losses and gains count in the extraction measure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 1992, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1992, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1992, 0.3).
narrative_ontology:measurement_basis(west_tr_t1992, observed).
narrative_ontology:measurement(west_tr_t1998, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1998, 0.34).
narrative_ontology:measurement_basis(west_tr_t1998, observed).
narrative_ontology:measurement(west_tr_t2003, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2003, 0.38).
narrative_ontology:measurement_basis(west_tr_t2003, observed).
narrative_ontology:measurement(west_tr_t2005, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2005, 0.36).
narrative_ontology:measurement_basis(west_tr_t2005, observed).
narrative_ontology:measurement(west_tr_t2011, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2011, 0.4).
narrative_ontology:measurement_basis(west_tr_t2011, observed).
narrative_ontology:measurement(west_tr_t2017, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2017, 0.44).
narrative_ontology:measurement_basis(west_tr_t2017, observed).
narrative_ontology:measurement(west_tr_t2025, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2025, 0.47).
narrative_ontology:measurement_basis(west_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(west_be_t1992, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1992, 0.54).
narrative_ontology:measurement_basis(west_be_t1992, observed).
narrative_ontology:measurement(west_be_t1998, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1998, 0.58).
narrative_ontology:measurement_basis(west_be_t1998, observed).
narrative_ontology:measurement(west_be_t2003, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2003, 0.64).
narrative_ontology:measurement_basis(west_be_t2003, observed).
narrative_ontology:measurement(west_be_t2005, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement_basis(west_be_t2005, observed).
narrative_ontology:measurement(west_be_t2011, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2011, 0.65).
narrative_ontology:measurement_basis(west_be_t2011, observed).
narrative_ontology:measurement(west_be_t2017, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2017, 0.67).
narrative_ontology:measurement_basis(west_be_t2017, observed).
narrative_ontology:measurement(west_be_t2025, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2025, 0.66).
narrative_ontology:measurement_basis(west_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1992, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1992, 0.48).
narrative_ontology:measurement_basis(west_su_t1992, observed).
narrative_ontology:measurement(west_su_t1998, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1998, 0.53).
narrative_ontology:measurement_basis(west_su_t1998, observed).
narrative_ontology:measurement(west_su_t2003, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2003, 0.6).
narrative_ontology:measurement_basis(west_su_t2003, observed).
narrative_ontology:measurement(west_su_t2005, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2005, 0.57).
narrative_ontology:measurement_basis(west_su_t2005, observed).
narrative_ontology:measurement(west_su_t2011, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2011, 0.59).
narrative_ontology:measurement_basis(west_su_t2011, observed).
narrative_ontology:measurement(west_su_t2017, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2017, 0.6).
narrative_ontology:measurement_basis(west_su_t2017, observed).
narrative_ontology:measurement(west_su_t2025, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(west_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, conditional_sovereignty).

% DUAL FORMULATION NOTE:
% Constraint family: the westphalian_sovereignty kernel decomposes into three reading-stories - absolute_sovereignty, conditional_sovereignty, and this graduated reading - because no single story can carry one stable epsilon across readings that assign different victim sets (none; violators only; all low-capacity states). Each file links the others via affects_constraints. Downstream pressure runs from this reading's index infrastructure into the conditional reading's trigger determinations, since published capacity assessments supply the evidentiary basis on which conditional intervention claims are argued.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__graduated_sovereignty, moderate, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
