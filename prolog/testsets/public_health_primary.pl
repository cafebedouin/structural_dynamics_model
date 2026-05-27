% ============================================================================
% CONSTRAINT STORY: public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_primary, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: public_health_primary
 *   human_readable: Public Health Protection via Vaccine Mandate (Collective Necessity Reading)
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   The vaccine mandate constraint in this reading is framed as a necessary
 *   enforcement mechanism when voluntary vaccination compliance fails to
 *   achieve herd immunity thresholds and structurally vulnerable populations
 *   (immunocompromised, infants too young to vaccinate, individuals with
 *   documented allergies) face lethal exposure risk without collective
 *   protection. This is ONE READING of the contested kernel
 *   'vaccine_mandate_balance.' This reading instantiates the
 *   public_health_primary interpretation, which prioritizes the protection of
 *   vulnerable populations over individual consent when voluntary compliance
 *   demonstrably fails. The sibling readings — bodily_autonomy_primary (which
 *   prioritizes individual consent) and proportionality_reading (which weighs
 *   both but requires demonstrable proportionality) — offer alternative
 *   framings of the same kernel. This constraint story models the structural
 *   claim: collective necessity can override individual refusal when (1) the
 *   collective protection is empirically necessary (herd immunity threshold
 *   unmet), (2) vulnerable populations face genuine lethal risk, and (3) the
 *   enforcement mechanisms are proportional to that necessity. The constraint
 *   exhibits tangled rope structure: it coordinates disease prevention
 *   (genuine public health function) while enforcing compliance through
 *   mechanisms that extract consent from resistant populations (asymmetric
 *   burden). The low theater ratio (0.35) reflects that vaccine mandate
 *   enforcement is functionally transparent — the mechanism works as stated
 *   (vaccines do confer immunity, mandates do increase compliance) rather
 *   than relying on performative ritual.
 *
 * KEY AGENTS:
 *   - Immunocompromised Populations: Primary beneficiary (powerless/trapped immediate local) — face lethal exposure without herd immunity; cannot be protected individually. Mandate eliminates their exposure risk.
 *   - Unvaccinated Individuals: Primary victims (powerless/trapped biographical national) — face employment loss, school exclusion, social restriction; no legitimate exit pathway. Mandate extracts consent through coercion.
 *   - Religious Objectors: Secondary victims (moderate/constrained generational national) — face high but surmountable costs (relocation, community exit); also benefit from collective protection. Mixed extraction and benefit.
 *   - Public Health Authority: Institutional actor (organized/constrained generational national) — enforces mandate to achieve herd immunity; benefits from legitimacy of disease suppression; extracts compliance through legal mechanisms.
 *   - Pharmaceutical Industry: Institutional beneficiary (institutional/arbitrage civilizational global) — mandates create sustained vaccine demand and market expansion. Experiences constraint as pure coordination of their economic interests.
 *   - Analytical Observer: Civilizational frame (analytical/analytical civilizational global) — evaluates whether public health necessity justifies enforcement asymmetry.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_primary, 0.58).
domain_priors:suppression_score(public_health_primary, 0.68).
domain_priors:theater_ratio(public_health_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(public_health_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(public_health_primary, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_primary, "Public Health Protection via Vaccine Mandate (Collective Necessity Reading)").
narrative_ontology:topic_domain(public_health_primary, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_primary, '47358252-490b-43bc-b3fc-e982a76a2dcd').
narrative_ontology:cs_created_at('47358252-490b-43bc-b3fc-e982a76a2dcd', '').
narrative_ontology:cs_kernel_codification('47358252-490b-43bc-b3fc-e982a76a2dcd', formalized).
narrative_ontology:cs_authority_grounding('47358252-490b-43bc-b3fc-e982a76a2dcd', lineage).
narrative_ontology:cs_interpretation_layer_present('47358252-490b-43bc-b3fc-e982a76a2dcd').
narrative_ontology:cs_kernel_id(public_health_primary, vaccine_mandate_balance).
narrative_ontology:cs_reading_relation('47358252-490b-43bc-b3fc-e982a76a2dcd', bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('47358252-490b-43bc-b3fc-e982a76a2dcd', proportionality_reading, influences).
narrative_ontology:cs_axiom('47358252-490b-43bc-b3fc-e982a76a2dcd', foundational, public_health_necessity_supersedes_consent).
narrative_ontology:cs_axiom_status(public_health_necessity_supersedes_consent, holdable).
narrative_ontology:cs_axiom_grounding('47358252-490b-43bc-b3fc-e982a76a2dcd', public_health_necessity_supersedes_consent, deontological).
narrative_ontology:cs_axiom('47358252-490b-43bc-b3fc-e982a76a2dcd', secondary, proportionality_requirement_for_enforcement).
narrative_ontology:cs_axiom_status(proportionality_requirement_for_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('47358252-490b-43bc-b3fc-e982a76a2dcd', proportionality_requirement_for_enforcement, instrumental).
narrative_ontology:cs_reference_frame('47358252-490b-43bc-b3fc-e982a76a2dcd', vulnerable_population_protection_as_core_duty).
narrative_ontology:cs_drift_state('47358252-490b-43bc-b3fc-e982a76a2dcd', contemporary_pandemic_response, gap(authority_erosion, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_primary, infants_unvaccinatable).
narrative_ontology:constraint_beneficiary(public_health_primary, vaccine_allergic_populations).
narrative_ontology:constraint_beneficiary(public_health_primary, public_health_collective).
narrative_ontology:constraint_victim(public_health_primary, unvaccinated_individuals_subject_to_mandate).
narrative_ontology:constraint_victim(public_health_primary, religious_objectors).
narrative_ontology:constraint_victim(public_health_primary, medical_autonomy_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNVACCINATED INDIVIDUAL (SNARE) — Faces legal coercion (employment loss, school exclusion, fines) with no exit pathway. Suppression is structural: employment dependency, geographic scope of mandate, legal status. Extraction is maximal because the individual has no legitimate avenue to refuse. The mandate mechanism itself is the suppression.
constraint_indexing:constraint_classification(public_health_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IMMUNOCOMPROMISED PERSON (ROPE) — When herd immunity fails (low vaccination rates), experiences the constraint as pure coordination: vaccine mandate solves their lethal exposure risk without extracting from them. No cost, maximum benefit. From their immediate, local perspective, the constraint is coordination that enables their survival. Exit would mean accepting lethal risk — not a real option.
constraint_indexing:constraint_classification(public_health_primary, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: RELIGIOUS OBJECTOR (TANGLED ROPE) — Faces high but surmountable costs to refuse: relocation, community exit, school switching. Also benefits from collective immunity protection if disease becomes endemic — their own vulnerable family members gain protection even if they personally refuse. Mixed extraction and coordination: the individual bears extraction cost (forced choice between conviction and livelihood) but also benefits from the collective protection the mandate enables.
constraint_indexing:constraint_classification(public_health_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH AUTHORITY (TANGLED ROPE) — Enforces the mandate to achieve herd immunity; extracts compliance from resistant populations while coordinating disease prevention for the collective. Active enforcement required. The authority benefits from successful disease suppression (legitimacy, resource justification) and extracts through coercive mechanisms. Genuine coordination function (preventing epidemic) paired with asymmetric enforcement burden.
constraint_indexing:constraint_classification(public_health_primary, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PHARMACEUTICAL INDUSTRY (ROPE) — Mandate creates sustained demand for vaccines; extracts economic benefit through market creation. Experiences the constraint as pure coordination: the mandate solves their sales and distribution challenge. No coercion from their perspective; the constraint coordinates their interests perfectly. High exit optionality (can supply other markets, develop new vaccines) mitigates extraction risk.
constraint_indexing:constraint_classification(public_health_primary, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational view, vaccine mandates represent a genuine coordination mechanism (herd immunity protects populations below vaccination threshold) paired with asymmetric enforcement (coercion of holdouts). The constraint is not a natural law but a structurally necessary choice when voluntary vaccination fails to reach critical thresholds. Genuine coordination function + active enforcement + beneficiaries + victims = tangled rope from the canonical analytical position.
constraint_indexing:constraint_classification(public_health_primary, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_health_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_health_primary, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts consent from unvaccinated individuals through employment and school exclusion, representing substantial economic and social coercion. However, the value is moderated by the coordination function: the mandate solves a genuine collective action problem (free-rider dynamics in voluntary vaccination) that prevents herd immunity from being reached. The extraction rises over time (from 0.35 to 0.58) as enforcement mechanisms harden and social pressure accumulates, reflecting the deepening of coercive mechanisms. Suppression (0.68): High. Structural barriers to refusing the mandate include employment dependency (loss of income), mandatory school attendance (children's access to education), geographic scope (state or national level, no exit to non-mandate jurisdiction), and legal status (violation subjects individuals to legal penalties). Alternative options are constrained by practical reality: relocation is economically unfeasible for most; homeschooling is not available to all; geographic arbitrage is blocked by coordinated mandates across jurisdictions. Theater ratio (0.35): Low. The constraint's mechanism is transparent and functionally operative: vaccines do provide immunity, mandates do increase compliance, herd immunity does protect vulnerable populations. No performative ritual obscures the actual mechanism. The theater component that exists reflects only the administrative apparatus (reporting requirements, documentation) rather than the core mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence driven by structural position and vulnerability status. The unvaccinated individual experiences Snare (maximal extraction, no coordination benefit). The immunocompromised person experiences Rope (pure coordination, maximum benefit, no extraction). The religious objector experiences mixed Tangled Rope (both extraction and benefit; benefit comes through collective protection they didn't consent to). The public health authority experiences Tangled Rope with beneficiary status (genuine coordination function paired with extraction mechanisms they deploy). The pharmaceutical industry experiences Rope (coordination of their market interests). The analytical observer at civilizational scope sees Tangled Rope (the structure is genuinely mixed: real coordination function + real extraction). The perspectival gap reveals that the constraint is not purely extractive (the immunocompromised person's experience proves coordination is real) but also not purely coordinative (the unvaccinated person's coercion is structurally real). This gap is the diagnostic feature that distinguishes this reading from the bodily_autonomy_primary reading, which would classify the constraint as Snare from all victim perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values vary sharply by agent structural position. Immunocompromised beneficiaries derive d ≈ 0.15 (beneficiary + trapped exit = full benefit from constraint) → low effective extraction. Unvaccinated individuals subject to mandate derive d ≈ 0.88 (victim + trapped exit = full extraction target) → high effective extraction. Religious objectors derive d ≈ 0.60 (mixed: victim of enforcement but also beneficiary of collective protection; constrained exit = moderate extraction). Public health authority derives d ≈ 0.35 (institutional beneficiary deploying enforcement; arbitrage exit = moderate extraction reversal). The pharmaceutical industry derives d ≈ 0.05 (institutional beneficiary with arbitrage options = negative extraction, pure coordination). The canonical analytical observer derives d ≈ 0.72 (no structural position advantage; observes asymmetry) → moderate extraction from analytical perspective. The directionality derivation is primary to this reading's structure: IF the vulnerable populations are real and face genuine lethal risk, their low d value (beneficiary) is justified, confirming the coordination function. IF unvaccinated individuals are coerced through enforceable legal mechanisms, their high d value (victim + trapped) is justified, confirming the extraction function. The reading stands on the empirical claim that BOTH are true: genuine coordination + real extraction = Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy through explicit structural balance: it claims that vaccine mandates are neither pure extraction (Snare) nor pure coordination (Rope) but a genuine hybrid. The vulnerability of immunocompromised populations proves the coordination function is real — the constraint solves a problem (herd immunity for those who cannot vaccinate themselves) that cannot be solved through individual choice. The coercion of unvaccinated individuals proves the extraction function is real — the constraint uses enforcement mechanisms that violate individual autonomy. The constraint's justification rests on the claim that the coordination benefit (protecting the lethal risk to vulnerable populations) outweighs the extraction cost (violating individual consent). This is a substantive ethical claim, not a structural claim about classification. The mandatrophy is resolved by distinguishing the structural question (is this Tangled Rope?) from the ethical question (is it justified?). The constraint is structurally Tangled Rope; the ethical reading argues it is justified Tangled Rope when (and only when) the empirical conditions in the omegas are satisfied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    herd_immunity_threshold_empirical,
    'What is the actual herd immunity threshold for a given pathogen, and has voluntary compliance achieved it in the jurisdiction?',
    'Epidemiological modeling + observed vaccination rates vs. R-effective calculations. Direct measurement of disease transmission chains in the population.',
    'If voluntary rates exceed threshold: mandate is unnecessary extraction (Snare reclassifies). If voluntary rates fall below threshold and vulnerable populations face lethal exposure: mandate is necessary coordination (Tangled Rope confirmed). This is the empirical fact that distinguishes this reading from the bodily_autonomy_primary reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(herd_immunity_threshold_empirical, empirical, 'Whether herd immunity threshold is met by voluntary compliance').

omega_variable(
    alternative_mitigation_feasibility,
    'Do alternative protections (isolation of immunocompromised, targeted prophylaxis, rapid testing) provide equivalent protection to mandate-based herd immunity?',
    'Cost-effectiveness analysis of alternative strategies; epidemiological modeling of protection levels; real-world performance data from jurisdictions using alternative approaches.',
    'If alternatives are equivalent and feasible: mandate is extraction with minimal coordination function (reclassifies to Snare). If alternatives are inadequate: mandate is necessary coordination (Tangled Rope confirmed, this reading prevails).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_mitigation_feasibility, empirical, 'Whether alternative protections can substitute for mandate-based immunity').

omega_variable(
    enforcement_mechanism_proportionality,
    'Are the enforcement mechanisms (employment loss, school exclusion, fines) proportional to the public health necessity, or do they exceed what is needed to achieve herd immunity?',
    'Comparative analysis: what vaccination level is achieved at different enforcement intensity levels? Identify minimum coercion required to reach critical threshold. Compare to alternative enforcement mechanisms (incentives, restrictions on unvaccinated activities rather than employment).',
    'If current enforcement exceeds minimum necessary: suppression component is higher than justified by coordination need (reclassifies to Snare or elevated Tangled Rope). If enforcement is minimal and proportional: Tangled Rope confirmed with lower suppression value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_proportionality, empirical, 'Proportionality of enforcement mechanisms to public health necessity').

omega_variable(
    mandate_reading_vs_autonomy_reading_foreclosure,
    'Does the public_health_primary reading logically foreclose the bodily_autonomy_primary reading, or do they coexist as incommensurable frameworks?',
    'Logical analysis: Does the premise ''collective protection supersedes individual consent when voluntary compliance fails and vulnerable populations face lethal risk'' entail the denial of ''individual bodily autonomy is inviolable'' or do they describe different moral/legal priorities that can coexist in different frameworks?',
    'If foreclosed: only one reading can be held within a single moral framework (rare, requires categorical priority claim). If coexists: different communities/jurisdictions can rationally hold either reading depending on their priority axiom. This omega resolves the fundamental interpretive dispute over the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_reading_vs_autonomy_reading_foreclosure, conceptual, 'Logical relationship between public health necessity and bodily autonomy readings').

omega_variable(
    vulnerable_population_identification,
    'How are immunocompromised, unvaccinatable, and allergic populations identified and enumerated? Are they actual victims (facing lethal exposure) or potential victims (in abstract risk scenarios)?',
    'Epidemiological data on immunocompromised prevalence, documented vaccine adverse events, clinical guidelines for contraindications. Real-world disease mortality in these populations under different vaccination scenarios.',
    'If vulnerable populations are small, rare, or can be protected through targeted measures: mandate''s coordination function is weaker, extraction component becomes more salient (reclassifies toward Snare). If vulnerable populations are substantial and cannot be protected except through collective immunity: coordination function is strong, Tangled Rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vulnerable_population_identification, empirical, 'Actual vs. potential vulnerability of populations protected by mandate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_primary, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(public_health_theater_t0, public_health_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement(public_health_theater_t6, public_health_primary, theater_ratio, 6, 0.32).
narrative_ontology:measurement(public_health_theater_t12, public_health_primary, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(public_health_extract_t0, public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(public_health_extract_t6, public_health_primary, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(public_health_extract_t12, public_health_primary, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_primary, resource_allocation).
narrative_ontology:affects_constraint(public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_primary, proportionality_reading).
narrative_ontology:affects_constraint(public_health_primary, informed_consent_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the vaccine_mandate_balance kernel. The public_health_primary reading (this story) instantiates the necessity-based framework: mandates are justified when herd immunity is necessary and vulnerable populations face lethal risk. The bodily_autonomy_primary reading treats the same constraint as Snare from all victim perspectives, rejecting the coordination claim. The proportionality_reading accepts both but requires strict proof of necessity before engaging proportionality analysis. All three stories share the same base_extractiveness (0.58) and suppression (0.68) values — these are intrinsic properties of the mandate mechanism itself. The readings differ in how they classify the constraint type: this reading says Tangled Rope (genuine coordination + real extraction); bodily_autonomy_primary says Snare (extraction with false coordination claim); proportionality_reading says Tangled Rope (like this reading) but with higher emphasis on the empirical thresholds in omegas. Write all three stories with linked network entries. Do not merge them into one story with multiple classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_primary, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
