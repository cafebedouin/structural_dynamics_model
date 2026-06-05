% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Credential Requirements as Graduated Access Filter
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   Statutory credential requirements for licensed professions create a
 *   tiered labor market structure where access is distributed by ability to
 *   pay for credential acquisition rather than demonstrated competence. This
 *   constraint is one reading of the contested licensing_statute_mandate
 *   kernel: the graduated_access_filter reading interprets the statute's
 *   primary structural function as sorting workers by prior resource access
 *   (class, geography, family wealth) rather than exclusively protecting
 *   public safety. From the perspective of resource-constrained workers, the
 *   constraint functions as a snare: state-enforced barriers to entry with no
 *   alternative pathways and suppression maintained through prosecution of
 *   unlicensed practice and credential monopoly enforcement. From the
 *   perspective of credentialed incumbents, the same statute provides
 *   coordination benefits (skill verification, reputation protection, market
 *   stability) and functions as rope. The regulatory licensing board
 *   maintains performative review functions (exam design, continuing
 *   education requirements) with low functional safety correlation — theater
 *   ratio of 0.45 reflects that the apparatus is less theatrical than a full
 *   piton but more theatrical than a genuine coordination mechanism. The
 *   constraint exhibits extraction accumulation over the interval:
 *   extractiveness increases from 0.42 to 0.58 as credential acquisition
 *   costs inflate, suppression requirements increase to maintain the access
 *   barrier (enforcement machinery expanded), and theater ratio remains
 *   stable (performance ritual unchanged despite cost inflation). The
 *   analytical observer risks naturalizing this as an immutable natural law
 *   of labor market organization ('any complex service requires credential
 *   barriers'), which the engine will detect as a false summit: identifiable
 *   beneficiaries exist, suppression is state-enforced rather than natural,
 *   and alternative institutional arrangements (apprenticeship, portfolio
 *   licensing, tiered credentials) are structurally possible.
 *
 * KEY AGENTS:
 *   - Resource-Constrained Workers: Primary victim (powerless/trapped) — absolute barriers to entry without credential; no alternative pathways; experience maximum snare-level extraction
 *   - Aspiring Workers with Limited Resources: Secondary victim (moderate/constrained) — can theoretically acquire credential but face high cost barriers functioning as exclusion mechanism
 *   - Credentialed Incumbents (Professional Association): Primary beneficiary (institutional/arbitrage) — captures coordination benefits and market protection; experiences constraint as legitimate rope; lowest experienced extraction due to high agency
 *   - Credential-Granting Institution: Secondary beneficiary (institutional/arbitrage) — extracts rents through tuition inflation, reduced competition, and regulatory moat; mixed coordination and extraction (tangled rope)
 *   - Regulatory Licensing Board: Institutional actor (institutional/arbitrage) — maintains performative enforcement apparatus; sees own function as degraded (piton); captures legitimacy from statute despite low functional safety correlation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as inherent to functional labor markets; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.58).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.68).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.58).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Credential Requirements as Graduated Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, '9a1a026b-a358-40c8-8267-d9c40ffcdfff').
narrative_ontology:cs_kernel_codification('9a1a026b-a358-40c8-8267-d9c40ffcdfff', formalized).
narrative_ontology:cs_authority_grounding('9a1a026b-a358-40c8-8267-d9c40ffcdfff', extraction).
narrative_ontology:cs_interpretation_layer_present('9a1a026b-a358-40c8-8267-d9c40ffcdfff').
narrative_ontology:cs_reading_relation('9a1a026b-a358-40c8-8267-d9c40ffcdfff', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('9a1a026b-a358-40c8-8267-d9c40ffcdfff', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_axiom('9a1a026b-a358-40c8-8267-d9c40ffcdfff', foundational, credential_requirements_sort_by_prior_resource_access).
narrative_ontology:cs_axiom_status(credential_requirements_sort_by_prior_resource_access, holdable).
narrative_ontology:cs_axiom_grounding('9a1a026b-a358-40c8-8267-d9c40ffcdfff', credential_requirements_sort_by_prior_resource_access, empirically_contingent).
narrative_ontology:cs_axiom('9a1a026b-a358-40c8-8267-d9c40ffcdfff', foundational, credential_acquisition_cost_functions_as_market_exclusion_mechanism).
narrative_ontology:cs_axiom_status(credential_acquisition_cost_functions_as_market_exclusion_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('9a1a026b-a358-40c8-8267-d9c40ffcdfff', credential_acquisition_cost_functions_as_market_exclusion_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('9a1a026b-a358-40c8-8267-d9c40ffcdfff', credential_protection_for_competence).
narrative_ontology:cs_drift_state('9a1a026b-a358-40c8-8267-d9c40ffcdfff', contemporary_access_barrier_escalation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a1a026b-a358-40c8-8267-d9c40ffcdfff', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_incumbents).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credential_granting_institutions).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, resource_constrained_workers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_entry_cohort).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS WORKER (SNARE) — Faces absolute barrier to entry without credential. Cannot exit labor market entirely; must accept informal/unregulated alternatives or below-credential wages. No alternative pathways; credential monopoly is enforceable by state apparatus. Maximum experienced extraction — structural barriers are enforced through licensing statute, credential verification, and prosecution of unlicensed practice.
constraint_indexing:constraint_classification(licensing_statute_mandate__graduated_access_filter, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MODERATE WORKER (SNARE) — Can acquire credential but faces high cost barriers (tuition, exam fees, prerequisites, income forgone during training). Exit option (credential acquisition) is available but costly enough to function as exclusion. Experiences snare-level extraction through the cost-barrier mechanism. Suppression is sustained through credential gatekeeping and cost-inflation.
constraint_indexing:constraint_classification(licensing_statute_mandate__graduated_access_filter, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIALED INCUMBENTS (ROPE) — Captures coordination benefit from the credential statute: unified skill standard, reputation protection, market stability, reduced unlicensed competition. Experiences the constraint as legitimate coordination mechanism. Arbitrage option (can relocate license across jurisdictions, can establish credential standards) means low or negative experienced extraction. Beneficiary position with highest agency.
constraint_indexing:constraint_classification(licensing_statute_mandate__graduated_access_filter, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CREDENTIAL-GRANTING INSTITUTION (TANGLED ROPE) — Genuinely coordinates skill transmission and labor market matching. Simultaneously extracts rents through tuition inflation, reduced competition from alternative credentials, and regulatory moat (only certain institutions can grant recognized credentials). Benefits from state enforcement of credential monopoly. Mixed coordination and extraction with significant extraction component — institutional power and arbitrage exit options allow navigation of the constraint rather than entrapment.
constraint_indexing:constraint_classification(licensing_statute_mandate__graduated_access_filter, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY LICENSING BOARD (PITON) — Maintains credential verification and enforcement apparatus as ostensible public safety mechanism. Actual function has atrophied: board performance is largely performative (licensing exam validity disputed, continuing education requirements often theater). Persists through institutional inertia and credentialed incumbent capture. Theater ratio below full snare because the enforcement machinery is visible but degraded. Low functional coordination capacity despite formal mandate.
constraint_indexing:constraint_classification(licensing_statute_mandate__graduated_access_filter, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN - FALSE SUMMIT) — From a universal/civilizational perspective, market entry regulation might appear as an immutable natural law: 'any complex service requires verification of practitioner competence, ergo credential barriers are inherent to functional labor markets.' This framing naturalizes what is actually a contingent institutional choice (could be apprenticeship, peer review, portfolio-based entry, provisional licensing). The engine will classify this as false summit: identifiable beneficiaries exist (credentialed incumbents), suppression is enforced through state apparatus (not natural law), and alternative institutional arrangements are structurally possible.
constraint_indexing:constraint_classification(licensing_statute_mandate__graduated_access_filter, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(licensing_statute_mandate__graduated_access_filter, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(licensing_statute_mandate__graduated_access_filter, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, TR),
    TR >= 0.70.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting the magnitude of economic extraction through credential cost barriers and market access restriction. The value is not as extreme as 0.72+ (snare floor) because some credential incumbents genuinely value the coordination function and maintain safety standards; the extraction is not purely predatory. However, extractiveness exceeds 0.46 threshold, indicating that the extraction component dominates over coordination function from the primary target's perspective. Suppression (0.68): High. The state enforces credential monopoly through prosecution of unlicensed practice, credential verification requirements, exam gatekeeping, and reciprocal licensing restrictions across jurisdictions. Barriers are sustained through active enforcement machinery (licensing boards, disciplinary proceedings) rather than passive market forces. Theater ratio (0.45): Moderate-low. The licensing board maintains exam design and continuing education requirements, but these are not primarily performative — they reflect genuine (if contested) safety concerns. The theater is lower than a full piton (0.70+) because some functional safety review occurs, but the ratio indicates that performative maintenance exceeds functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits radical perspectival divergence. The resource-constrained worker sees an absolute snare — state-enforced exclusion with no exit option and economic extraction through credential cost barriers. The credentialed incumbent sees a legitimate rope — coordination mechanism that protects reputation and maintains skill standards without being experienced as extraction (arbitrage options and beneficiary status produce negative chi). The credential-granting institution sees tangled rope — genuine education provided alongside rent extraction through tuition inflation and competition suppression. The licensing board sees itself as performing a piton role — maintaining a degraded safety verification ritual. The analytical observer risks seeing a mountain — naturalizing labor market credential requirements as inherent to functional markets, a reading that the engine will flag as false summit because identifiable beneficiaries exist and alternative entry mechanisms are structurally viable. The perspectival gap is not mere disagreement; it reflects real structural differences in how the constraint functions for different agent types.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position relative to the extraction flow. Resource-constrained workers (powerless/trapped) experience high d (~0.95) because they are full targets of extraction with no exit options — the sigmoid function produces maximum f(d). Credentialed incumbents (institutional/arbitrage) experience low d (~0.10) because they are beneficiaries with high exit options (can relocate, establish alternative credentials, influence regulation) — f(d) produces negative effective extraction. The perspectival gap is wide: measured extractiveness chi is high (~0.68) for powerless targets but negative (~-0.15) for institutional beneficiaries, both derived from the same base extractiveness and suppression metrics. Moderately-resourced workers (moderate/constrained) experience medium d (~0.65), producing moderate-to-high chi. The credential-granting institution experiences mixed extraction (tangled rope) because it benefits from the regulatory moat while simultaneously funding genuine skill transmission — d is intermediate (~0.35), producing moderate positive chi. The licensing board experiences low d (institutional/arbitrage position) but piton classification derives from theater ratio, not from chi magnitude.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the six types represent legitimate structural readings of different agent positions within the same institutional arrangement. The snare classification (resource-constrained workers) is not 'the correct answer' but rather the perspective of agents bearing the extraction. The rope classification (credentialed incumbents) is their genuine experience of coordination benefits. The tangled rope (institutions) reflects the actual mixture of educational service and rent extraction. The piton (regulatory board) is the degraded performance of safety verification. The mountain (analytical observer) is the false-summit framing that must be rejected because the evidence reveals contingent institutional choices rather than natural law. The mandatrophy resolves not by choosing one type but by recognizing that the constraint's structural form instantiates all types simultaneously from different positions. What appears as 'coordination' from the incumbent's perspective appears as 'extraction' from the excluded worker's perspective, and both are correct observations of the same constraint's function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_validity_measurement,
    'Does the licensing credential actually predict practitioner competence and public safety outcomes, or does it primarily function as an exclusion mechanism?',
    'Comparative analysis: outcome data (client/patient safety, complaint rates, disciplinary actions) for credentialed vs alternative-entry practitioners; correlation analysis between credential standards and measured public safety improvements',
    'If credential predicts safety: classification shifts toward Rope/Tangled Rope (genuine coordination function). If correlation is weak or absent: classification confirms Snare (credential is pure extraction disguised as safety).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_validity_measurement, empirical, 'Whether licensing credential predicts competence and safety outcomes').

omega_variable(
    alternative_entry_viability,
    'Are structurally viable alternative entry mechanisms available (apprenticeship, portfolio-based licensing, tiered credentials, reciprocal recognition across jurisdictions) that would maintain public safety while reducing access barriers?',
    'Comparative institutional analysis: evidence from jurisdictions with alternative entry pathways; outcome data from hybrid/apprenticeship models; analysis of barriers to adoption in current regime',
    'If alternatives are viable: suppression is choice-based (regulatory stance) rather than structural inevitability — snare classification confirmed. If alternatives fail safety tests: suppression reflects genuine complexity — classification shifts toward Tangled Rope or legitimate Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_entry_viability, empirical, 'Viability of alternative entry mechanisms').

omega_variable(
    cost_inflation_structural_vs_intentional,
    'Is credential acquisition cost inflation (tuition, exam fees, prerequisite programs) a structural consequence of credential maintenance or an intentional extraction mechanism designed to limit access?',
    'Historical cost analysis: comparison of credential acquisition cost to actual education/verification cost; analysis of cost increases independent of instruction quality or safety standard increases; comparative analysis of cost across jurisdictions with similar safety standards',
    'If structural: cost barrier is incidental to coordination. If intentional: cost barrier is primary extraction mechanism — strengthens Snare classification and identifies rent-seeking suppression as the organizing principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_inflation_structural_vs_intentional, empirical, 'Cost inflation as structural vs intentional mechanism').

omega_variable(
    reading_contestation_public_safety_vs_access_filter,
    'Is this statute best read as primarily protecting public safety (public_safety_coordination kernel reading) or as primarily filtering market access by class/resource status (graduated_access_filter reading instantiated here)?',
    'Discourse analysis: legislative record, regulatory guidance, enforcement priority documentation; outcome comparison with public safety mandate; analysis of protected vs excluded populations for correlation with resource access patterns',
    'If public safety is primary: beneficiary set differs (public as victim, credentialed as incidental), suppression has different justification. If access filter is primary: current Snare classification confirmed. Both readings coexist in policy debate — this reading formalizes one pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_public_safety_vs_access_filter, conceptual, 'Kernel reading contestation: public safety coordination vs access filter').

omega_variable(
    intersectional_access_barrier_magnitude,
    'What proportion of excluded populations face barriers due to resource constraints vs discrimination, spatial accessibility, prior educational gaps, or credential transfer blockages?',
    'Demographic analysis of credentialed vs excluded populations; analysis of barrier composition by exclusion mechanism (cost, discrimination, geographic, educational prerequisite, jurisdictional); comparative analysis across protected classes',
    'If majority is resource-constrained: suppression primarily economic (class-based sorting). If significant proportion discriminatory/geographic/jurisdictional: suppression has compound mechanisms requiring separate constraints for each barrier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersectional_access_barrier_magnitude, empirical, 'Intersectional composition of access barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lic_grad_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.38).
narrative_ontology:measurement(lic_grad_tr_t10, licensing_statute_mandate__graduated_access_filter, theater_ratio, 10, 0.42).
narrative_ontology:measurement(lic_grad_tr_t20, licensing_statute_mandate__graduated_access_filter, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(lic_grad_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lic_grad_be_t10, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(lic_grad_be_t20, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lic_grad_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lic_grad_su_t10, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(lic_grad_su_t20, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, enforcement_mechanism).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__rent_seeking_suppression).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, credential_acquisition_cost_barrier).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, apprenticeship_pathway_suppression).

% DUAL FORMULATION NOTE:
% The licensing statute manifests as three structurally distinct constraints depending on reading and observable: public_safety_coordination (statute protects public through competence standards — ε~0.25), rent_seeking_suppression (statute benefits credentialed incumbents through market restriction — ε~0.70), and graduated_access_filter (this constraint — statute sorts workers by resource access — ε~0.58). All three operate simultaneously; this story formalizes the access-filter reading. Decomposition follows from ε-invariance principle: changing which mechanism (safety, extraction, or access-filtering) is treated as primary changes ε and classification, indicating structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
