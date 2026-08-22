% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__supranational_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: Paris Agreement Article 4 NDC Regime — Supranational Ratchet Reading
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested Paris Agreement Article 4
 *   NDC kernel: the supranational reading, which holds that Nationally
 *   Determined Contributions constitute binding commitments on a legally
 *   enforceable ratcheting trajectory toward net-zero, backed by
 *   international accountability mechanisms with real reputational and
 *   financial teeth. Under this reading, the treaty's transparency framework,
 *   global stocktake, and finance-conditionality architecture function as an
 *   emergent compliance regime — not merely aspirational review. The sibling
 *   readings (sovereigntist: NDCs as voluntary self-determined pledges;
 *   equity: NDCs as necessarily differentiated by CBDR) are NOT part of this
 *   story; they are separate constraints with their own ε values, authored
 *   elsewhere and linked via network.affects_constraints. This reading's ε is
 *   high because, taken at its own word, the regime imposes real and rising
 *   extraction on carbon-dependent economies and workers through externally
 *   set targets they did not autonomously choose and cannot easily exit.
 *
 * KEY AGENTS:
 *   - unfccc_secretariat_and_compliance_committee: administers the ratchet and review architecture — agenda_setter
 *   - climate_vulnerable_coastal_states: primary beneficiary of binding accountability — bears no direct enforcement cost but existential exposure without it
 *   - fossil_fuel_dependent_economies: primary institutional target — bears sanction risk on a treaty-set decarbonization timetable
 *   - carbon_intensive_heavy_industry_workers: powerless payer, concentrated local cost, no negotiating voice
 *   - emerging_industrializing_states: payer with partial exclusion — bound by ratchet stringency calibrated without full deference to differentiated capacity
 *   - renewable_energy_industry_coalitions and international_climate_finance_institutions: mobile/institutional beneficiaries whose scope and budget expand with regime stringency
 *   - sovereigntist_dissenting_states: excluded voice — present in negotiation but interpretively overridden by this reading's own account of bindingness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.71).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.62).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Agreement Article 4 NDC Regime — Supranational Ratchet Reading").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, '2402315b-c32d-4ca7-b517-31bf7f54c658').
narrative_ontology:cs_kernel_codification('2402315b-c32d-4ca7-b517-31bf7f54c658', fixed_text).
narrative_ontology:cs_authority_grounding('2402315b-c32d-4ca7-b517-31bf7f54c658', extraction).
narrative_ontology:cs_interpretation_layer_present('2402315b-c32d-4ca7-b517-31bf7f54c658').
narrative_ontology:cs_reading_relation('2402315b-c32d-4ca7-b517-31bf7f54c658', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('2402315b-c32d-4ca7-b517-31bf7f54c658', paris_article_4_ndc__equity_reading, influences).
narrative_ontology:cs_axiom('2402315b-c32d-4ca7-b517-31bf7f54c658', foundational, ndc_substantive_content_is_internationally_binding).
narrative_ontology:cs_axiom_status(ndc_substantive_content_is_internationally_binding, holdable).
narrative_ontology:cs_axiom_grounding('2402315b-c32d-4ca7-b517-31bf7f54c658', ndc_substantive_content_is_internationally_binding, conventional).
narrative_ontology:cs_axiom('2402315b-c32d-4ca7-b517-31bf7f54c658', secondary, ratchet_trajectory_overrides_national_discretion_on_pace).
narrative_ontology:cs_axiom_status(ratchet_trajectory_overrides_national_discretion_on_pace, holdable).
narrative_ontology:cs_axiom_grounding('2402315b-c32d-4ca7-b517-31bf7f54c658', ratchet_trajectory_overrides_national_discretion_on_pace, instrumental).
narrative_ontology:cs_reference_frame('2402315b-c32d-4ca7-b517-31bf7f54c658', kyoto_binding_target_failure_baseline).
narrative_ontology:cs_drift_state('2402315b-c32d-4ca7-b517-31bf7f54c658', post_first_global_stocktake_2023, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('2402315b-c32d-4ca7-b517-31bf7f54c658', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_vulnerable_coastal_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, renewable_energy_industry_coalitions).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, international_climate_finance_institutions).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, compliance_verification_bodies).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_dependent_economies).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_heavy_industry_workers).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, emerging_industrializing_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, national_energy_ministries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the enhanced transparency framework, reviews NDC submissions against the ratchet mechanism, and issues findings on non-compliance. Has no direct enforcement power of its own but its findings feed into reputational sanction, market access conditions, and climate finance eligibility administered by other bodies. Frames the ratchet as the treaty's core legal obligation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, unfccc_secretariat_and_compliance_committee, agenda_setter,
    institutional, civilizational, analytical, global).

% Face existential physical risk from continued emissions and have organized diplomatically to demand a binding, ratcheting, internationally accountable regime, since a voluntary regime offers them no leverage over larger emitters. They benefit from every mechanism that raises the cost of non-compliance for major emitters but have little independent capacity to enforce it themselves.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_vulnerable_coastal_states, beneficiary,
    moderate, generational, trapped, global).

% States whose fiscal base, employment, and export revenue depend substantially on coal, oil, or gas extraction and export. Under the ratchet trajectory their core industries face a mandated wind-down on a treaty-set timetable, with reputational and financial exposure (credit downgrades, exclusion from green finance, carbon border measures) if they miss targets. Exiting the regime means diplomatic isolation and loss of access to climate finance and trade preferences tied to Paris alignment.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_dependent_economies, payer,
    organized, biographical, constrained, national).

% Workers in steel, cement, coal mining, and heavy manufacturing whose jobs are directly threatened by accelerated decarbonization timetables set at the international level, with no seat at the negotiating table and typically inadequate domestic transition programs. They bear concentrated, immediate costs from a treaty text they never ratified as individuals.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_heavy_industry_workers, payer,
    powerless, biographical, trapped, local).

% States in the early stages of industrialization who argue the ratchet mechanism locks them into decarbonization pathways calibrated to already-industrialized economies, foreclosing the fossil-fuel-intensive development pathway wealthy states used. Bound by the same accountability architecture as high-income emitters despite vastly different starting capacities, with limited voice in how the ratchet's stringency is calibrated.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, emerging_industrializing_states, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, emerging_industrializing_states, excluded).

% Domestic bureaucracies responsible for energy security and grid stability, which must reconcile internationally set ratchet targets with domestic supply constraints, often without matching international financing. Formal treaty accountability substitutes for negotiated domestic consensus on the pace of transition.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, national_energy_ministries, payer,
    institutional, biographical, constrained, national).

% Manufacturers, financiers, and developers of wind, solar, and grid-storage technology whose addressable market expands directly with the stringency of the ratchet mechanism. Mobile capital allows them to relocate to jurisdictions with the most favorable subsidy and mandate environments created by NDC compliance pressure.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, renewable_energy_industry_coalitions, beneficiary,
    organized, generational, arbitrage, global).

% Multilateral funds and development banks that administer North-to-South climate finance flows conditioned on NDC compliance and ratchet participation. Their institutional mandate and budget grow with the scope of the accountability architecture; they help define compliance benchmarks they are also funded to help states meet.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, international_climate_finance_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, international_climate_finance_institutions, agenda_setter).

% Technical review teams, auditors, and MRV (measurement, reporting, verification) contractors whose professional and institutional existence depends on the ratchet's demand for continuous international accountability reporting.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, compliance_verification_bodies, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, compliance_verification_bodies, observer).

% States and blocs (and the sovereigntist reading of the same treaty text) who hold that NDCs are self-determined and non-binding by design, and who object that the supranational reading imports enforcement machinery the treaty's negotiators explicitly declined to create. Their objection is a matter of ongoing legal and diplomatic dispute, not a settled minority view — they are present in the negotiating rooms but structurally outvoted in the interpretive contest over what 'binding' means under Article 4.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, sovereigntist_dissenting_states, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__supranational_reading, diffuse).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__supranational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: unilateral national mitigation is individually costly and collectively insufficient to avoid catastrophic warming, so an internationally verified, progressively tightening set of commitments is meant to prevent free-riding and lock in a credible aggregate trajectory toward net-zero.
% TRANSFER_FUNCTION: Moves industrial capacity, capital investment, and employment away from carbon-intensive sectors and states and toward renewable energy sectors and states; moves climate finance and technology transfer from wealthy historical emitters toward vulnerable and developing states, formalized through compliance-conditioned funding mechanisms.
% ABSENT_VOICES: Fossil-fuel-dependent workers and communities have no direct representation in treaty negotiation or in the technical bodies that set ratchet stringency; sovereigntist states are present but interpretively overridden in this reading's own account of what 'binding' means; emerging industrializing states participate but with asymmetric technical and legal capacity relative to the institutions setting compliance benchmarks.
% DISAPPEARANCE_RATIONALE: Climate-vulnerable states and the finance/verification institutions built around the ratchet mechanism argue the world reorganizes catastrophically without binding accountability — emissions trajectories would slacken and free-riding would resume. Fossil-fuel-dependent economies and sovereigntist states argue that removing the supranational enforcement layer would return the world to something close to the status quo of nationally determined climate policy shaped by domestic politics rather than treaty sanction, which they hold is what was actually negotiated.
% FOUNDING_PROBLEM: The Kyoto Protocol's top-down binding-target model collapsed due to non-ratification (by the US) and non-participation by major emerging emitters; Paris Article 4 was built to solve the participation problem by trading binding international targets for near-universal participation via self-determined pledges strengthened by a review-and-ratchet cycle.
% FOUNDING_PROBLEM_CORROBORATION: UNFCCC legal historians and several state delegations that participated in the Paris negotiations (including US and Indian delegation accounts) attest that the negotiating record shows NDCs were deliberately designed as nationally determined and non-binding in their content, with only the procedural obligations (reporting, ratchet cycle participation) treaty-binding — a reading that contradicts this constraint's own supranational characterization of the substantive targets as binding. This corroboration comes from outside the beneficiary set (vulnerable states, finance institutions, verification bodies) and represents the strongest outside challenge to the founding-problem narrative this reading asserts.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, contested).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__supranational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__supranational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.71 by the end of the interval because, under this reading's own terms, the ratchet mechanism imposes externally set, tightening obligations with real financial and reputational sanctions on states and sectors that did not consent to the pace or shape of those obligations, and the trajectory of extraction rises measurably as review cycles harden (2015 Paris adoption baseline through 2023 first global stocktake and beyond). Suppression is elevated (0.62) because non-compliance carries real, if diffuse, costs — carbon border adjustments, credit rating exposure, exclusion from green finance — and there is no clean formal exit from the accountability architecture short of full treaty withdrawal, which itself carries diplomatic and market costs. Theater ratio starts elevated (0.55 in 2015, when transparency mechanisms were largely aspirational) and falls over the interval (0.40 by 2035) as enhanced transparency framework reporting, the global stocktake cycle, and finance conditionality became progressively more operational rather than merely declaratory — the opposite of typical piton drift, reflecting genuine institutional maturation of enforcement capacity under this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate-vulnerable states and the finance/verification apparatus sit near the beneficiary end: the ratchet's stringency directly serves their interests (survival exposure reduction, institutional mandate growth) without symmetric cost exposure. Fossil-fuel economies, heavy industry workers, and emerging industrializing states sit near the target end: the ratchet imposes concentrated, rising costs on their core economic base with limited capacity to renegotiate the pace bilaterally. National energy ministries occupy an intermediate position — institutionally powerful but structurally constrained by having to reconcile externally set targets against domestic energy security without matching finance, which the derivation captures as constrained exit despite institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — non-universal participation under Kyoto's top-down binding model — is contested as either still-live (this reading's own claim: without binding ratchet accountability, free-riding resumes) or resolved-but-persisting (competing account: participation was solved by making the SUBSTANTIVE content voluntary, and this reading's supranational bindingness claim is an interpretive overreach beyond what was negotiated, evidenced by the corroborating negotiating-history testimony from outside the beneficiary set). This is precisely the founding_problem/disappearance_verdict mismatch pattern the R5 interview is designed to surface: if this reading's own account of bindingness is correct, removing the ratchet accountability structure would cause the world to rearrange (free-riding resumes); if the sovereigntist historical account is correct, this reading's classification of the current architecture as already-binding is itself the constructed claim, and no rearrangement would follow from acknowledging that the substantive targets were never binding in the first place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bindingness_of_substantive_ndc_content,
    'Are the substantive emissions-reduction targets within NDCs legally binding under international law, or only the procedural obligations to submit, report, and participate in the ratchet cycle?',
    'Authoritative international judicial or arbitral ruling interpreting Article 4''s legal character (e.g., ICJ advisory proceedings on climate obligations), or a definitive consensus reading emerging from state practice and treaty-body jurisprudence over subsequent review cycles.',
    'If only procedural obligations are binding, this reading''s core premise (binding ratchet trajectory) is a constructed overreach and the constraint''s high ε reflects contested rather than settled legal fact — the sovereigntist reading would be structurally vindicated rather than merely coexisting. If substantive bindingness is judicially confirmed, this reading''s ε is validated as descriptively accurate rather than aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bindingness_of_substantive_ndc_content, empirical, 'Whether NDC substantive content, not just procedure, carries binding legal force.').

omega_variable(
    kernel_reading_selection_evidence,
    'What evidence justified selecting the supranational reading as the frame for this story, given that the sovereigntist and equity readings are equally available interpretations of the identical treaty text?',
    'None — this is a framing choice inherent to the committer-axis structure of the kernel, not an empirically resolvable fact. Document as conceptual under-determination.',
    'Had the equity or sovereigntist reading been selected instead, this same underlying treaty architecture would classify with a substantially different ε and likely a different type (the sovereigntist reading trends toward scaffold or rope; the equity reading foregrounds differentiated-responsibility beneficiary structures this reading treats as a payer-side complication rather than a design principle). The choice of reading is not neutral and should be read as one committed interpretive stance among three live ones.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'The kernel admits at least three structurally distinct, non-averaging readings; this story commits to one.').

omega_variable(
    enforcement_capacity_vs_stated_bindingness,
    'Does the international accountability architecture (transparency framework, global stocktake, finance conditionality) actually possess sufficient enforcement teeth to make the ratchet trajectory function as genuinely binding in practice, or does formal bindingness claims outrun actual enforcement capacity?',
    'Track actual sanction/consequence rates against missed or weakened NDCs across successive stocktake cycles (2023, 2028, 2033); compare declared consequences to realized ones.',
    'If enforcement consistently fails to materialize despite the binding claim, the theater_ratio trajectory authored here (declining over time) would need revision upward — the falling theater_ratio assumes enforcement infrastructure genuinely matures rather than merely accumulating procedural apparatus without teeth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_stated_bindingness, empirical, 'Gap between claimed enforcement bindingness and observed sanction practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__supranational_reading, theater_ratio, 2015, 0.55).
narrative_ontology:measurement(pari_tr_t2019, paris_article_4_ndc__supranational_reading, theater_ratio, 2019, 0.5).
narrative_ontology:measurement(pari_tr_t2023, paris_article_4_ndc__supranational_reading, theater_ratio, 2023, 0.44).
narrative_ontology:measurement(pari_tr_t2027, paris_article_4_ndc__supranational_reading, theater_ratio, 2027, 0.42).
narrative_ontology:measurement_basis(pari_tr_t2027, projected).
narrative_ontology:measurement(pari_tr_t2031, paris_article_4_ndc__supranational_reading, theater_ratio, 2031, 0.41).
narrative_ontology:measurement_basis(pari_tr_t2031, projected).
narrative_ontology:measurement(pari_tr_t2035, paris_article_4_ndc__supranational_reading, theater_ratio, 2035, 0.4).
narrative_ontology:measurement_basis(pari_tr_t2035, projected).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__supranational_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(pari_be_t2019, paris_article_4_ndc__supranational_reading, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement(pari_be_t2023, paris_article_4_ndc__supranational_reading, base_extractiveness, 2023, 0.62).
narrative_ontology:measurement(pari_be_t2027, paris_article_4_ndc__supranational_reading, base_extractiveness, 2027, 0.67).
narrative_ontology:measurement_basis(pari_be_t2027, projected).
narrative_ontology:measurement(pari_be_t2031, paris_article_4_ndc__supranational_reading, base_extractiveness, 2031, 0.7).
narrative_ontology:measurement_basis(pari_be_t2031, projected).
narrative_ontology:measurement(pari_be_t2035, paris_article_4_ndc__supranational_reading, base_extractiveness, 2035, 0.71).
narrative_ontology:measurement_basis(pari_be_t2035, projected).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__supranational_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(pari_su_t2019, paris_article_4_ndc__supranational_reading, suppression_requirement, 2019, 0.45).
narrative_ontology:measurement(pari_su_t2023, paris_article_4_ndc__supranational_reading, suppression_requirement, 2023, 0.55).
narrative_ontology:measurement(pari_su_t2027, paris_article_4_ndc__supranational_reading, suppression_requirement, 2027, 0.59).
narrative_ontology:measurement_basis(pari_su_t2027, projected).
narrative_ontology:measurement(pari_su_t2031, paris_article_4_ndc__supranational_reading, suppression_requirement, 2031, 0.61).
narrative_ontology:measurement_basis(pari_su_t2031, projected).
narrative_ontology:measurement(pari_su_t2035, paris_article_4_ndc__supranational_reading, suppression_requirement, 2035, 0.62).
narrative_ontology:measurement_basis(pari_su_t2035, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__equity_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, carbon_border_adjustment_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, green_climate_fund_conditionality).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the paris_article_4_ndc kernel (supranational, sovereigntist, equity), each authored as a separate constraint story with its own ε, beneficiary/victim structure, and type per the ε-invariance principle. The supranational reading (this file) authors the highest ε of the three, reflecting binding-enforcement extraction; the sovereigntist reading is expected to author substantially lower ε (voluntary pledge framing); the equity reading restructures the beneficiary/victim map around CBDR differentiation rather than uniform ratchet compliance. All three should link to each other via affects_constraints, and none should be treated as an average or synthesis of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
