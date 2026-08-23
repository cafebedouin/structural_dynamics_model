% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__precautionary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-15
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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Precautionary Legitimacy Criterion for Climate Mitigation Technologies
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The precautionary reading of the technology legitimacy kernel asserts
 *   that climate mitigation technologies must pass a reversibility test:
 *   their worst-case failure modes and legacy costs must be bounded and
 *   reversible within a human generation (~30 years). This reading
 *   structurally privileges renewables and storage (decommissioning is
 *   technically straightforward, land recovers) while excluding nuclear
 *   (high-level waste requires 100,000+ year stewardship, catastrophic
 *   accident risk is unbounded) and constraining fossil+CCS (CO2 leakage
 *   risk, pipeline network legacy). The constraint operates as a tangled
 *   rope: it performs a genuine coordination function (intergenerational risk
 *   allocation, preventing sacrificial technologies) while extracting from
 *   incumbent industries and industrial users who lose technological options.
 *   Active enforcement is required — the criterion must be instantiated in
 *   policy, permitting, and finance rules to exclude non-compliant
 *   technologies. The measurement series tracks the criterion's rising
 *   extractiveness and theater as climate urgency intensifies and the
 *   excluded technologies lobby for inclusion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, 0.68).
domain_priors:suppression_score(technology_legitimacy_kernel__precautionary_reading, 0.55).
domain_priors:theater_ratio(technology_legitimacy_kernel__precautionary_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__precautionary_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__precautionary_reading, "Precautionary Legitimacy Criterion for Climate Mitigation Technologies").
narrative_ontology:topic_domain(technology_legitimacy_kernel__precautionary_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__precautionary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__precautionary_reading, '00a77fa1-59f0-447b-a653-f10980cad772').
narrative_ontology:cs_kernel_codification('00a77fa1-59f0-447b-a653-f10980cad772', distributed).
narrative_ontology:cs_authority_grounding('00a77fa1-59f0-447b-a653-f10980cad772', distributed).
narrative_ontology:cs_reading_relation('00a77fa1-59f0-447b-a653-f10980cad772', technology_legitimacy_kernel__reliability_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('00a77fa1-59f0-447b-a653-f10980cad772', technology_legitimacy_kernel__velocity_primacy_reading, influences).
narrative_ontology:cs_axiom('00a77fa1-59f0-447b-a653-f10980cad772', foundational, intergenerational_non_imposition).
narrative_ontology:cs_axiom_status(intergenerational_non_imposition, holdable).
narrative_ontology:cs_axiom_grounding('00a77fa1-59f0-447b-a653-f10980cad772', intergenerational_non_imposition, deontological).
narrative_ontology:cs_axiom('00a77fa1-59f0-447b-a653-f10980cad772', foundational, reversibility_as_legitimacy_condition).
narrative_ontology:cs_axiom_status(reversibility_as_legitimacy_condition, holdable).
narrative_ontology:cs_axiom_grounding('00a77fa1-59f0-447b-a653-f10980cad772', reversibility_as_legitimacy_condition, deontological).
narrative_ontology:cs_reference_frame('00a77fa1-59f0-447b-a653-f10980cad772', pre_nuclear_waste_crisis_era).
narrative_ontology:cs_drift_state('00a77fa1-59f0-447b-a653-f10980cad772', contemporary_climate_urgency_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('00a77fa1-59f0-447b-a653-f10980cad772', '2026-07-15T14:22:00Z').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, future_generations).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, environmental_justice_communities).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, insurance_reinsurance_sector).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, nuclear_energy_industry).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, fossil_fuel_infrastructure_owners).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, industrial_energy_intensive_users).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, geological_repository_hosts).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, intergenerational_equity_principle).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, precautionary_principle_environmental_law).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, reversibility_as_legitimacy_condition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop wind, solar, and storage projects whose decommissioning is technically reversible and whose legacy costs are bounded within a generation. They gain policy preference, financing advantages, and market access under the precautionary criterion. Exit means shifting to other clean tech or jurisdictions; their capital is redeployable.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, global).

% Bear the irreversible costs of technologies chosen today — nuclear waste stewardship, carbon cycle disruption, contaminated landscapes. They have no voice in present decisions and no exit from the consequences. The criterion structurally protects them by excluding technologies whose harms they would inherit without consent or recourse.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(technology_legitimacy_kernel__precautionary_reading, future_generations).

% Historically burdened by siting of extractive and waste infrastructure. The criterion favors technologies whose harms do not concentrate in their neighborhoods and whose decommissioning restores land. They gain procedural leverage in permitting but remain constrained by systemic underrepresentation in energy governance.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, environmental_justice_communities, beneficiary,
    moderate, generational, constrained, regional).

% Underwrites long-tail risks of energy infrastructure. Technologies with bounded, reversible failure modes are insurable at calculable premiums; nuclear and carbon capture carry unquantifiable tail risks. The sector benefits from a criterion that aligns legitimacy with insurability, reducing unhedgeable exposure.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, insurance_reinsurance_sector, beneficiary,
    institutional, biographical, arbitrage, global).

% Provides dispatchable low-carbon generation but produces high-level waste requiring geological isolation for 100,000+ years and carries catastrophic accident risk (Fukushima, Chernobyl). Under the precautionary criterion, nuclear is structurally illegitimate — its legacy costs are unbounded and irreversible on generational timescales. The industry advocates for the criterion's rejection, wielding regulatory capture and baseload narratives to maintain legitimacy.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_energy_industry, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, nuclear_energy_industry, agenda_setter).

% Own assets (pipelines, plants, reserves) whose legacy costs — stranded assets, decommissioning liabilities, climate externalities — are unbounded and irreversible. The criterion accelerates their devaluation. They resist through political lobbying, capture of transition narratives, and funding of reliability_primacy_reading advocates.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, fossil_fuel_infrastructure_owners, payer,
    institutional, biographical, constrained, global).

% Require high-reliability, high-density energy for processes (steel, cement, chemicals). They perceive the criterion as threatening grid reliability and cost competitiveness by excluding nuclear and constraining fossil backup. Their exit is limited: relocation faces carbon border adjustments; process electrification is capital-intensive and slow.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, industrial_energy_intensive_users, payer,
    organized, immediate, constrained, national).

% Communities and jurisdictions hosting or candidate for nuclear waste repositories (e.g., Olkiluoto, Yucca Mountain, Gorleben). They bear the localized, irreversible burden of waste stewardship across millennia. Their identity is often fused with the repository project — economic dependence, technical workforce, civic narrative — making exit psychologically and politically identity_locked.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, geological_repository_hosts, payer,
    moderate, generational, identity_locked, local).

% Design mitigation portfolios, carbon budgets, and technology eligibility rules (IPCC, UNFCCC, national ministries). They set the legitimacy criterion by choosing which reading governs. Under the precautionary reading, they exclude nuclear and constrain fossil CCS; under rival readings, they include them. Their situation is the arena where the kernel's readings contest.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, climate_policy_architects, agenda_setter,
    institutional, generational, mobile, global).

% Model grid integration, system costs, and risk profiles across technology portfolios. They evaluate the criterion's claims: does reversibility correlate with lower system risk? Does exclusion of nuclear raise total mitigation cost or delay? They hold no stake in the outcome but their analyses are weaponized by all sides.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, energy_systems_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates intergenerational risk allocation by establishing a legitimacy filter that prevents technologies whose failure modes exceed the remedial capacity of the deploying generation. Solves the coordination problem of how to choose mitigation technologies without imposing unbounded liabilities on those who cannot consent.
% TRANSFER_FUNCTION: Transfers the option value of high-density, dispatchable generation (nuclear, fossil+CCS) from present industrial users and infrastructure owners to future generations and vulnerable communities, in the form of avoided irreversible legacy costs. The transfer is not monetary but risk-allocation: present actors forgo certain technological options; future actors avoid certain harms.
% ABSENT_VOICES: Future generations (structurally excluded by non-existence). Communities in the Global South who would host waste or bear climate impacts disproportionately but lack representation in OECD-dominated technology governance. Indigenous nations whose consent is required for repository siting but whose sovereignty is overridden by national energy security claims.
% DISAPPEARANCE_RATIONALE: If the precautionary criterion vanished overnight, nuclear and fossil+CCS would re-enter legitimacy contests on equal footing with renewables. Investment would shift toward technologies with higher power density but unbounded tail risks. Waste repository programs would accelerate. The intergenerational risk allocation would revert to de facto 'present generation decides, future generations bear' — a structural rearrangement of the mitigation portfolio and its liability distribution.
% FOUNDING_PROBLEM: The founding problem is the historical pattern of energy technologies (coal, oil, nuclear) being deployed at scale before their waste and failure modes were understood, leaving successor generations with irreversible contamination, climate disruption, and stewardship burdens they did not choose and cannot escape.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: (1) Nuclear waste programs in 30+ countries with no operating geological repository after 60 years of effort (IAEA, NEA reports). (2) Climate science establishing carbon cycle irreversibility on millennial timescales (IPCC AR6). (3) Environmental justice literature documenting sacrificial zones from energy extraction/waste (Bullard, Schlosberg). (4) Insurance industry withdrawal from nuclear liability coverage (Price-Anderson Act renewal debates). No corroboration comes from nuclear industry or fossil fuel incumbents, who contest the problem's continued relevance given 'improved designs' and 'managed adaptation'.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__precautionary_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__precautionary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__precautionary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(technology_legitimacy_kernel__precautionary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__precautionary_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the criterion's asymmetric impact: it transfers option value from powerful incumbents (nuclear, fossil) to diffuse beneficiaries (future generations, vulnerable communities) who cannot reciprocate. Suppression (0.55) is moderate — the criterion excludes technologies but does not physically prevent their deployment; enforcement is through policy/finance gates. Theater (0.42) is significant: the criterion's coordination justification (protecting future generations) is real, but a growing share of advocacy performs precaution without addressing the grid reliability and deployment velocity trade-offs the excluded technologies claim to solve. Accessibility collapse (0.62) is high — once the reversibility frame is accepted, alternatives (nuclear, CCS) appear structurally illegitimate rather than merely costly. Resistance (0.58) is substantial from institutional incumbents with generational time horizons.
 *
 * PERSPECTIVAL GAP:
 *   From the renewable developer seat, the criterion is a rope — genuine coordination with net benefit. From the nuclear industry seat, it is a snare — extraction disguised as principle, suppressing their technology. From the future generations seat, it is a mountain — the only barrier between them and irreversible harm. From the industrial user seat, it is a scaffold — a temporary constraint that should sunset when storage solves reliability. The engine computes these divergences from the structural data; the single authored claim (tangled_rope) reflects the generator's judgment that the constraint's coordination function is real but its extraction is asymmetric and enforced.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: renewable developers (organized, mobile exit) collect policy rents; future generations (powerless, trapped) receive risk protection; environmental justice communities (moderate, constrained) gain procedural leverage; insurance sector (institutional, arbitrage) gains insurability. Payers: nuclear industry (institutional, constrained — regulatory capture mitigates exit); fossil infrastructure owners (institutional, constrained — stranded asset risk); industrial users (organized, constrained — process lock-in); repository hosts (moderate, identity_locked — civic/economic fusion with the burden). Agenda-setter: climate policy architects (institutional, mobile) choose the reading. Observer: analysts (analytical). Directionality d is low for beneficiaries (subsidized by constraint), high for payers (extracted from), symmetric-ish for agenda-setter (sets rules, bears implementation cost).
 *
 * MANDATROPHY ANALYSIS:
 *   The criterion's founding problem (intergenerational risk from unbounded energy externalities) remains live — waste programs still fail, climate irreversibility accelerates. Yet the criterion's enforcement increasingly serves as a veto for nuclear without a parallel build-out of the storage/transmission that would make renewables a full substitute. This is mandatrophy in the coordination channel: the legitimacy filter operates, but the complementary investments it requires are underfunded. The theater ratio rise (0.1→0.42) tracks this — more performance of precaution, less delivery of the reliable zero-carbon system the coordination function promises.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_boundary_definition,
    'What counts as ''reversible within a generation''? Does solar panel recycling at 90% after 30 years qualify? Does nuclear waste in dry cask storage (retrievable for 100 years) qualify? The boundary is contestable and changes the beneficiary/victim sets.',
    'Technical standard-setting (ISO, IAEA) for decommissioning metrics; legal precedent on ''reversibility'' in environmental law; empirical tracking of actual decommissioning outcomes.',
    'A narrow boundary (only technologies with demonstrated full-site restoration in <30 years) excludes most current renewables (blade waste, critical mineral extraction scars). A broad boundary (retrievable/contained waste) admits nuclear dry cask storage. The classification shifts from tangled_rope toward snare or rope depending on boundary placement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_boundary_definition, conceptual, 'Operational definition of the criterion''s core threshold').

omega_variable(
    coordination_extraction_separability,
    'Is the precautionary criterion''s coordination function (intergenerational risk allocation) separable from its extraction function (transferring option value from nuclear/fossil to renewables)? Or does the criterion structurally require the extraction to achieve the coordination?',
    'Counterfactual policy modeling: simulate a regime with the coordination function (liability funds, waste bonds, intergenerational trusts) but without technology exclusions. If risk allocation works without exclusions, extraction is separable.',
    'If separable, the current criterion is a tangled_rope with unnecessary extraction — a purer rope exists. If inseparable, the extraction is the price of coordination, and the tangled_rope classification is structurally necessary. Affects mandatrophy assessment: is the theater in the exclusion or in the missing complementary policies?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s two functions can be decoupled').

omega_variable(
    kernel_reading_foreclosure,
    'Does the precautionary reading''s foundational axiom (intergenerational_non_imposition) logically foreclose the reliability_primacy_reading within a single governance framework, or do they coexist as competing legitimate positions?',
    'Constitutional/legal analysis: can a single jurisdiction simultaneously hold ''no unbounded legacy costs'' and ''baseload reliability is paramount'' as binding principles? Historical test: jurisdictions attempting both (Germany Energiewende, Sweden nuclear phase-out/reversal).',
    'If forecloses, the kernel has a structural fracture — one reading must displace the other. If coexists_with, the kernel sustains permanent contestation and the constraint family persists. If influences, the precautionary reading raises the burden of proof for reliability claims without eliminating them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between this reading and the reliability_primacy_reading').

omega_variable(
    identity_locked_repository_hosts,
    'Are geological repository host communities genuinely identity_locked (civic identity fused with the repository project), or is this a mischaracterization of economic dependence that would dissolve with just transition investment?',
    'Longitudinal social science in repository host regions (Östhammar, Olkiluoto, Gorleben): track identity narratives vs. economic diversification outcomes. Compare communities that rejected repositories vs. those that accepted.',
    'If identity_locked, their payer seat has d≈1.0 (full target) and the constraint''s suppression is experienced as existential. If economic dependence, exit_options=constrained and d<1.0 — the extraction is severe but not identity-fused. Changes the seat''s computed type and the constraint''s overall directional profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_repository_hosts, empirical, 'Nature of exit constraint for waste repository hosts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__precautionary_reading, 1970, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tlk_prec_tr_t1970, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(tlk_prec_tr_t1986, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 1986, 0.18).
narrative_ontology:measurement(tlk_prec_tr_t1997, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 1997, 0.25).
narrative_ontology:measurement(tlk_prec_tr_t2011, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2011, 0.32).
narrative_ontology:measurement(tlk_prec_tr_t2015, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(tlk_prec_tr_t2020, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(tlk_prec_tr_t2030, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2030, 0.4).
narrative_ontology:measurement(tlk_prec_tr_t2040, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2040, 0.41).
narrative_ontology:measurement(tlk_prec_tr_t2050, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2050, 0.42).

% Extraction over time
narrative_ontology:measurement(tlk_prec_be_t1970, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(tlk_prec_be_t1986, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 1986, 0.35).
narrative_ontology:measurement(tlk_prec_be_t1997, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 1997, 0.42).
narrative_ontology:measurement(tlk_prec_be_t2011, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2011, 0.55).
narrative_ontology:measurement(tlk_prec_be_t2015, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(tlk_prec_be_t2020, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(tlk_prec_be_t2030, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2030, 0.65).
narrative_ontology:measurement(tlk_prec_be_t2040, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2040, 0.67).
narrative_ontology:measurement(tlk_prec_be_t2050, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2050, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tlk_prec_su_t1970, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(tlk_prec_su_t1986, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 1986, 0.4).
narrative_ontology:measurement(tlk_prec_su_t1997, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 1997, 0.45).
narrative_ontology:measurement(tlk_prec_su_t2011, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2011, 0.5).
narrative_ontology:measurement(tlk_prec_su_t2015, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(tlk_prec_su_t2020, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2020, 0.53).
narrative_ontology:measurement(tlk_prec_su_t2030, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2030, 0.54).
narrative_ontology:measurement(tlk_prec_su_t2040, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2040, 0.55).
narrative_ontology:measurement(tlk_prec_su_t2050, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2050, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__precautionary_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__precautionary_reading, 0.08).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, nuclear_waste_governance).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, renewable_deployment_mandates).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, carbon_budget_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the technology_legitimacy_kernel. The kernel decomposes into three structurally distinct constraints with different ε values: precautionary (ε=0.68, tangled_rope), reliability_primacy (ε≈0.45, rope/snare depending on grid context), velocity_primacy (ε≈0.6, tangled_rope). They share the referent 'what counts as legitimate mitigation technology' but differ in the structural relationship they impose between present and future generations. The precautionary reading is the only one that makes reversibility the gate; the others make it a weighted factor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__precautionary_reading, institutional, 0.35).
constraint_indexing:directionality_override(technology_legitimacy_kernel__precautionary_reading, organized, 0.55).
constraint_indexing:directionality_override(technology_legitimacy_kernel__precautionary_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
