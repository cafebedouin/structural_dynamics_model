% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__facility_constraint_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: caa_section_111d_delegation__facility_constraint_reading
 *   human_readable: CAA Section 111(d) Facility-Constraint Reading
 *   domain: administrative_law/environmental_regulation
 *
 * SUMMARY:
 *   This constraint is one reading of the contested Section 111(d) delegation
 *   kernel. The facility-constraint reading holds that the Clean Air Act's
 *   phrase 'best system of emission reduction' is limited to measures that
 *   can be applied at and achieved by individual existing
 *   sourcesâprincipally heat-rate improvements and on-site carbon capture.
 *   It forecloses EPA from mandating or crediting generation shifting,
 *   renewable substitution, or early coal retirement. The reading was
 *   consolidated in West Virginia v. EPA and structurally protects coal asset
 *   values and state energy-mix autonomy while imposing a regulatory ceiling
 *   on federal climate action. It is claimed as a federalism coordination
 *   mechanism; the metrics independently describe its extractive operation.
 *
 * KEY AGENTS:
 *   - coal_plant_operators: Primary beneficiary (powerful/constrained) â protected from accelerated retirement and generation-shifting mandates
 *   - state_utility_regulators: Secondary beneficiary (institutional/constrained) â retain autonomy over resource planning and energy mix
 *   - federal_judiciary: Agenda-setter (institutional/constrained) â interprets and enforces the statutory boundary through judicial review
 *   - environmental_protection_agency: Primary payer/target (institutional/constrained) â authority curtailed to fenceline measures
 *   - climate_advocacy_orgs: Payer (organized/constrained) â regulatory ceiling prevents federal climate action through 111(d)
 *   - renewable_energy_developers: Payer (moderate/constrained) â blocked from federal compliance market for existing source displacement
 *   - frontline_communities: Payer (powerless/trapped) â bear ongoing pollution as regulatory acceleration is barred
 *   - administrative_law_scholars: Analytical observer (analytical/analytical) â evaluate textual and structural claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.72).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.78).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "CAA Section 111(d) Facility-Constraint Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, 'a22ed7e6-c2b8-45de-866f-2071c840f4ea').
narrative_ontology:cs_kernel_codification('a22ed7e6-c2b8-45de-866f-2071c840f4ea', fixed_text).
narrative_ontology:cs_authority_grounding('a22ed7e6-c2b8-45de-866f-2071c840f4ea', lineage).
narrative_ontology:cs_interpretation_layer_present('a22ed7e6-c2b8-45de-866f-2071c840f4ea').
narrative_ontology:cs_reading_relation('a22ed7e6-c2b8-45de-866f-2071c840f4ea', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('a22ed7e6-c2b8-45de-866f-2071c840f4ea', foundational, bsr_fenceline_limited).
narrative_ontology:cs_axiom_status(bsr_fenceline_limited, holdable).
narrative_ontology:cs_axiom_grounding('a22ed7e6-c2b8-45de-866f-2071c840f4ea', bsr_fenceline_limited, conventional).
narrative_ontology:cs_axiom('a22ed7e6-c2b8-45de-866f-2071c840f4ea', foundational, generation_shifting_exceeds_statutory_authority).
narrative_ontology:cs_axiom_status(generation_shifting_exceeds_statutory_authority, holdable).
narrative_ontology:cs_axiom_grounding('a22ed7e6-c2b8-45de-866f-2071c840f4ea', generation_shifting_exceeds_statutory_authority, conventional).
narrative_ontology:cs_reference_frame('a22ed7e6-c2b8-45de-866f-2071c840f4ea', fenceline_performance_standard).
narrative_ontology:cs_drift_state('a22ed7e6-c2b8-45de-866f-2071c840f4ea', post_west_virginia_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a22ed7e6-c2b8-45de-866f-2071c840f4ea', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_plant_operators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, state_utility_regulators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, environmental_protection_agency).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_advocacy_orgs).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, frontline_communities).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, textualism_statutory_interpretation).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, major_questions_doctrine).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, state_energy_mix_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own fossil-fuel generation assets that would face accelerated retirement under a systemic transformation rule. Under this reading, they are subject only to heat-rate improvements or on-site carbon capture, preserving plant operating lives, capacity revenues, and asset values that would otherwise be stranded by generation-shifting mandates.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_plant_operators, beneficiary,
    powerful, biographical, constrained, national).

% Retain primary authority over resource planning, integrated resource plans, and energy mix decisions without federal preemption via EPA-mandated generation shifting or portfolio standards. The reading provides a judicial ceiling that protects state regulatory turf.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, state_utility_regulators, beneficiary,
    institutional, generational, constrained, national).

% Interprets and enforces the statutory boundary of Section 111(d) through judicial review of EPA rules. Under this reading, courts hold that the best system of emission reduction is limited to measures implementable at and achievable by individual stationary sources, invalidating rules that mandate or credit generation shifting.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Statutorily responsible for reducing power-sector greenhouse gas emissions but judicially barred from requiring generation shifting, renewable substitution, or early retirement. Must design regulations using only heat-rate improvements and on-site carbon capture, which are more costly per ton abated and less effective, curtailing its authority under the statute.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, environmental_protection_agency, payer,
    institutional, generational, constrained, national).

% Seek rapid power-sector decarbonization through federal regulation. Under this reading, the most cost-effective emission reduction pathways are legally unavailable, forcing acceptance of a slower transition or a strategic shift to state-level and private-pressure campaigns.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_advocacy_orgs, payer,
    organized, generational, constrained, national).

% Would benefit from a regulatory framework that credits or mandates zero-carbon generation displacement of coal-fired output. Under this reading, EPA cannot treat their product as a compliance mechanism for existing coal sources, limiting market expansion driven by federal environmental rules.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers, payer,
    moderate, biographical, constrained, national).

% Live near coal-fired power plants and bear ongoing particulate and criteria pollutant exposure. The regulatory ceiling prevents federal rules from accelerating plant retirement, reducing utilization, or inducing substitution that would lower local air pollution faster than fenceline heat-rate improvements allow.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, frontline_communities, payer,
    powerless, generational, trapped, local).

% Analyze the statutory text, legislative history, and constitutional structure of Section 111(d) to assess whether the facility-constraint reading is legally compelled by the text or a policy choice dressed in textualist and major-questions reasoning.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, administrative_law_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a federalism boundary between national ambient environmental regulation and state resource planning authority, preventing EPA from using air quality statutes to reorganize the national electricity generation mix and protecting state utility commissions from federal preemption.
% TRANSFER_FUNCTION: Transfers regulatory authority and asset protection from federal climate regulation to coal plant operators and state utility regulators, while transferring the health and climate costs of continued coal operation to frontline communities, renewable developers, and climate advocates.
% ABSENT_VOICES: Future generations and non-US populations affected by incremental climate change are not represented in the litigation or rulemaking record. Congressional drafters of the 1970 and 1990 CAA amendments are absent, and the statutory text is ambiguous on the scope of best system. Low-income communities downwind of coal plants are often underrepresented in the utility regulatory proceedings that interact with EPA compliance.
% DISAPPEARANCE_RATIONALE: If the facility-constraint reading vanished, EPA would redesign power plant greenhouse gas rules around generation-shifting, portfolio standards, and crediting mechanisms; coal retirements would accelerate, renewable deployment would expand, state regulators would lose their protective ceiling against federal energy-mix mandates, and electricity sector emission trajectories would shift downward more rapidly.
% FOUNDING_PROBLEM: How to regulate emissions from existing stationary sources under Section 111(d) without freezing technology, imposing impossible retrofit burdens, or disrupting existing state resource planning, while allowing states flexibility to implement performance standards.
% FOUNDING_PROBLEM_CORROBORATION: Coal industry and state regulators attest the problem is live and that federalism protections remain necessary. EPA and environmental historians attest the statute was intended to allow flexible, cost-effective compliance including fuel switching and generation shifting, and that the fence-line reading is a modern judicial invention unsupported by legislative history. Independent textual scholars and administrative law experts are split on the question.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the reading caps regulatory ambition at the facility fence line, foreclosing lower-cost abatement and imposing higher social and climate costs on communities and advocates. Suppression (0.78) is high because judicial enforcement eliminates EPA's alternative design pathways and binds subsequent rulemaking; alternatives do not merely face friction but are structurally barred by interpretation. Theater_ratio (0.30) reflects moderate performative maintenance: the textualist reasoning is a genuine interpretive framework, but an increasing share of its defense consists in major-questions rhetoric and federalism tropes that perform restraint rather than analyze statutory meaning. Accessibility_collapse (0.65) captures the narrowing of EPA's design space once the reading is adopted; states retain alternatives, but the federal floor is capped. Resistance (0.80) is high because EPA, climate advocates, renewable developers, and public health groups actively contest the reading in litigation, legislation, and academic criticism.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (EPA, frontline communities, climate advocates, renewable developers) experience this constraint as a judicially enforced extraction of regulatory potential and health protection, converting a statutory delegation into a protective ceiling for fossil capital. The beneficiary seats (coal operators, state regulators) experience it as a necessary federalism boundary that prevents federal overreach into state resource planning. The agenda-setter seat (the federal judiciary) experiences it as a compelled textual interpretation. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Coal plant operators and state utility regulators are structural beneficiaries: they collect asset protection and jurisdictional autonomy, yielding low directionality and damped effective extraction. The EPA is an institutional payer, structurally targeted by the judicial constraint on its statutory authority. Frontline communities are powerless and trapped, placing them at the high end of effective extraction. Climate advocacy organizations and renewable developers are organized and moderate payers, respectively, with constrained exit options, placing them in the mid-to-high target range. No override is needed because beneficiary and victim declarations plus exit options correctly map the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by preserving its genuine coordination function: it protects state utility regulators from federal preemption and provides a predictable regulatory boundary. Without that coordination component, the constraint would read as a pure snare for coal interests. However, the asymmetric cost distributionâconcentrated benefits to coal operators and diffuse, severe costs to frontline communities and the climateâplaces it in tangled_rope rather than rope. The mandatrophy question is whether the original technology-based regulation problem has atrophied into a fossil-capital protection device; the contested founding_problem_status and the presence of real federalism beneficiaries keep it in the hybrid category rather than piton or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_ambiguity_bsr_scope,
    'Does the statutory phrase ''best system of emission reduction'' inherently denote only fenceline technologies, or is it structurally ambiguous between facility-level and system-wide measures?',
    'Historical legislative history analysis, contemporaneous statutory usage surveys, and comparison with other Clean Air Act provisions employing ''system'' language.',
    'If the text is genuinely ambiguous, the facility reading is a judicial policy choice rather than textual exegesis, increasing its extractiveness and moving it toward a snare classification. If the text is unambiguously fenceline, the coordination story strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_ambiguity_bsr_scope, conceptual, 'Whether the statutory kernel is ambiguous or compels the facility reading.').

omega_variable(
    major_questions_stability,
    'Does the facility-constraint reading depend on the Major Questions Doctrine, and will it survive shifts in judicial composition or interpretive methodology?',
    'Longitudinal study of MQD application across statutory domains and Court configurations; tracking of judicial confirmation effects on environmental statutory interpretation.',
    'If MQD-dependent, the constraint''s persistence is political rather than strictly legal, increasing theater_ratio and raising the risk of piton degradation if the doctrine loses support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(major_questions_stability, empirical, 'Stability of the reading''s legal grounding under doctrinal change.').

omega_variable(
    social_cost_of_fence_line,
    'Does limiting compliance to heat-rate improvements and CCS at individual coal plants impose a higher marginal social cost than generation-shifting, and does that cost fall disproportionately on frontline communities?',
    'Integrated assessment models comparing per-ton abatement costs, air quality co-benefits, and mortality impacts of alternative 111(d) designs across demographic groups.',
    'If fence-line compliance is substantially more costly and concentrates health harms in disadvantaged communities, the coordination story weakens and the constraint''s extractiveness is higher than the base metric suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_cost_of_fence_line, empirical, 'Comparative social cost and distributional equity of the facility-only compliance pathway.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(caa__tr_t4, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(caa__tr_t8, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(caa__tr_t12, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(caa__tr_t16, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(caa__tr_t20, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(caa__be_t4, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(caa__be_t8, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(caa__be_t12, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(caa__be_t16, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(caa__be_t20, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(caa__su_t4, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(caa__su_t8, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(caa__su_t12, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(caa__su_t16, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(caa__su_t20, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation__systemic_transformation_reading).

% DUAL FORMULATION NOTE:
% This story and its sibling are two readings of the Section 111(d) 'best system' kernel. They share the same statutory text but instantiate different constraints with different epsilon values, beneficiary/victim structures, and legal effects. The facility reading constrains EPA to fenceline measures; the systemic reading authorizes generation-shifting. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
