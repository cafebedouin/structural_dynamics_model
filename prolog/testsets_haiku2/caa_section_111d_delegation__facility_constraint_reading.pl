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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: CAA Section 111(d) 'Best System' Limited to Facility-Level Measures (Facility Constraint Reading)
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   Section 111(d) of the Clean Air Act gives EPA authority to set
 *   performance standards for stationary sources (power plants). The
 *   statute's text — 'the best system of emission reduction' — is ambiguous:
 *   it could mean the best pollution-control technology at a single plant
 *   (facility constraint), or the best energy/emissions system achievable
 *   through fuel switching, retirement, and grid transformation (systemic
 *   transformation). This is ONE reading — the facility-constraint reading —
 *   which limits EPA to facility-level measures (heat-rate improvements,
 *   carbon capture, efficiency retrofits) and preserves state authority over
 *   fuel mix and generation retirement. Under this reading, EPA cannot
 *   mandate the coal sector's phase-out or force renewable substitution. Coal
 *   operators and state regulators benefit; climate advocates are foreclosed
 *   from using 111(d) as a lever for systemic decarbonization and must pursue
 *   their goals through state renewable mandates or federal legislation. The
 *   reading is CLAIMED as tangled rope (coordination of federalism with air
 *   quality + asymmetric extraction from climate advocates) and the authored
 *   metrics describe high extraction and suppression, with rising extraction
 *   over the interval as courts solidified the facility-constraint
 *   interpretation and climate advocates' regulatory alternatives narrowed.
 *
 * KEY AGENTS:
 *   - EPA Air Quality Office: interprets and enforces 111(d) under the facility-constraint reading; cannot mandate generation-shifting.
 *   - Coal-fired power operators: primary beneficiaries; avoid forced retirement and fuel-switching mandates; comply through unit-level retrofits.
 *   - State energy regulators: secondary beneficiaries; retain authority over energy mix and dispatch; EPA cannot override state choices.
 *   - Climate advocates and environmental groups: primary victims; excluded from using 111(d) as a climate lever; must pursue goals through alternative channels (state law, federal legislation, litigation on alternative readings).
 *   - Renewable energy sector: secondary victims; cannot rely on EPA-mandated generation-shifting to accelerate coal retirement; must compete on price and state mandates alone.
 *   - Federal courts: observers; hold authority over statutory interpretation and can overturn this reading.
 *   - Congress: observers; could amend the statute to resolve the ambiguity but has not done so.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.68).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.72).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "CAA Section 111(d) 'Best System' Limited to Facility-Level Measures (Facility Constraint Reading)").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, '7aa52f34-f060-4cd4-946f-402fadb4d6ac').
narrative_ontology:cs_kernel_codification('7aa52f34-f060-4cd4-946f-402fadb4d6ac', formalized).
narrative_ontology:cs_authority_grounding('7aa52f34-f060-4cd4-946f-402fadb4d6ac', lineage).
narrative_ontology:cs_interpretation_layer_present('7aa52f34-f060-4cd4-946f-402fadb4d6ac').
narrative_ontology:cs_reading_relation('7aa52f34-f060-4cd4-946f-402fadb4d6ac', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('7aa52f34-f060-4cd4-946f-402fadb4d6ac', foundational, regulatory_limits_doctrine).
narrative_ontology:cs_axiom_status(regulatory_limits_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('7aa52f34-f060-4cd4-946f-402fadb4d6ac', regulatory_limits_doctrine, deontological).
narrative_ontology:cs_axiom('7aa52f34-f060-4cd4-946f-402fadb4d6ac', foundational, federalism_deference_energy_authority).
narrative_ontology:cs_axiom_status(federalism_deference_energy_authority, holdable).
narrative_ontology:cs_axiom_grounding('7aa52f34-f060-4cd4-946f-402fadb4d6ac', federalism_deference_energy_authority, deontological).
narrative_ontology:cs_reference_frame('7aa52f34-f060-4cd4-946f-402fadb4d6ac', statutory_agency_boundary_preservation).
narrative_ontology:cs_drift_state('7aa52f34-f060-4cd4-946f-402fadb4d6ac', post_west_virginia_v_epa_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7aa52f34-f060-4cd4-946f-402fadb4d6ac', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_fired_power_operators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, state_energy_regulators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_advocates_and_environmental_groups).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_sector).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, coal_fired_power_operators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers_and_utilities).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, regulatory_limits_doctrine).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, agency_statutory_fidelity).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, federalism_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces Section 111(d) of the Clean Air Act. Under this reading, must limit the 'best system of emission reduction' to measures implementable at the generating unit itself: heat-rate improvements, carbon capture, efficiency retrofits. Cannot mandate generation-shifting (fuel substitution, retirement of coal units, dispatch changes across the grid). Administers state plan review and litigation defense.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, epa_air_quality_office, agenda_setter,
    institutional, generational, analytical, national).

% Operate existing coal-fired generating units. Under this facility-constraint reading, are not forced to retire units or switch fuels; face only retrofit/efficiency requirements on their own units. Can comply through capital expenditure on their assets rather than operational restructuring. Regulatory exposure is capped at unit-level technology deployment; the reading protects coal as a fuel choice at the system level. Pay compliance costs (retrofits, carbon capture) but avoid existential threat of forced retirement.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_fired_power_operators, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, coal_fired_power_operators, payer).

% Retain authority to set state energy mix (fossil/renewable portfolio) and dispatch rules. EPA cannot override that authority via generation-shifting mandates in 111(d) plans. State regulatory autonomy over resource adequacy, grid reliability, and fuel diversity are preserved under this reading. States coordinate compliance as rule-followers, not as subordinate implementers of a national energy transition.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, state_energy_regulators, beneficiary,
    institutional, generational, constrained, regional).

% Seek to reduce economy-wide carbon emissions and phase out coal generation as part of climate mitigation. Under this reading, cannot use Section 111(d) as a lever to force retirement of coal units or mandate renewable substitution at the system level. Exit options are legislative advocacy (pursue Clean Energy Standard, carbon tax, or statutory amendment) or continued litigation on alternative 111(d) readings — both costly and uncertain. Their preferred regulatory outcome is foreclosed by this interpretation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_advocates_and_environmental_groups, payer,
    moderate, generational, constrained, national).

% Develop and deploy renewable capacity, often competing with coal for grid dispatch and revenue. Under this facility-constraint reading, cannot rely on EPA-mandated generation-shifting to accelerate coal retirement or open market share. Must compete on price/performance alone, without regulatory support for fuel switching. Their growth depends on state-level renewable mandates and market dynamics, not on 111(d) enforcement of a systemic transition.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers_and_utilities, payer,
    powerful, biographical, mobile, national).

% Represent coal plant workers and unionized utilities. Have interests in preserving coal jobs and slowing coal retirement (shared with operators), but are not formally in the regulatory conversation around 111(d) interpretation. Their voice on job transition or managed decline is absent from the three-party negotiation (EPA, states, operators/advocates).
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, power_sector_unions, excluded,
    organized, biographical, constrained, national).

% Adjudicate disputes over the meaning of Section 111(d) 'best system'. This reading — facility constraint — is one interpretation the courts have entertained and (in the West Virginia v. EPA line) have validated. Courts hold the final authority over statutory construction and agency deference doctrine, and can overturn this reading if they find an alternative reading more faithful to the statute.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% Enacted the Clean Air Act and Section 111(d). Could amend the statute to mandate generation-shifting or to lock in the facility-constraint reading. Currently, Congress has taken no action to resolve the ambiguity; the constraint persists because legislative resolution has not occurred and courts have sustained the facility-constraint reading.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, congress, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__facility_constraint_reading, coal_fired_power_operators).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__facility_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state air quality planning with federal baseline standards: EPA sets a minimum performance bar for coal plants (emissions per MWh); states design compliance plans to achieve that bar using their chosen measures. The coordination solves the federalism puzzle — states lead, EPA validates — without EPA dictating the energy mix.
% TRANSFER_FUNCTION: Transfers regulatory authority and compliance flexibility from EPA to state regulators and from federal climate advocates to coal operators and state legislatures. Climate advocates must pursue their goals through state-level renewable mandates or federal legislation rather than through EPA's Section 111(d) authority. Coal operators and coal-dependent states retain discretion over fuel choice and retirement pace.
% ABSENT_VOICES: Coal plant workers and union leadership are structurally excluded from the formal 111(d) interpretation dispute (three-party contest: EPA, states, and coal operators); their interests in job transition and managed decline are not articulated in the regulatory record. Labor's perspective on the constraints would question whether the facility-level scope adequately addresses workforce disruption.
% DISAPPEARANCE_RATIONALE: If this facility-constraint reading were replaced overnight by a systemic-transformation reading (EPA could mandate generation-shifting and early retirement), the entire U.S. energy grid would reorganize: coal units would face forced retirement timelines, renewable investment would surge, state energy planning would subordinate to federal EPA authority, and the regulatory floor for coal would collapse. The reading structures state autonomy and coal sector preservation; its removal would trigger massive sectoral reorganization.
% FOUNDING_PROBLEM: The Clean Air Act's structure distinguishes stationary source standards (111(b), (d)) from ambient air standards (NAAQS). Section 111(d) was designed to give EPA a tool to set performance standards for plants that had not yet triggered NAAQS violations. The founding problem is: how do you set an emissions reduction target that is both ambitious enough to improve air quality AND stays within the EPA's statutory authority as a regulator of industrial processes (not energy policy)? The facility-constraint reading interprets 'best system' as EPA's outer boundary: the best cleanup a plant can do on its own unit, not the best energy system the nation could choose.
% FOUNDING_PROBLEM_CORROBORATION: EPA air quality engineers attest the facility-constraint interpretation matches the original statutory purpose (state-led planning with federal performance floors). Coal operators and energy economists argue this reading preserves the Clean Air Act's federalist structure and legislative intent not to displace FERC and state regulators from energy mix decisions. Climate scientists and environmental advocates attest the problem has *evolved*: air quality in most of the nation has improved, but climate change — a global, not local air quality problem — now requires systemic transformation beyond what facility-level improvements can achieve. Congressional testimony on legislative history (1970, 1990) supports both readings, reflecting statutory ambiguity.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.48 to 0.68 over the interval because the facility-constraint reading, once established by court precedent (West Virginia v. EPA, 2022), became the effective law of the land and climate advocates' preferred outcome (systemic transformation via 111(d)) receded from plausible. The reading's benefit to coal operators and states is asymmetric with its cost to climate advocates: operators gain a regulatory floor; advocates lose a regulatory tool. Suppression rises from 0.55 to 0.72 as the legal doctrine hardens and litigation on alternative readings becomes more costly and less likely to succeed. Theater is moderate (0.41 at interval end) because EPA continues to enforce facility-level measures and courts publish opinions citing statutory text, creating surface legitimacy; but a growing share of the enforcement apparatus defends coal sector preservation rather than air quality improvement. The accessibility collapse (0.61) reflects that climate advocates still have legislative and state-level pathways open, but the federal regulatory path through 111(d) has substantially closed. Resistance is high (0.74) because climate advocates and renewables advocates continue to litigate alternative readings and advocate for Congressional amendments; the constraint faces real opposition, not acquiescence. All measurements are on a shared time grid (t=0 to t=25, likely 2009–2034, marking the post-Obama-111(d)-rule through post-West-Virginia-v.-EPA period).
 *
 * PERSPECTIVAL GAP:
 *   From EPA's seat (agency_setter), the facility-constraint reading is faithful to statutory text and respects federalist limits on agency authority — the arrangement is the correct boundary between EPA air quality work and state/FERC energy regulation. From coal operators' seat, the reading is a genuine coordination of air quality compliance with operational flexibility — they can meet emissions targets through technology deployment without existential threat. From climate advocates' seat, the reading is regulatory ceiling that forecloses the most powerful federal tool for decarbonization and forces them into state-by-state advocacy. The engine computes each seat's experience from the structural data: EPA and coal operators see coordination + low/moderate extraction (they set or benefit); climate advocates see high extraction (they lose regulatory access). The authored claim (tangled rope) reflects the reading's structure: real coordination of federalism + air quality, coupled with asymmetric extraction from climate advocates forced into more costly alternative pathways.
 *
 * DIRECTIONALITY LOGIC:
 *   Coal operators have high d (near 1.0, full target) despite being beneficiaries, because they face extraction in the form of compliance costs (heat-rate improvements, carbon capture retrofits); but they also benefit by avoiding forced retirement. The directionality reflects the beneficiary-payer duality in their role (secondary_role='payer'). State regulators have moderate d (0.4-0.5) because they benefit from preserved authority but also face compliance management costs. Climate advocates have high d (near 0.9) because they bear the cost of foreclosed regulatory strategy and face constrained exit (must pursue alternatives through slower state/federal channels). Renewable energy developers have moderate d (0.5-0.6) because they lose the regulatory acceleration that systemic transformation would provide but retain mobile exit via state markets. The beneficiary/victim declarations map: beneficiaries (coal operators, state regulators) drive d downward for those seats; victims (climate advocates, renewables) drive d upward. Exit options modulate: coal operators' 'constrained' exit keeps d high despite beneficiary status (they cannot simply leave coal generation); climate advocates' 'constrained' exit (must operate through alternative channels) keeps their d high; renewable developers' 'mobile' exit (can pursue state markets) moderates their d downward from full target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('how do you set emissions standards that improve air quality without displacing EPA's authority into energy policy?') was live when Section 111(d) was enacted (1970s/1990) and remains contested today. The facility-constraint reading answers that problem by preserving the boundary: EPA sets unit-level performance standards, states set energy policy. Climate advocates contest this answer, arguing the problem has *evolved* and now climate change (not just local air quality) demands systemic transformation within EPA's scope. The constraint does NOT resolve mandatrophy (founding problem is live and contested); instead, the reading embeds the statutory ambiguity in a particular interpretive choice that benefits coal interests and state autonomy while harming climate interests. If the founding problem is reframed as 'how do you decarbonize the power sector,' then the facility-constraint reading actively contradicts the new problem and becomes a mandatrophic artifact (regulatory ceiling preventing the solution). The tangled-rope classification prevents mischaracterization: this is not pure extraction dressed as coordination (snare) because the federalism coordination is genuine; but the coordination serves coal's interests, not decarbonization, and the beneficiaries/victims structure reveals asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_ambiguity_best_system,
    'Does ''best system of emission reduction'' in Section 111(d) refer to the best pollution-control technology deployable at a single facility, or the best system (energy system, grid configuration, fuel mix) the nation could adopt to reduce emissions?',
    'Statutory textual analysis and legislative history review; Supreme Court interpretation of Chevron deference and ambiguity in administrative statute; Congressional amendment or clarification of the statute''s scope.',
    'Facility-constraint reading = EPA has no authority to mandate generation-shifting, fuel switching, or retirement of units; systemic-transformation reading = EPA can mandate these changes as part of the ''best system''. The two readings are mutually exclusive within any single statutory interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_ambiguity_best_system, conceptual, 'Core interpretive disagreement over the scope of ''best system'' in CAA 111(d).').

omega_variable(
    agency_statutory_authority_boundary,
    'Where does EPA''s authority under Section 111(d) end and FERC''s/state regulatory authority over energy markets begin?',
    'Jurisdictional delineation by the courts; Congressional clarification of EPA''s delegation and FERC/state roles; administrative coordination agreements between agencies (unlikely to resolve substantively).',
    'The facility-constraint reading preserves the boundary by keeping EPA out of fuel-switching decisions; the systemic-transformation reading relocates the boundary by treating generation-shifting as an ''emission reduction'' measure within EPA''s scope. This is a structural disagreement about agency roles, not just technical interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(agency_statutory_authority_boundary, conceptual, 'Jurisdictional boundary between EPA air quality authority and energy market regulation.').

omega_variable(
    climate_change_vs_air_quality_mandate_scope,
    'Can Section 111(d), enacted to address local air pollution from stationary sources, legitimately be read to authorize global climate change mitigation through systemic energy transformation?',
    'Legislative history and statutory purpose analysis; climate science integration into regulatory interpretation; Congressional amendment to clarify EPA''s climate authority or enact a separate climate statute.',
    'Facility-constraint reading = 111(d) is an air quality statute, not a climate statute; systemic-transformation reading = the same statute can authorize climate action because emissions from power plants contribute to climate change. The readings diverge on statutory purpose and permissible scope expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_change_vs_air_quality_mandate_scope, conceptual, 'Whether Section 111(d) air quality mandate extends to systemic climate mitigation.').

omega_variable(
    coal_extraction_victim_or_constituent,
    'Is the climate advocates'' displacement under this reading a form of extraction — they are forced to bear the cost of regulatory ceiling imposed for coal''s benefit — or a neutral reallocation where two legitimate policy claims (climate action vs. federalism/coal preservation) are weighed and one prevails?',
    'Empirical assessment of the constraint''s distributional impact: does it concentrate benefits (coal operators, state autonomy) while diffusing costs (climate mitigation delay, renewable sector growth suppression)? Normative framing: is coal sector preservation a ''vindicated proposition'' (legitimate public value) or a beneficiary claiming protection?',
    'If extraction: the constraint is snare-like (high suppression, active enforcement against alternatives, identifiable victims). If reallocation: the constraint is tangled-rope (genuine coordination of federalism with air quality goals, with winners and losers). The classification hinges on whether the reading''s beneficiaries are treated as legitimate policy actors or as rent-seeking operators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coal_extraction_victim_or_constituent, preference, 'Whether climate advocates'' displacement is extractive or a valid policy tradeoff.').

omega_variable(
    alternative_readings_foreclosure,
    'Does the facility-constraint reading logically foreclose the systemic-transformation reading, or do both remain live interpretations that different parties advocate for?',
    'Textual and structural analysis: can both readings coexist in a single statutory framework, or does accepting one require rejecting the other''s core premise? The answer determines whether the readings are forecloses-pair or coexists_with-pair.',
    'If forecloses: the reading is a direct defeat of the alternative, and the alternative constraint story should carry a cs_structure.reading_relations entry of ''forecloses''. If coexists_with: both remain live and the contest is political, not logical. This omega resolves the reading_relations value authoring decision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_foreclosure, conceptual, 'Logical compatibility of facility-constraint and systemic-transformation readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa_111d_facility_tr_t0, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(caa_111d_facility_tr_t0, observed).
narrative_ontology:measurement(caa_111d_facility_tr_t5, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(caa_111d_facility_tr_t5, observed).
narrative_ontology:measurement(caa_111d_facility_tr_t10, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(caa_111d_facility_tr_t10, observed).
narrative_ontology:measurement(caa_111d_facility_tr_t15, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(caa_111d_facility_tr_t15, observed).
narrative_ontology:measurement(caa_111d_facility_tr_t20, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(caa_111d_facility_tr_t20, observed).
narrative_ontology:measurement(caa_111d_facility_tr_t25, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(caa_111d_facility_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(caa_111d_facility_be_t0, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(caa_111d_facility_be_t0, observed).
narrative_ontology:measurement(caa_111d_facility_be_t5, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(caa_111d_facility_be_t5, observed).
narrative_ontology:measurement(caa_111d_facility_be_t10, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(caa_111d_facility_be_t10, observed).
narrative_ontology:measurement(caa_111d_facility_be_t15, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(caa_111d_facility_be_t15, observed).
narrative_ontology:measurement(caa_111d_facility_be_t20, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(caa_111d_facility_be_t20, observed).
narrative_ontology:measurement(caa_111d_facility_be_t25, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(caa_111d_facility_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(caa_111d_facility_su_t0, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(caa_111d_facility_su_t0, observed).
narrative_ontology:measurement(caa_111d_facility_su_t5, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(caa_111d_facility_su_t5, observed).
narrative_ontology:measurement(caa_111d_facility_su_t10, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(caa_111d_facility_su_t10, observed).
narrative_ontology:measurement(caa_111d_facility_su_t15, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(caa_111d_facility_su_t15, observed).
narrative_ontology:measurement(caa_111d_facility_su_t20, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(caa_111d_facility_su_t20, observed).
narrative_ontology:measurement(caa_111d_facility_su_t25, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(caa_111d_facility_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(caa_section_111d_delegation__facility_constraint_reading, 0.12).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation__systemic_transformation_reading).

% DUAL FORMULATION NOTE:
% CAA Section 111(d) 'best system' constraint family (two readings of a single contested kernel, caa_section_111d_delegation). This story (facility_constraint_reading) and the systemic_transformation_reading are mutually exclusive interpretations of the same statutory language. The facility-constraint reading limits EPA to facility-level measures (heat-rate, carbon capture) and preserves coal and state autonomy; the systemic-transformation reading authorizes generation-shifting and fuel switching. Their ε values differ substantially (this reading: moderate-to-high extraction from climate advocates; sibling reading: lower extraction, higher decarbonization alignment). Both are live in contemporary regulatory discourse. The network edge runs one direction only: this reading (currently Court-validated per West Virginia v. EPA) influences the sibling reading's plausibility (makes it harder to defend in courts, but still pursued by advocates).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__facility_constraint_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
