% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
 *   human_readable: CAA Section 111(d) Facility-Constraint Reading
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint story captures the 'facility constraint reading' of CAA
 *   Section 111(d) — the interpretation that 'best system of emission
 *   reduction' is limited to measures implementable at individual stationary
 *   sources (heat-rate improvements, carbon capture at the plant level). This
 *   reading emerged in litigation against the Clean Power Plan (2015) and was
 *   ultimately endorsed by the Supreme Court in West Virginia v. EPA (2022).
 *   The constraint operates as a ceiling on EPA authority: it coordinates
 *   regulatory certainty and preserves state autonomy over energy mix
 *   (coordination function) while structurally protecting the coal fleet from
 *   forced retirement and extracting the cost of foregone systemic mitigation
 *   from climate advocates and environmental justice communities (asymmetric
 *   extraction). The claimed type is tangled_rope because both functions are
 *   genuinely present: the reading provides a stable interpretive anchor for
 *   regulated entities and states, but its persistence depends on active
 *   judicial enforcement and it concentrates benefits on coal interests while
 *   imposing costs on climate mitigation.
 *
 * KEY AGENTS:
 *   - coal_fleet_operators: Primary beneficiary (powerful/constrained) — avoids forced retirement, captures regulatory certainty
 *   - state_energy_autonomy_advocates: Primary beneficiary (institutional/constrained) — preserves state authority over generation mix
 *   - climate_advocates: Primary victim (organized/trapped) — bears cost of regulatory ceiling on systemic mitigation
 *   - environmental_justice_communities: Secondary victim (powerless/trapped) — disproportionate pollution burden continues
 *   - federal_courts: Agenda setter (institutional/analytical) — enforces the interpretive limit through major questions doctrine
 *   - epa: Secondary agenda setter (institutional/constrained) — implements within the ceiling, self-censors systemic proposals
 *   - future_generations_interest: Excluded (powerless/trapped) — no voice in current doctrinal framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.68).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.82).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "CAA Section 111(d) Facility-Constraint Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, '3f3afa05-3c2d-46ec-a403-c9845bcddb3e').
narrative_ontology:cs_kernel_codification('3f3afa05-3c2d-46ec-a403-c9845bcddb3e', formalized).
narrative_ontology:cs_authority_grounding('3f3afa05-3c2d-46ec-a403-c9845bcddb3e', extraction).
narrative_ontology:cs_interpretation_layer_present('3f3afa05-3c2d-46ec-a403-c9845bcddb3e').
narrative_ontology:cs_reading_relation('3f3afa05-3c2d-46ec-a403-c9845bcddb3e', caa_section_111d_delegation__systemic_transformation_reading, forecloses).
narrative_ontology:cs_axiom('3f3afa05-3c2d-46ec-a403-c9845bcddb3e', foundational, section_111d_best_system_limited_to_facility_measures).
narrative_ontology:cs_axiom_status(section_111d_best_system_limited_to_facility_measures, holdable).
narrative_ontology:cs_axiom_grounding('3f3afa05-3c2d-46ec-a403-c9845bcddb3e', section_111d_best_system_limited_to_facility_measures, conventional).
narrative_ontology:cs_axiom('3f3afa05-3c2d-46ec-a403-c9845bcddb3e', secondary, major_questions_doctrine_bars_generation_shifting).
narrative_ontology:cs_axiom_status(major_questions_doctrine_bars_generation_shifting, holdable).
narrative_ontology:cs_axiom_grounding('3f3afa05-3c2d-46ec-a403-c9845bcddb3e', major_questions_doctrine_bars_generation_shifting, conventional).
narrative_ontology:cs_reference_frame('3f3afa05-3c2d-46ec-a403-c9845bcddb3e', textualist_cooperative_federalism_framework).
narrative_ontology:cs_drift_state('3f3afa05-3c2d-46ec-a403-c9845bcddb3e', post_west_virginia_v_epa, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('3f3afa05-3c2d-46ec-a403-c9845bcddb3e', '2026-01-15T14:30:00Z').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_fleet_operators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, state_energy_autonomy_advocates).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, regulated_utility_shareholders).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_advocates).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, environmental_justice_communities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, future_generations_interest).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, epa).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, statutory_textualism_in_environmental_law).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, major_questions_doctrine_application).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__facility_constraint_reading, cooperative_federalism_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and operate coal-fired power plants. The facility constraint reading protects them from EPA-mandated generation shifting or early retirement. They comply with heat-rate improvement requirements but avoid the existential threat of systemic regulation. Exit means retiring assets early or investing in CCS — both costly. They lobby and litigate to maintain this reading.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_fleet_operators, beneficiary,
    powerful, biographical, constrained, national).

% State governments (particularly coal-dependent and Republican-led) that invoke cooperative federalism to preserve authority over electricity generation mix. They benefit from the constraint's limitation on EPA because it prevents federal displacement of state energy policy. They can exit by adopting their own climate policies (some do), but the constraint protects their option not to.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, state_energy_autonomy_advocates, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, state_energy_autonomy_advocates, agenda_setter).

% Investors in regulated utilities with coal assets. The constraint provides regulatory certainty — they know EPA cannot force systemic changes that would strand assets. They can arbitrage by shifting capital to gas/renewables in other jurisdictions, but the constraint protects existing coal rate base.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, regulated_utility_shareholders, beneficiary,
    organized, biographical, arbitrage, national).

% Environmental NGOs, climate policy advocates, and their congressional allies. They bear the extraction: the facility constraint reading makes adequate federal mitigation legally impossible, forcing them into slower, piecemeal pathways (state policy, voluntary markets, innovation). They cannot exit the constraint — it is binding federal law until Court reversal or statutory amendment. Their resistance is high (litigation, advocacy, electoral pressure) but structurally blocked.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_advocates, payer,
    organized, generational, trapped, global).

% Fenceline communities disproportionately burdened by coal plant pollution. The facility constraint reading prolongs coal operation, continuing local health harms while foreclosing the federal lever that could force cleanup or retirement. They have no meaningful exit — relocation is economically impossible, and they lack political power to change the doctrine.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, environmental_justice_communities, payer,
    powerless, biographical, trapped, local).

% The federal judiciary, culminating in the Supreme Court, that enforces the facility constraint reading through the major questions doctrine and textualist interpretation. They set the agenda by defining the legal boundary. They benefit from the interpretive authority the constraint affirms. Their exit is analytical — they could adopt the systemic reading in a future case, but stare decisis and doctrinal commitment constrain that.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% The agency tasked with implementing Section 111(d). It both administers the constraint (writing facility-level standards) and is constrained by it (barred from systemic approaches). It pays a bureaucratic cost: its expertise favors systemic mitigation, but it must self-censor to survive judicial review. Exit means either non-acquiescence (risking reversal) or waiting for political change.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, epa, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, epa, payer).

% The intergenerational interest in a stable climate. Not represented in current standing doctrine. Bears the ultimate cost of the regulatory ceiling — locked-in emissions from prolonged coal operation. No exit, no voice, no standing to challenge. Their exclusion is structural to the legal framework.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, future_generations_interest, excluded,
    powerless, civilizational, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__facility_constraint_reading, coal_fleet_operators).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__facility_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, textually grounded limit on EPA authority under Section 111(d), giving regulated entities and states regulatory certainty and preserving state autonomy over electricity generation mix. Solves the coordination problem of arbitrary or oscillating federal climate regulation.
% TRANSFER_FUNCTION: Transfers the option value of systemic, cost-effective climate mitigation (generation shifting, renewable substitution) from climate advocates and future generations to coal fleet operators and state autonomy advocates, in the form of regulatory protection for existing coal assets and preservation of state policy discretion.
% ABSENT_VOICES: Future generations have no standing and no voice. Environmental justice communities are structurally excluded from the doctrinal conversation — their harm is local and cumulative, not the kind of 'particularized injury' courts recognize. The systemic transformation reading's beneficiaries (renewable industry, climate migrants, international partners) are absent because the constraint forecloses the regulatory pathway that would empower them.
% DISAPPEARANCE_RATIONALE: If the facility constraint reading vanished overnight (e.g., Supreme Court overruled West Virginia v. EPA), EPA could immediately propose systemic generation-shifting standards. Coal retirement would accelerate. State energy offices would lose their federalism shield. The renewable industry would gain a massive federal policy tailwind. The entire U.S. climate mitigation architecture would reorganize around the systemic pathway.
% FOUNDING_PROBLEM: The Clean Air Act's 'best system of emission reduction' language was ambiguous on whether EPA could look beyond the fenceline. The facility constraint reading was built to solve the problem of regulatory unpredictability: without a clear textual limit, EPA could impose transformative requirements that states and industry could not anticipate or plan for, undermining cooperative federalism and investment certainty.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (regulatory unpredictability) is attested by industry groups, conservative legal scholars, and Republican state attorneys general. It is contested by EPA's own historical practice (which used system-wide averaging in other CAA sections), progressive legal scholars who argue the text authorizes systemic approaches, and the D.C. Circuit's pre-West Virginia precedent. No neutral arbiter has settled whether the unpredictability problem was real or constructed.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is high because the constraint forecloses the most cost-effective mitigation pathway (generation shifting), forcing higher-cost facility-level compliance or inaction. Suppression (0.82) is very high because the constraint is maintained by Supreme Court precedent — exiting requires either Court reversal or statutory amendment. Theater ratio (0.48) reflects the genuine textualist argument layered over a policy outcome that aligns with coal protection; the ratio has risen as the major questions doctrine has expanded. Accessibility collapse (0.78) is high because once the 'facility constraint' is doctrinally entrenched, alternative readings (systemic transformation) become legally non-viable. Resistance (0.72) is significant: environmental litigants, progressive states, and congressional allies contest the reading, but the institutional pathway to change is narrow.
 *
 * PERSPECTIVAL GAP:
 *   From the coal fleet operator seat, this constraint is a Rope — it provides regulatory certainty and prevents arbitrary EPA overreach. From the climate advocate seat, it is a Snare — it extracts the possibility of adequate mitigation through a textualist cover story. From the Court's seat, it is a Mountain — the statute simply means what it says. The engine computes this divergence from the structural data: coal operators have constrained exit (can't leave the regulatory regime) but low directionality (beneficiaries); climate advocates have trapped exit and high directionality (victims). The same legal rule produces different constraint types at different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Coal fleet operators and state autonomy advocates are declared beneficiaries — they gain regulatory certainty and protection from forced retirement. Their directionality d is low (near 0.2) because the constraint subsidizes their position. Climate advocates and EJ communities are declared victims — they bear the cost of the regulatory ceiling. Their d is high (near 0.9) because the constraint extracts their preferred policy outcome. EPA sits at moderate d (~0.5) — it both administers the constraint and is constrained by it. Courts are near d=0.1 (beneficiary of interpretive authority). Future generations are trapped (d=1.0) but excluded from the stakeholder surface — an omega documents this.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (providing a stable, textually grounded limit on EPA authority to prevent regulatory oscillation) remains live but contested. The constraint has not resolved its mandatrophy because the coordination function (certainty, federalism) is genuinely valued by beneficiaries, while the extraction function (blocking systemic climate policy) is genuinely costly to victims. The classification as tangled_rope prevents mislabeling: calling it pure coordination (rope) would ignore the asymmetric coal protection; calling it pure extraction (snare) would ignore the genuine regulatory certainty it provides to states and regulated entities. The mandatrophy is unresolved — the constraint persists because neither side can dislodge it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_kernel_reading,
    'This constraint is one reading (facility_constraint_reading) of the contested kernel caa_section_111d_delegation. The sibling reading (systemic_transformation_reading) authorizes generation-shifting. What structural elements differ between readings?',
    'Compare the two readings'' beneficiary/victim structures, coordination claims, and extraction profiles. The facility reading protects coal; the systemic reading enables transition. They cannot both be authoritative in the same jurisdiction.',
    'If the kernel is recognized as genuinely contested, both readings get separate constraint stories with independent ε. If treated as one constraint with measurement variance, ε-invariance fails and classification becomes observer-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Committer frame: this story is one reading of a contested kernel; sibling reading would change beneficiary/victim structure and ε').

omega_variable(
    textualism_vs_purpose_ambiguity,
    'Does the facility constraint reading genuinely follow from statutory text, or is textualism a cover for a policy preference protecting coal?',
    'Legislative history analysis of 1970/1990 CAA amendments; comparison with how ''best system'' language operates in other CAA sections; doctrinal consistency with Chevron/major questions jurisprudence.',
    'If textualism is genuine, the constraint is a Mountain (law as written). If cover, it is a Snare/Tangled Rope extracting for coal. The theater_ratio of 0.48 reflects this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualism_vs_purpose_ambiguity, conceptual, 'Whether the reading''s textualist justification is structural or performative').

omega_variable(
    state_autonomy_vs_capture,
    'Is state autonomy over energy mix a genuine federalism benefit, or does it function as regulatory capture by coal-dependent states?',
    'Track which states invoke this reading and their energy portfolio composition; measure whether ''autonomy'' correlates with coal retention vs. renewable adoption.',
    'If genuine federalism, beneficiaries include all states. If capture, beneficiaries are coal-dependent states specifically — changing the beneficiary structure from broad to narrow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_autonomy_vs_capture, empirical, 'Whether state autonomy benefit is universal or concentrated in coal-dependent states').

omega_variable(
    suppression_mechanism_courts,
    'Is the suppression of systemic regulation structural (court enforcement of doctrinal limit) or internalized (EPA self-censors due to litigation risk)?',
    'Post-West Virginia v. EPA: track EPA rulemaking behavior. If EPA proposes only facility-level measures without litigation pressure, suppression is internalized. If EPA proposes systemic measures and courts strike them, suppression is structural.',
    'If internalized, effective suppression exceeds the structural measure — the constraint operates through anticipation, not just adjudication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_courts, empirical, 'Structural vs. internalized suppression in administrative agency behavior').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa_111d_facility_tr_t2015, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(caa_111d_facility_tr_t2017, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2017, 0.32).
narrative_ontology:measurement(caa_111d_facility_tr_t2019, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(caa_111d_facility_tr_t2021, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2021, 0.43).
narrative_ontology:measurement(caa_111d_facility_tr_t2023, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2023, 0.46).
narrative_ontology:measurement(caa_111d_facility_tr_t2025, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(caa_111d_facility_be_t2015, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(caa_111d_facility_be_t2017, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(caa_111d_facility_be_t2019, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement(caa_111d_facility_be_t2021, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement(caa_111d_facility_be_t2023, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2023, 0.66).
narrative_ontology:measurement(caa_111d_facility_be_t2025, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(caa_111d_facility_su_t2015, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(caa_111d_facility_su_t2017, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2017, 0.65).
narrative_ontology:measurement(caa_111d_facility_su_t2019, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2019, 0.72).
narrative_ontology:measurement(caa_111d_facility_su_t2021, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2021, 0.78).
narrative_ontology:measurement(caa_111d_facility_su_t2023, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2023, 0.8).
narrative_ontology:measurement(caa_111d_facility_su_t2025, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(caa_section_111d_delegation__facility_constraint_reading, 0.12).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation__systemic_transformation_reading).

% DUAL FORMULATION NOTE:
% Kernel caa_section_111d_delegation decomposes into two constraint stories: facility_constraint_reading (this story, ε=0.68, tangled_rope) and systemic_transformation_reading (ε≈0.35, rope/scaffold). The facility reading has higher ε because it forecloses lower-cost mitigation, concentrating extraction on climate advocates. The systemic reading has lower ε because it enables cost-effective mitigation, though it imposes transition costs on coal. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__facility_constraint_reading, institutional, 0.15).
constraint_indexing:directionality_override(caa_section_111d_delegation__facility_constraint_reading, powerful, 0.25).
constraint_indexing:directionality_override(caa_section_111d_delegation__facility_constraint_reading, organized, 0.85).
constraint_indexing:directionality_override(caa_section_111d_delegation__facility_constraint_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
