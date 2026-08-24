% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__systemic_transformation_reading, []).

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
 *   constraint_id: caa_section_111d_delegation__systemic_transformation_reading
 *   human_readable: CAA §111(d) Systemic Transformation Reading — Grid-Wide Generation Shifting Authority
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint story models the 'systemic transformation' reading of CAA
 *   §111(d) — the interpretation that 'best system of emission reduction'
 *   authorizes EPA to set standards based on grid-wide generation shifting
 *   (coal-to-gas, coal-to-renewables, nuclear retention) rather than
 *   facility-level measures alone. This reading powered the Obama Clean Power
 *   Plan (2015), was repealed by the Trump ACE rule (2019), and was
 *   ultimately constrained by West Virginia v. EPA (2022) which held that
 *   such 'generation shifting' exceeds §111(d) without clear congressional
 *   authorization. The Biden EPA's 2024 rule attempts a narrowed systemic
 *   approach. The constraint is a kernel reading: the same statutory text
 *   ('best system') generates two mutually reinforcing but structurally
 *   distinct constraints depending on interpretive frame.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.78).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.82).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "CAA §111(d) Systemic Transformation Reading — Grid-Wide Generation Shifting Authority").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, '5f5c463f-0b31-44e9-b048-d4af55790cdb').
narrative_ontology:cs_kernel_codification('5f5c463f-0b31-44e9-b048-d4af55790cdb', formalized).
narrative_ontology:cs_authority_grounding('5f5c463f-0b31-44e9-b048-d4af55790cdb', extraction).
narrative_ontology:cs_interpretation_layer_present('5f5c463f-0b31-44e9-b048-d4af55790cdb').
narrative_ontology:cs_reading_relation('5f5c463f-0b31-44e9-b048-d4af55790cdb', caa_section_111d_delegation__facility_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('5f5c463f-0b31-44e9-b048-d4af55790cdb', foundational, systemic_bsr_authorized).
narrative_ontology:cs_axiom_status(systemic_bsr_authorized, holdable).
narrative_ontology:cs_axiom_grounding('5f5c463f-0b31-44e9-b048-d4af55790cdb', systemic_bsr_authorized, empirically_contingent).
narrative_ontology:cs_axiom('5f5c463f-0b31-44e9-b048-d4af55790cdb', foundational, generation_shifting_is_emission_reduction).
narrative_ontology:cs_axiom_status(generation_shifting_is_emission_reduction, holdable).
narrative_ontology:cs_axiom_grounding('5f5c463f-0b31-44e9-b048-d4af55790cdb', generation_shifting_is_emission_reduction, empirically_contingent).
narrative_ontology:cs_reference_frame('5f5c463f-0b31-44e9-b048-d4af55790cdb', clean_air_act_textual_authority).
narrative_ontology:cs_drift_state('5f5c463f-0b31-44e9-b048-d4af55790cdb', post_west_virginia_v_epa, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5f5c463f-0b31-44e9-b048-d4af55790cdb', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, clean_energy_states).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, climate_policy_advocates).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_sector).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_states).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_states).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, systemic_bsr_interpretation).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, generation_shifting_as_emission_reduction).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, epa_grid_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues emission guidelines under §111(d) defining 'best system of emission reduction' as including generation shifting from coal to renewables and nuclear. Sets state targets, reviews state plans, can impose federal plan. Authority contested in courts; each administration reverses predecessor's reading. Gains regulatory legacy and bureaucratic mission expansion; bears political cost when courts strike down rules.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa_administrator, agenda_setter,
    institutional, generational, analytical, national).

% Faces mandated early retirement of coal plants or costly compliance via generation shifting. Capital-intensive assets become stranded; workforce cannot transition quickly. Lobbies aggressively, funds litigation, secures congressional allies. Exit means selling assets at fire-sale prices or bankruptcy; identity fused to 'baseload power' narrative. Extraction is direct: compliance costs + asset stranding transferred to renewable competitors via regulatory mechanism.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_sector, payer,
    powerful, biographical, trapped, national).

% States with coal-heavy grids (WV, WY, KY, ND) bear disproportionate compliance burden: must redesign entire electricity system, lose tax revenue, manage workforce displacement. Some receive transition funding (secondary beneficiary) but amounts are fraction of losses. Exit from coal economy is multi-decade; political leadership fused to fossil identity. Sue EPA, join multi-state litigation, threaten non-compliance.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_states, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_states, beneficiary).

% Gains guaranteed market access via state compliance plans that mandate renewable procurement. Federal tax credits (IRA) stack with regulatory demand. No enforcement burden — they are the compliance pathway. Exit is easy: sell projects, move capital globally. Benefit is structural: regulation creates demand floor that private contracts alone would not.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_sector, beneficiary,
    organized, biographical, mobile, national).

% States with existing renewable mandates (CA, NY, WA, CO) already meet or exceed systemic targets. Gain competitive advantage: their grids are 'compliant by default,' attracting investment. Can export clean electricity to fossil-locked states. Exit not relevant — they designed their own transition. Benefit is regulatory asymmetry: federal rule ratifies their head start.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, clean_energy_states, beneficiary,
    organized, biographical, mobile, regional).

% Towns, counties, school districts dependent on coal tax base and jobs. No voice in EPA rulemaking; state leaders often oppose transition while communities bear costs. Identity fused to coal culture — 'coal miner' is not just a job but a self-concept. Transition funding (DOE, IRA) exists but is fragmented, slow, and culturally alien. Cannot exit geographically (homes underwater, no buyers) or professionally (skills non-transferable at scale).
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_communities, payer,
    powerless, generational, identity_locked, local).

% Ultimate arbiter of the delegation's scope. West Virginia v. EPA (2022) applied major questions doctrine to reject systemic reading without clear congressional authorization. Does not implement policy but defines the constraint's legal boundary. Its composition determines which reading survives. No extraction or benefit — but its doctrine shapes which stakeholders prevail.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% Enacted §111(d) in 1970, amended in 1990; has not legislated on 'best system' scope since. Would need to clarify or overturn Court's reading but is gridlocked. Fossil-state senators block clarification; climate advocates lack 60 votes. Excluded from real-time implementation but their silence is the constraint's enabling condition — the ambiguity persists because Congress cannot resolve it.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, congress, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates economy-wide decarbonization of the electricity sector by treating the grid as an integrated system rather than a collection of facilities — enabling least-cost emission reduction through generation shifting, renewable deployment, and nuclear retention.
% TRANSFER_FUNCTION: Transfers compliance costs and asset stranding from coal sector and fossil-locked states to renewable energy sector and clean energy states via state implementation plans that mandate renewable procurement, coal retirement schedules, and emissions trading — with federal tax subsidies (IRA) amplifying the transfer.
% ABSENT_VOICES: Coal communities (local governments, school districts, displaced workers) who bear concentrated transition costs without representation in EPA rulemaking or state planning; fossil fuel workers whose skills and pensions are tied to coal; Native nations affected by both coal pollution and renewable siting on ancestral lands.
% DISAPPEARANCE_RATIONALE: If systemic reading vanished overnight, EPA would revert to facility-constraint reading (heat-rate improvements, CCS only). Coal plants would run longer; renewable deployment would slow to state-only mandates; 2030/2035 climate targets would become unattainable; fossil-locked states would avoid transition costs but lock in higher long-term system costs and climate damages.
% FOUNDING_PROBLEM: Climate change requires rapid, system-wide decarbonization of electricity generation; facility-level measures (heat-rate improvements, carbon capture) are technically insufficient and economically inefficient to achieve necessary emission reductions at scale.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 (2021-2023) and IEA Net Zero by 2050 corroborate that system-wide transformation is necessary; but fossil interests, textualist judges, and fossil-locked states contest whether §111(d) authorizes it — no corroboration from outside the beneficiary coalition (renewable sector, clean states, climate advocates) that the *statute* authorizes this specific coordination mechanism.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__systemic_transformation_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint transfers massive compliance costs and asset stranding to coal sector and fossil-locked states while creating guaranteed demand for renewables — the transfer is the mechanism, not a side effect. Suppression (0.82) is high because fossil-locked states and coal sector have no exit: they must comply, litigate, or secede (politically impossible). Theater ratio (0.45) reflects that the coordination function (least-cost decarbonization) is real but increasingly performative as legal authority erodes — post-West Virginia, EPA's 2024 rule carefully avoids 'generation shifting' language while achieving similar outcomes through state flexibility. Accessibility collapse (0.75) is high for coal communities: once the systemic target is set, alternatives (keep plants running) collapse legally and economically. Resistance (0.85) is maximal: multi-state litigation, congressional opposition, Supreme Court intervention, and industry campaigns.
 *
 * PERSPECTIVAL GAP:
 *   From EPA/renewable/clean-state seats, the constraint computes as rope/tangled_rope: genuine coordination solving a collective action problem (climate) with asymmetric but justified burden-sharing. From coal sector/fossil-state/coal-community seats, it computes as snare: the coordination story is cover for political extraction — the same result (coal retirement) could be achieved via carbon price or congressional action, but those require political majorities that don't exist. The Supreme Court seat reads it as mountain-adjacent: the statutory text is a fixed boundary (major questions doctrine) that the systemic reading violates. The engine computes these divergences from the structural data; the claimed_type (tangled_rope) is this author's structural judgment.
 *
 * DIRECTIONALITY LOGIC:
 *   EPA administrator is agenda_setter with analytical exit — they design the rule but bear no compliance cost; directionality near beneficiary end (d ~ 0.2). Coal sector is payer with trapped exit — asset stranding is irreversible; directionality near target end (d ~ 0.95). Fossil-locked states are payer with constrained exit — they can litigate but must eventually comply or lose federal highway funds; d ~ 0.85. Renewable sector and clean states are beneficiaries with mobile exit — they capture compliance demand but can redeploy capital; d ~ 0.15. Coal communities are payer with identity_locked exit — they cannot leave geographically or culturally; d ~ 0.9. Supreme Court is observer with analytical exit — defines boundary but extracts nothing; d ~ 0.5. Congress is excluded — would object but is structurally absent; d undefined.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (climate requires systemic grid transformation) is live and worsening — IPCC emissions gaps grow. But the *statutory authorization* for this specific coordination mechanism is dead per West Virginia v. EPA. The constraint persists because EPA, states, and markets have built implementation infrastructure around the systemic reading (IRA subsidies, state RPS, corporate PPAs) that would be costly to unwind. Mandatrophy is partially resolved: the original statutory mandate is foreclosed, but a de facto systemic constraint now operates through layered state/federal/market mechanisms that no single actor can dismantle. The theater_ratio rise (0.25→0.45) tracks this: the legal authority atrophied but the coordination apparatus persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the systemic_transformation_reading a legitimate interpretation of ''best system'' or an ultra vires expansion that the major questions doctrine correctly forecloses?',
    'Congressional clarification (new statute amending §111(d)) or a future Supreme Court composition that either affirms or overrules West Virginia v. EPA''s application to this specific delegation.',
    'If legitimate, the constraint remains a tangled_rope with EPA authority intact; if ultra vires, the constraint collapses to facility_constraint_reading (lower extractiveness, different beneficiaries/victims) and the systemic coordination apparatus becomes legally unauthorized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the statutory text bears the systemic reading or whether the reading is a construction that serves beneficiary interests.').

omega_variable(
    coal_community_transition_adequacy,
    'Do existing transition policies (IRA energy community tax credits, DOE grants, POWER Initiative) structurally mitigate the identity_locked extraction on coal communities, or is the mitigation performative?',
    'Longitudinal tracking of coal community economic indicators (employment, tax base, population, opioid mortality) vs. transition funding flows over 2025-2035.',
    'If mitigation is adequate, coal_communities shift from payer (d~0.9) toward constrained (d~0.6) — the extraction becomes a managed transition. If performative, identity_locked extraction persists and the constraint''s snare character deepens regardless of legal reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coal_community_transition_adequacy, empirical, 'Whether the constraint''s extraction on the most vulnerable stakeholders is structurally mitigated or merely rhetorical.').

omega_variable(
    major_questions_doctrine_scope,
    'Does West Virginia v. EPA''s major questions doctrine application to §111(d) establish a stable boundary, or will its scope expand/contract with Court composition?',
    'Track lower court applications of MQD to other EPA rules (methane, wastewater, PM2.5) and any future cert grants on climate regulation.',
    'If MQD expands, systemic reading is permanently foreclosed and facility_constraint_reading becomes the only legally viable constraint. If MQD contracts or is limited to its facts, EPA may reclaim systemic authority in future rules.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(major_questions_doctrine_scope, conceptual, 'Whether the judicial constraint on this reading is settled law or a contested doctrinal frontier.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression on fossil-locked states structural (legal mandate, loss of highway funds) or internalized (political leadership fuses identity with fossil resistance, making compliance politically impossible even when economically rational)?',
    'Compare compliance behavior of fossil-locked states with similar economic profiles but different political leadership; track whether resistance persists after federal enforcement threat diminishes.',
    'If internalized, effective suppression exceeds the structural measure — the constraint extracts political legitimacy as well as compliance costs. If structural, suppression drops when legal mandate is removed (post-West Virginia).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression operates through external coercion or identity-fused political resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa_111d_systemic_tr_t2015, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(caa_111d_systemic_tr_t2017, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2017, 0.3).
narrative_ontology:measurement(caa_111d_systemic_tr_t2019, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2019, 0.35).
narrative_ontology:measurement(caa_111d_systemic_tr_t2021, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2021, 0.42).
narrative_ontology:measurement(caa_111d_systemic_tr_t2022, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2022, 0.5).
narrative_ontology:measurement(caa_111d_systemic_tr_t2024, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(caa_111d_systemic_be_t2015, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(caa_111d_systemic_be_t2017, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2017, 0.65).
narrative_ontology:measurement(caa_111d_systemic_be_t2019, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2019, 0.72).
narrative_ontology:measurement(caa_111d_systemic_be_t2021, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2021, 0.76).
narrative_ontology:measurement(caa_111d_systemic_be_t2022, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2022, 0.8).
narrative_ontology:measurement(caa_111d_systemic_be_t2024, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(caa_111d_systemic_su_t2015, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(caa_111d_systemic_su_t2017, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2017, 0.75).
narrative_ontology:measurement(caa_111d_systemic_su_t2019, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2019, 0.8).
narrative_ontology:measurement(caa_111d_systemic_su_t2021, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2021, 0.85).
narrative_ontology:measurement(caa_111d_systemic_su_t2022, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2022, 0.9).
narrative_ontology:measurement(caa_111d_systemic_su_t2024, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(caa_section_111d_delegation__systemic_transformation_reading, 0.12).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation__facility_constraint_reading).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, ira_clean_energy_subsidies).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, state_renewable_portfolio_standards).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, west_virginia_v_epa_doctrine).

% DUAL FORMULATION NOTE:
% This constraint and facility_constraint_reading form a kernel family: same statutory text, mutually reinforcing but structurally distinct readings. The systemic reading has higher extractiveness (0.78 vs ~0.35) and different victim/beneficiary structure. The facility reading would have coal_sector as regulated party (moderate extraction) but not extraction victim; renewable_sector would not be compliance-subsidized beneficiary. The dual formulation is the interpretive choice that creates two different constraints from one kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__systemic_transformation_reading, organized, 0.85).
constraint_indexing:directionality_override(caa_section_111d_delegation__systemic_transformation_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
