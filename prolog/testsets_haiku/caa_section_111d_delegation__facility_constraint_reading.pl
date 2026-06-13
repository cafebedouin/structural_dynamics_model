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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: caa_section_111d_delegation__facility_constraint_reading
 *   human_readable: CAA Section 111(d) 'Best System' — Facility-Level Constraint Reading
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   Section 111(d) of the Clean Air Act directs EPA to set emission standards
 *   for existing sources based on the 'best system of emission reduction'
 *   (BSER). This constraint story instantiates ONE READING of a contested
 *   constitutional kernel: the facility-constraint reading holds that 'best
 *   system' is limited to measures implementable at individual plants —
 *   heat-rate improvements, post-combustion carbon capture, efficiency
 *   upgrades — NOT generation-shifting, coal retirement mandates, or
 *   wholesale market restructuring. This reading protects fossil fuel
 *   operators from forced-exit mandates, preserves state energy-mix
 *   authority, and creates a regulatory ceiling that climate advocates
 *   experience as an extraction (they lose access to the most direct federal
 *   decarbonization tool). The sibling reading
 *   (systemic_transformation_reading) claims the statute authorizes grid-wide
 *   transformation, including renewable substitution and coal retirement.
 *   These readings are NOT observables of the same constraint; they are
 *   structurally distinct constraints arising from competing interpretations
 *   of the same authorizing text (the kernel). This story generates the
 *   facility-constraint reading only.
 *
 * KEY AGENTS:
 *   - fossil_fuel_operators: benefit from regulatory ceiling that forbids generation-shifting mandates; organized, powerful, near-beneficiary directionality
 *   - coal_plant_owners: benefit from protection against forced retirement; regional, organized, captured upside from regulatory constraint
 *   - state_energy_authorities: benefit from retained control over energy-mix decisions; institutional, autonomous, agenda-setters under the constraint
 *   - climate_policy_advocates: payer seat; excluded from direct EPA-mandated decarbonization pathway; organized, powerful enough to litigate but structurally constrained
 *   - environmental_justice_communities: victim seat; trapped co-location with plants; powerless; bear local costs of continued coal operation
 *   - epa_enforcement_seat: analytical observer; interprets the statute; authority cabined to facility-level measures
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
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "CAA Section 111(d) 'Best System' — Facility-Level Constraint Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, '3bf8bdfe-44f3-4944-93b9-3dd0c2d99200').
narrative_ontology:cs_kernel_codification('3bf8bdfe-44f3-4944-93b9-3dd0c2d99200', fixed_text).
narrative_ontology:cs_authority_grounding('3bf8bdfe-44f3-4944-93b9-3dd0c2d99200', lineage).
narrative_ontology:cs_interpretation_layer_present('3bf8bdfe-44f3-4944-93b9-3dd0c2d99200').
narrative_ontology:cs_reading_relation('3bf8bdfe-44f3-4944-93b9-3dd0c2d99200', caa_section_111d_delegation__systemic_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('3bf8bdfe-44f3-4944-93b9-3dd0c2d99200', foundational, facility_level_authority_limit).
narrative_ontology:cs_axiom_status(facility_level_authority_limit, holdable).
narrative_ontology:cs_axiom_grounding('3bf8bdfe-44f3-4944-93b9-3dd0c2d99200', facility_level_authority_limit, deontological).
narrative_ontology:cs_axiom('3bf8bdfe-44f3-4944-93b9-3dd0c2d99200', foundational, state_energy_mix_sovereignty).
narrative_ontology:cs_axiom_status(state_energy_mix_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('3bf8bdfe-44f3-4944-93b9-3dd0c2d99200', state_energy_mix_sovereignty, conventional).
narrative_ontology:cs_reference_frame('3bf8bdfe-44f3-4944-93b9-3dd0c2d99200', enumerated_federal_powers_federalism_doctrine).
narrative_ontology:cs_drift_state('3bf8bdfe-44f3-4944-93b9-3dd0c2d99200', contemporary_climate_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3bf8bdfe-44f3-4944-93b9-3dd0c2d99200', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, fossil_fuel_operators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_plant_owners).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, state_energy_authorities).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_policy_advocates).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, future_generations_climate_exposure).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).

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
 *   Extractiveness at 0.68 reflects the asymmetry: fossil operators benefit from a regulatory ceiling that climate advocates experience as foreclosure. The ceiling is sustained by active enforcement (EPA does not issue generation-shifting rules; litigation enforces the boundary) — hence suppression at 0.72. Theater ratio at 0.41 indicates the facility-level compliance narrative is substantial but increasingly performative: operators implement efficiency measures and announce carbon-reduction commitments, but the aggregate effect is slow decarbonization relative to climate science demands. The trajectory over the interval shows extractiveness rising modestly as fossil operators adapt to modest facility-level standards while maintaining market advantage; theater rises as the gap widens between facility-level compliance and climate-scale need. Accessibility collapse at 0.62 reflects that alternatives (EPA generation-shifting, renewable mandate) are foreclosed by this reading; climate advocates can litigate or seek state policies, but the federal pathway is closed — not quite a natural law (collapse could be reversed by legislative amendment or a sibling-reading victory), but substantially constrained.
 *
 * PERSPECTIVAL GAP:
 *   From the fossil operator and state energy authority seats, this reading is genuine coordination: EPA sets clear facility-level standards, states choose how to meet them, markets respond. From the climate advocate and EJ community seats, the same structure is enforced extraction: a regulatory ceiling that protects incumbent operators and forecloses rapid decarbonization. The engine computes different directionality and classification for each seat based on whether they benefit from or bear costs from the facility constraint. The agenda_setter/beneficiary vs. payer asymmetry drives the perspectival split.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil operators (powerful, organized, beneficiary) sit near d=0.1–0.2: they benefit directly from the constraint and face low exit friction (can operate efficiently under facility-level standards). State energy authorities (institutional, beneficiary/agenda_setter) sit near d=0.15–0.25: they wield the constraint and benefit from state autonomy, with mobile exit options (can shift energy mix if political pressure grows). Climate advocates (organized, payer) sit near d=0.75–0.85: they bear the cost of regulatory foreclosure, have constrained exit (can litigate, lobby, seek state change, but cannot unilaterally bypass the constraint), and lack power parity with operators. EJ communities (powerless, trapped, victim) sit near d=0.9–1.0: fully targeted by continued plant operation, trapped in location, structurally excluded from remedy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (does Section 111(d) authorize generation-shifting?) was live at the statute's enactment (1970); the facility-constraint reading treats it as definitively resolved by the West Virginia v. EPA decision (2022), which affirmed that EPA lacks that authority. However, climate advocates and the dissent argue the founding problem remains contested — the statute's text is ambiguous and the policy case for EPA authority is live. The measurement series shows extractiveness rising over the interval: this reflects the constraint hardening as fossil operators adjust their operations to the new regulatory ceiling and climate advocates exhaust litigation avenues without reversing the ruling. Theater rises because facility-level compliance becomes increasingly theatrical (the plants achieve modest efficiency while the broader energy system locks in fossil dependency). This trajectory is consistent with mandatrophy — the constraint's original coordination function (EPA/state coordination on emission standards) is superseded by a new extraction function (protection of fossil capital and regulatory foreclosure of decarbonization). The constraint is NOT defunct (it actively forecloses alternatives), but its founding problem has been declaratively 'solved' by judicial authority while the parties dispute whether the solution is correct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_text_interpretation_indeterminacy,
    'Does the text ''best system of emission reduction'' in Section 111(d) more naturally read as facility-level-only or as system-wide emission reduction?',
    'Canonical statutory interpretation (textualism, intentionalism, purposivism produce different readings); legislative history review; comparable statutory language in other environmental statutes; academic statutory construction scholarship from neutral sources outside the benefiting parties.',
    'If the text more naturally reads system-wide, the facility-constraint reading is a restrictive interpretation that benefits incumbents; if facility-level-only is the natural reading, the constraint reflects statutory design. This determines whether the constraint is a judicial artifact favoring fossil operators or a faithful implementation of congressional intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_text_interpretation_indeterminacy, conceptual, 'Whether the statutory kernel admits the facility-constraint reading as a reasonable (vs. forced or artificial) interpretation.').

omega_variable(
    climate_necessity_vs_statutory_boundaries,
    'If climate science establishes that facility-level measures are insufficient to prevent catastrophic atmospheric carbon accumulation, does that scientific fact alter the legal boundary between EPA and state authority?',
    'Legislative amendment to Section 111(d) explicitly authorizing EPA generation-shifting; judicial reversal of West Virginia v. EPA via new Supreme Court composition or case law evolution; expert testimony on climate tipping points and their relationship to feasible decarbonization pathways from climate science organizations independent of policy advocates.',
    'Scientific evidence of insufficiency would create a gap between the constraint''s foreclosure effect and climate necessity, motivating legislative remedy or new judicial interpretation. The constraint''s persistence despite such evidence would establish it as a form of democratic foreclosure (the regulatory ceiling persists despite emergent need). This feeds the mandatrophy analysis and the false-summit question: is the facility constraint a discovered legal boundary or a constructed protection of fossil assets dressed as statutory interpretation?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_necessity_vs_statutory_boundaries, empirical, 'Whether climate science necessity can override the legal/political constraint''s boundaries.').

omega_variable(
    suppression_internalization_vs_structural,
    'Is the climate advocate and EJ community suppression primarily structural (litigation costs, political imbalance, legal foreclosure) or internalized (belief that EPA lacks authority, acceptance of federalism doctrine)?',
    'Survey of climate advocate and EJ community actors: if suppression persists after the facility-constraint reading is reversed (via legislative amendment or judicial overruling), that persistence indicates internalization; if suppression ceases immediately upon legal change, the suppression was primarily structural. Post-reversal trajectory of advocacy aggressiveness and legislative-reform energy.',
    'Structural suppression indicates the constraint operates via external barriers (the legal rule itself); internalized suppression indicates the constraint''s subjects have absorbed the boundary as legitimate, creating path dependence even if the boundary changes. High internalization would indicate the constraint has done identity work on the advocacy community (they have become ''EPA-authority-limited'' rather than ''climate-first''), which persists even when the constraint is formally reversed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'Mechanism of suppression operation in climate-policy advocacy.').

omega_variable(
    false_summit_kernel_reading,
    'Is the facility-constraint reading itself a false summit — a reading that serves fossil operator interests, dressed as statutory interpretation, maintained through beneficiary institutional support and interpreted by justices with ideological commitments?',
    'Critical statutory interpretation scholarship from law schools independent of fossil-industry funding; comparison of how the same statutory language was interpreted in prior EPA regulations before the political landscape changed; analysis of the Supreme Court''s voting bloc composition and prior decisions on regulatory authority in other statutes; examination of amicus curiae briefs and their funding sources in West Virginia v. EPA.',
    'If the reading is a false summit (natural law dressed as interpretation, benefiting fossil operators), the constraint''s claimed type (tangled_rope coordination) would reclassify toward snare (pure extraction). The false-summit signature would fire because a commitment-system reading produces a constraint that is formalized/fixed_text (Section 111(d)), grounds itself in lineage/expertise (statutory interpretation, Supreme Court authority), but primarily serves fossil operator extraction rather than genuine coordination. Declaring beneficiaries (fossil operators) on a constraint claiming to be statutory interpretation would trigger FSM evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_kernel_reading, conceptual, 'Whether the facility-constraint reading is a natural legal boundary or a constructed reading that serves fossil operator interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(caa__tr_t5, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement(caa__tr_t10, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(caa__tr_t15, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(caa__tr_t20, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(caa__tr_t25, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(caa__be_t5, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(caa__be_t10, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(caa__be_t15, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(caa__be_t20, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(caa__be_t25, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(caa__su_t5, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(caa__su_t10, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(caa__su_t15, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(caa__su_t20, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(caa__su_t25, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(caa_section_111d_delegation__facility_constraint_reading, 0.12).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation__systemic_transformation_reading).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, coal_plant_economic_viability_constraint).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, state_renewable_portfolio_standard_constraint).

% DUAL FORMULATION NOTE:
% This constraint and caa_section_111d_delegation__systemic_transformation_reading are competing interpretations of the same statutory kernel (Section 111(d) 'best system of emission reduction'). They are NOT two measurements of one constraint; they are structurally distinct constraints with different beneficiaries, victims, and extraction profiles. The facility-constraint reading benefits fossil operators and coal plant owners by limiting EPA's authority; the systemic-transformation reading would benefit climate advocates and renewable developers by expanding EPA's authority. Only ONE reading prevails in any given period of law; when the facility-constraint reading is dominant (as under West Virginia v. EPA), the systemic reading remains a live dispute and a separate constraint. The two readings form a kernel family: all members link via network.affects_constraints. The upstream story (facility_constraint_reading) currently influences the downstream story (systemic_transformation_reading) because the facility reading is the legally dominant interpretation; if the systemic reading were adopted (via legislative amendment or judicial reversal), the directionality would invert.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__facility_constraint_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
