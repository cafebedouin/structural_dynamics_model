% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__historical_responsibility_reading, []).

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
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR — Historical Responsibility Reading (Binding Reductions + Loss/Damage Finance)
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   The CBDR (Common But Differentiated Responsibilities) principle is a
 *   foundational but persistently contested element of international climate
 *   law. This story generates the reading under which CBDR is read as
 *   requiring BINDING obligations — enforceable emissions reduction targets
 *   scaled to cumulative historical emissions, plus dedicated, binding
 *   loss/damage financing (as partially realized in the 2022 COP27 Loss and
 *   Damage Fund). This reading treats the atmosphere as a shared sink whose
 *   remaining capacity has already been disproportionately consumed by
 *   industrialized nations, and treats that historical consumption as
 *   generating an enforceable liability, not a discretionary courtesy.
 *
 * KEY AGENTS:
 *   - small_island_developing_states: primary beneficiary (powerless/trapped) — existential exposure, minimal historical emissions
 *   - least_developed_countries: primary beneficiary (powerless/trapped) — adaptation cost burden without historical contribution
 *   - developed_nation_treasuries: primary target (institutional/constrained) — bears binding financial and emissions obligations
 *   - developed_nation_carbon_intensive_industries: secondary target (powerful/constrained) — absorbs domestic compliance cost
 *   - unfccc_secretariat: agenda-setting institution (institutional/analytical) — administers whichever reading prevails
 *   - climate_justice_advocacy_coalitions: analytical/excluded observer — documents cumulative-emissions case, no voting standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.61).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.52).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR — Historical Responsibility Reading (Binding Reductions + Loss/Damage Finance)").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, '3d891360-d081-4635-850d-2116ca811e6f').
narrative_ontology:cs_kernel_codification('3d891360-d081-4635-850d-2116ca811e6f', fixed_text).
narrative_ontology:cs_authority_grounding('3d891360-d081-4635-850d-2116ca811e6f', distributed).
narrative_ontology:cs_reading_relation('3d891360-d081-4635-850d-2116ca811e6f', cbdr_principle__voluntary_commitment_reading, coexists_with).
narrative_ontology:cs_axiom('3d891360-d081-4635-850d-2116ca811e6f', foundational, cumulative_historical_emitters_bear_binding_liability).
narrative_ontology:cs_axiom_status(cumulative_historical_emitters_bear_binding_liability, holdable).
narrative_ontology:cs_axiom_grounding('3d891360-d081-4635-850d-2116ca811e6f', cumulative_historical_emitters_bear_binding_liability, deontological).
narrative_ontology:cs_axiom('3d891360-d081-4635-850d-2116ca811e6f', foundational, atmospheric_sink_capacity_already_disproportionately_consumed).
narrative_ontology:cs_axiom_status(atmospheric_sink_capacity_already_disproportionately_consumed, holdable).
narrative_ontology:cs_axiom_grounding('3d891360-d081-4635-850d-2116ca811e6f', atmospheric_sink_capacity_already_disproportionately_consumed, empirically_contingent).
narrative_ontology:cs_reference_frame('3d891360-d081-4635-850d-2116ca811e6f', rio_1992_polluter_liability_framework).
narrative_ontology:cs_drift_state('3d891360-d081-4635-850d-2116ca811e6f', post_paris_agreement_ndc_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3d891360-d081-4635-850d-2116ca811e6f', '').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, small_island_developing_states).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, climate_vulnerable_coastal_populations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nation_treasuries).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nation_carbon_intensive_industries).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nation_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face existential territorial loss from sea-level rise despite negligible cumulative emissions. Under this reading they are entitled to loss/damage financing and to demand binding, not voluntary, reductions from historical emitters. They have no exit from the physical exposure and depend on treaty mechanisms they did not design and cannot enforce unilaterally.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, small_island_developing_states, beneficiary,
    powerless, generational, trapped, global).

% Bear disproportionate adaptation costs relative to historical contribution to atmospheric carbon stock. Under this reading they gain a legal claim to differentiated obligations and dedicated financing, rather than relying on discretionary aid. Their leverage is coalition voice inside UNFCCC negotiations, not market or exit power.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, least_developed_countries, beneficiary,
    powerless, generational, trapped, global).

% Directly experience flooding, crop failure, and displacement. They are represented in the negotiation only through their states' delegations and have no independent standing; loss/damage financing under this reading is meant to reach them but is mediated by national governments.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_vulnerable_coastal_populations, beneficiary,
    powerless, biographical, trapped, regional).

% Face binding financial transfer obligations and emissions targets calculated against a cumulative historical baseline going back to the industrial revolution. They can negotiate the pace and mechanism of compliance and can withdraw from specific instruments (as with the 2001 US withdrawal from Kyoto and periodic Paris Agreement exits), but cannot escape the underlying historical-emissions accounting once it is adopted as the treaty's operative principle.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nation_treasuries, payer,
    institutional, biographical, constrained, national).

% Absorb compliance costs, carbon pricing, and stranded-asset risk as governments translate binding national targets into domestic regulation. They lobby extensively against binding formulations and have some capacity to relocate operations to weaker-jurisdiction states, but full exit means abandoning home markets and political relationships.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nation_carbon_intensive_industries, payer,
    powerful, biographical, constrained, national).

% Fund loss/damage transfers and domestic decarbonization subsidies through taxation, without having personally caused the cumulative emissions being billed against their nation's historical account. Exit requires emigration or political mobilization against the commitment; most simply bear the diffuse cost through public spending allocation.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nation_taxpayers, payer,
    moderate, biographical, constrained, national).

% Countries whose current-year emissions now rival or exceed developed nations but whose cumulative historical contribution remains comparatively low. This reading's historical baseline places them outside binding reduction obligations and outside the payer set, a position developed nations contest as inequitable but that the reading's own accounting logic protects. They are not centrally in this story's dispute but shape its political feasibility.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, emerging_economy_high_emitters, excluded,
    organized, generational, mobile, global).

% Administers the treaty architecture, convenes negotiations, and operationalizes whichever CBDR reading the Conference of Parties adopts into binding text. It does not itself collect financing but sets the procedural agenda that determines whether the historical-responsibility or voluntary-commitment reading governs the operative instrument in a given cycle.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, analytical, global).

% NGOs and academic networks that document cumulative emissions data and press for the historical-responsibility framing in negotiations. They have no vote in COP decisions and are formally excluded from binding-text authorship, though their analysis shapes which reading gains traction.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_justice_advocacy_coalitions, observer,
    organized, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, climate_justice_advocacy_coalitions, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__historical_responsibility_reading, diffuse).
narrative_ontology:fixing_cost_class(cbdr_principle__historical_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global emissions-reduction and financing regime by allocating obligation according to who put the atmospheric carbon stock there, solving the free-rider problem that would otherwise let historically high emitters delay action while low-emitting states bear the physical consequences.
% TRANSFER_FUNCTION: Moves binding emissions-reduction obligations and loss/damage financing from developed nations (measured by cumulative historical emissions) to climate-vulnerable developing nations and populations who contributed least to the atmospheric stock causing the harm.
% ABSENT_VOICES: Climate-vulnerable coastal populations and future generations have no independent standing in the negotiation; they are represented only through state delegations that may trade away financing commitments for other national priorities. Emerging economy high emitters are structurally advantaged by this reading's historical baseline and have incentive to support it publicly while resisting any future reading that would extend liability to recent large emitters.
% DISAPPEARANCE_RATIONALE: If the binding historical-responsibility reading vanished, developed-nation treasuries would face no enforceable financing schedule, loss/damage mechanisms (like the Warsaw International Mechanism and the 2022 Loss and Damage Fund) would lose their legal anchor, and negotiations would revert toward the voluntary-commitment reading's nationally-determined framework — vulnerable states would lose a claim they currently use to press for binding transfers.
% FOUNDING_PROBLEM: The 1992 UNFCCC founding problem: industrialized nations had already emitted the large majority of cumulative atmospheric carbon by the time global climate action began, while developing nations faced pressure to constrain their own still-nascent emissions and bore disproportionate physical exposure to resulting climate harm without having caused it.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports and independent carbon-accounting bodies (e.g., Global Carbon Project) corroborate the underlying cumulative-emissions asymmetry from outside both the beneficiary coalition and the payer governments; this is not solely self-asserted by vulnerable states. Developed-nation governments dispute the status as 'live' in binding form, arguing current-year emissions from large emerging emitters make the historical baseline an increasingly incomplete picture — but do not dispute the historical asymmetry itself.
narrative_ontology:disappearance_verdict(cbdr_principle__historical_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__historical_responsibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__historical_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cbdr_principle__historical_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__historical_responsibility_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__historical_responsibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cbdr_principle__historical_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.61 by 2024 — substantial but not extreme — because the binding obligations this reading demands are real (Loss and Damage Fund, historical-baseline accounting proposals) but their enforcement mechanism remains partial: many developed-nation commitments are honored inconsistently and the fund's initial capitalization was a small fraction of estimated need. Suppression is moderate (0.52) and rising, reflecting the treaty mechanisms (COP consensus rules, review cycles, NDC ratchets) used to compel developed-nation compliance, though no supranational enforcement body can compel payment. Theater ratio is elevated and oscillating (peaking near COP15/Copenhagen 2009 at 0.50, dipping post-Paris 2015, rising again into the 2022-2024 loss-and-damage era) because a persistent share of activity is negotiation-as-performance — headline pledges exceeding realized transfers. Accessibility collapse is low-moderate (0.38): alternative framings (voluntary reading, market-based mechanisms) remain fully live and contested, unlike a mountain where alternatives have collapsed. Resistance is high (0.72) because developed-nation payers actively contest the binding historical-baseline framing in every negotiating cycle.
 *
 * PERSPECTIVAL GAP:
 *   From the vulnerable-state seat, this arrangement is coordination correcting a historical asymmetry — a rope, or at least a rope-shaped remedy for accumulated extraction that occurred before the treaty regime existed. From the developed-nation treasury seat, the same binding historical-baseline accounting is experienced as retroactive liability imposed for choices made by prior generations under different global knowledge conditions, and enforced through diplomatic and reputational suppression rather than a coordination benefit they perceive receiving. The engine's per-seat computation should reflect this asymmetry: beneficiary seats compute closer to rope, payer seats closer to tangled_rope or snare, given the real coordination function (a shared atmospheric commons genuinely required international action) coexisting with real asymmetric extraction (specific liability assigned by historical accounting that payer states did not individually negotiate at the time of emitting).
 *
 * DIRECTIONALITY LOGIC:
 *   Small island states, least developed countries, and vulnerable coastal populations are structural beneficiaries under this reading: their directionality sits near the beneficiary end because the reading's entire operative logic routes financing and reduction pressure toward them and away from their own binding obligations. Developed-nation treasuries, carbon-intensive industries, and taxpayers sit near the target end: the reading imposes on them the binding costs (constrained exit — they can withdraw from specific instruments but not escape the accounting logic once broadly adopted). Emerging-economy high emitters are neither cleanly beneficiary nor victim under THIS reading's accounting — their current-year emissions are high but their cumulative historical contribution is comparatively low, so the reading's own logic places them outside the binding obligation, which is precisely the structural delta the kernel context specifies relative to the voluntary reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that cumulative historical emitters had already consumed a disproportionate share of a finite atmospheric sink before vulnerable states could act or object — remains empirically live (IPCC, Global Carbon Project corroboration outside both coalition and payer governments), which forecloses classifying this as mandatrophy: the coordination function is not vestigial. Tangled_rope, not snare, is the appropriate claim because both a genuine coordination function (shared-commons management requiring collective action) and asymmetric extraction (binding costs concentrated on specific named payers via a historical formula they cannot renegotiate) are simultaneously present, and active enforcement infrastructure (COP ratchet mechanism, loss and damage fund governance) is required to sustain it against payer resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cbdr_reading_selection_authority,
    'Which reading of the CBDR kernel — historical_responsibility_reading or voluntary_commitment_reading — governs a given treaty instrument, and who has authority to decide between them?',
    'Track COP consensus text across cycles (Kyoto Protocol Annex I/II differentiation vs. Paris Agreement''s nationally-determined-contribution architecture) to see which reading''s operative language survives into binding text at each negotiation.',
    'If the voluntary reading consistently displaces the historical-responsibility reading in binding text, the historical-responsibility reading''s claimed_type as tangled_rope overstates its actual enforcement — it may function more as an aspirational Scaffold that never sunsets into binding form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cbdr_reading_selection_authority, conceptual, 'Ambiguity over which kernel reading actually governs binding treaty text at any given moment.').

omega_variable(
    historical_baseline_calculation_boundary,
    'Should cumulative historical emissions be calculated from the industrial revolution (c. 1750-1850) or from a later date (e.g., 1990, when scientific consensus on anthropogenic warming solidified and nations could be said to have knowingly continued emitting)?',
    'Compare liability outcomes under both baselines against actual negotiating positions; historical baseline choice significantly shifts which nations and eras count as extractive.',
    'An earlier baseline increases developed-nation extraction/liability magnitude substantially; a later baseline narrows it and shifts some liability toward nations industrializing after 1990, altering the victim/beneficiary boundary this reading currently draws.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_baseline_calculation_boundary, conceptual, 'The historical baseline start-date is itself contested within the historical-responsibility reading.').

omega_variable(
    enforcement_capacity_gap,
    'Is the binding character of this reading''s obligations real (enforceable through treaty mechanisms) or largely rhetorical, given the absence of a supranational body that can compel developed-nation payment?',
    'Track realized Loss and Damage Fund disbursements against pledged/needed amounts, and track NDC compliance rates against binding-language commitments over multiple COP cycles.',
    'If realized enforcement remains consistently far below pledged levels, the tangled_rope claim should be reconsidered toward scaffold (transitional coordination not yet backed by real enforcement) or the theater_ratio should be revised upward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_gap, empirical, 'Gap between binding legal language and actual enforcement capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_tr_t1992, cbdr_principle__historical_responsibility_reading, theater_ratio, 1992, 0.3).
narrative_ontology:measurement(cbdr_tr_t1997, cbdr_principle__historical_responsibility_reading, theater_ratio, 1997, 0.38).
narrative_ontology:measurement(cbdr_tr_t2009, cbdr_principle__historical_responsibility_reading, theater_ratio, 2009, 0.5).
narrative_ontology:measurement(cbdr_tr_t2015, cbdr_principle__historical_responsibility_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(cbdr_tr_t2022, cbdr_principle__historical_responsibility_reading, theater_ratio, 2022, 0.35).
narrative_ontology:measurement(cbdr_tr_t2024, cbdr_principle__historical_responsibility_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(cbdr_be_t1992, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1992, 0.28).
narrative_ontology:measurement(cbdr_be_t1997, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1997, 0.35).
narrative_ontology:measurement(cbdr_be_t2009, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2009, 0.42).
narrative_ontology:measurement(cbdr_be_t2015, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(cbdr_be_t2022, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2022, 0.56).
narrative_ontology:measurement(cbdr_be_t2024, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2024, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_su_t1992, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement(cbdr_su_t1997, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(cbdr_su_t2009, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2009, 0.44).
narrative_ontology:measurement(cbdr_su_t2015, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(cbdr_su_t2022, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2022, 0.48).
narrative_ontology:measurement(cbdr_su_t2024, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cbdr_principle__historical_responsibility_reading, 0.12).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, voluntary_commitment_reading).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, loss_and_damage_fund_governance).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, paris_agreement_ndc_ratchet_mechanism).

% DUAL FORMULATION NOTE:
% This story and voluntary_commitment_reading are sibling readings of the same cbdr_principle kernel, sharing the same treaty text (UNFCCC Art. 3.1, Paris Agreement preamble) but instantiating structurally opposed obligations. This reading places developed nations in the victim/payer set for binding financial transfer and emissions-reduction obligations; the voluntary reading removes that binding character and instead centers technology transfer as the primary developed-nation duty, leaving developing nations more exposed to financing gaps. The two stories must be read together to see the full kernel contest; neither one alone represents 'the CBDR principle' as a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
