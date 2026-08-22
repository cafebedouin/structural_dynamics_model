% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone Directive — Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   Along the Sanriku coast, stone stelae cut after the 1896 and 1933
 *   tsunamis instruct descendants not to build below the remembered
 *   inundation line; the Aneyoshi stone is the most famous. This story
 *   instantiates the commemorative_husk_reading of the
 *   aneyoshi_stone_commitment kernel: the standing arrangement — the stones,
 *   their cultural-property designation, their upkeep, signage, observances,
 *   and tourist circulation — is assessed as a commitment whose directive has
 *   decoupled from land-use behavior. On this reading, siting decisions
 *   across the postwar decades were made on the independent bases of family
 *   plots, harbor access, and later seawall confidence; the hamlet's 2011
 *   survival reflects topography and settlement history rather than
 *   obedience; and the stone operates as a museum piece. The epsilon referent
 *   is the standing arrangement itself — the husk apparatus as it exists —
 *   assessed by this reading's lights: it commands funds, attention, and
 *   belief-compliance while performing none of its directive's content. The
 *   sibling reading (behavioral_competence_reading) assesses the same
 *   referent as a live rule and authors a far lower epsilon; the two stories
 *   are separate files linked by network.affects_constraints. Claim and
 *   metrics are independent: the claimed type is what this reading takes the
 *   structure to be; the metrics describe its observed operation.
 *
 * KEY AGENTS:
 *   - municipal_heritage_authority: agenda-setting administrator (institutional/arbitrage) — runs the designation, controls the narrative, could reactivate or retire the directive
 *   - regional_tourism_operators: beneficiary (organized/mobile) — monetizes the stones' post-2011 fame
 *   - village_elder_keepers: beneficiary (moderate/identity_locked) — perform the observance as inherited duty
 *   - coastal_hazard_zone_households: primary target (powerless/constrained) — carry unpriced tsunami exposure the directive no longer signals
 *   - municipal_taxpayers: payer (moderate/constrained) — fund upkeep of a directive that governs nothing
 *   - setback_policy_advocates: excluded voice (organized/constrained) — outside the heritage process that administers the stones
 *   - disaster_memory_researchers: analytical observer (analytical/analytical) — reconstruct inundation lines and attribute survival outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.7).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.3).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Tsunami Stone Directive — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, '68aab48f-e930-47c7-809e-e85b24e98397').
narrative_ontology:cs_kernel_codification('68aab48f-e930-47c7-809e-e85b24e98397', fixed_text).
narrative_ontology:cs_authority_grounding('68aab48f-e930-47c7-809e-e85b24e98397', lineage).
narrative_ontology:cs_interpretation_layer_present('68aab48f-e930-47c7-809e-e85b24e98397').
narrative_ontology:cs_reading_relation('68aab48f-e930-47c7-809e-e85b24e98397', aneyoshi_stone_commitment__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('68aab48f-e930-47c7-809e-e85b24e98397', foundational, directive_decoupled_from_land_use_decisions).
narrative_ontology:cs_axiom_status(directive_decoupled_from_land_use_decisions, holdable).
narrative_ontology:cs_axiom_grounding('68aab48f-e930-47c7-809e-e85b24e98397', directive_decoupled_from_land_use_decisions, empirically_contingent).
narrative_ontology:cs_axiom('68aab48f-e930-47c7-809e-e85b24e98397', secondary, survival_attributable_to_non_compliance_factors).
narrative_ontology:cs_axiom_status(survival_attributable_to_non_compliance_factors, holdable).
narrative_ontology:cs_axiom_grounding('68aab48f-e930-47c7-809e-e85b24e98397', survival_attributable_to_non_compliance_factors, empirically_contingent).
narrative_ontology:cs_reference_frame('68aab48f-e930-47c7-809e-e85b24e98397', binding_land_use_commitment).
narrative_ontology:cs_drift_state('68aab48f-e930-47c7-809e-e85b24e98397', post_2011_media_apotheosis, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('68aab48f-e930-47c7-809e-e85b24e98397', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_heritage_authority).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, regional_tourism_operators).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, village_elder_keepers).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, coastal_hazard_zone_households).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_taxpayers).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__commemorative_husk_reading, commemorative_fulfillment_doctrine).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__commemorative_husk_reading, memorial_preservation_paternalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the stones as designated cultural properties: budgets their upkeep, commissions signage and translations, organizes observances, and controls the official narrative presented to visitors and schools. Could redirect the budget line, rescind the designation, or convert the sites into an explicit hazard-education program; instead renews the existing form each cycle. Draws staffing justification, prestige, and grant eligibility from the designation.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_heritage_authority, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_heritage_authority, beneficiary).

% Run tours, lodging, and merchandise keyed to the 'tsunami stones' fame that followed international media coverage. Their itineraries treat the stones as heritage attractions; nothing in their operations depends on the directive governing construction. If attention shifts, they can repackage around other sites.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, regional_tourism_operators, beneficiary,
    organized, immediate, mobile, regional).

% A handful of hamlet elders who sweep the stones, lay offerings, and recite the inscription at anniversaries, understanding the duty as inherited from ancestors who buried kin in 1896 and 1933. Their standing in the community rests on keeping the observance; handing it off or declaring it finished would break a chain they experience as constitutive of who they are. They recount the hamlet's survival as vindication of the ancestors' word.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, village_elder_keepers, beneficiary,
    moderate, generational, identity_locked, local).

% Households along the Sanriku coast whose dwelling and workplace locations reflect family plots, harbor access, and road economics rather than any remembered inundation line. Where markers survive nearby, they register as scenery or school-trip stops. Moving inland means leaving fishing livelihoods and ancestral graves; staying means carrying tsunami exposure that no living arrangement prices or signals. After 2011 many such households lost kin in settlements that had comparable stones.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, coastal_hazard_zone_households, payer,
    powerless, generational, constrained, regional).

% Fund preservation works, signage, and event programming through municipal budgets and heritage grants. The line item is small enough that contesting it costs more than paying it; no electoral coalition forms around either expanding or ending it.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_taxpayers, payer,
    moderate, biographical, constrained, regional).

% Disaster-safety planners and engineers who argue for enforceable setback lines and land-use regulation in tsunami zones. They sit outside the cultural-properties process that governs the stones; their proposals would require converting a revered monument back into operative law, which the heritage framing makes politically awkward. They publish recommendations the commemorative administration does not answer.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, setback_policy_advocates, excluded,
    organized, generational, constrained, national).

% Comparative scholars of disaster memory who document where markers coincide with protective siting and where they do not, reconstruct inundation lines, and attribute survival outcomes. They bear none of the arrangement's costs and collect none of its gains; their accounts circulate in journals and documentaries rather than in the administration's programming.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_memory_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_heritage_authority).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes communal remembrance of the 1896 and 1933 tsunamis: fixes a shared site and date for mourning, anchors hamlet identity and intergenerational transmission, and supplies a legible heritage attraction. It no longer coordinates anything about where buildings go.
% TRANSFER_FUNCTION: Moves public maintenance funds and civic attention from municipal taxpayers and residents to the heritage-administration apparatus and the regional tourism economy; moves no land, no siting decisions, and no protection — the directive transfers nothing behavioral.
% ABSENT_VOICES: Setback-policy advocates and hazard-zone households are absent from the cultural-properties process that administers the stones; they would object that commemoration without enforceable siting rules launders risk into heritage. The dead of 1896, 1933, and 2011 — for whom the directive was first cut — have no seat; their interest is represented only by the keepers' narration.
% DISAPPEARANCE_RATIONALE: Land-use patterns would not shift — no decision currently routes through the directive. What would rearrange is the commemorative economy: observances would lapse, the heritage budget line and its staffing would dissolve, tour itineraries would lose their anchor stop, and the elder keepers would lose the practice that organizes their standing and calendar.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami killed roughly twenty-two thousand people and the 1933 Showa Sanriku tsunami killed thousands more, survivors needed a way to make catastrophe memory outlive the witnesses: to fix the inundation line in terrain and language so descendants would site dwellings above it without ever having seen the water.
% FOUNDING_PROBLEM_CORROBORATION: Meiji- and Showa-era prefectural reconstruction records and contemporaneous newspaper appeals attest the founding intent from outside any benefiting party. Regional scholarship documenting that numerous Sanriku settlements built below comparable markers — and the 2011 casualty geography, concentrated below remembered inundation lines in settlements other than Aneyoshi — corroborates, from outside the keeper and heritage seats, that the siting function lapsed. The hamlet head's account of compliance comes from inside the beneficiary set and is treated here as testimony, not corroboration.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.70) because the arrangement's costs are paid against near-zero functional output: upkeep funds, civic attention, and — dominantly — a complacency premium, the belief that remembrance is discharged, which displaces live risk governance. Suppression is low-moderate (0.30): the husk coerces no one; its crowding of alternatives is inertial (heritage framing makes reactivation politically awkward), not enforced. Theater ratio is high (0.80): sweeping, signage, translation, tours, and anniversary recitation constitute the arrangement's activity, and none of it touches building location. Accessibility collapse is low (0.35): enforceable setbacks, insurance, drilling, and relocation subsidies all remain legally available — nothing forecloses them; they are merely crowded out. Resistance is low (0.20): a directive that demands nothing meets no resistance; the historical wave of building below markers was defiance of a live rule, not of the husk. The three temporal series share one seven-point grid (1933–2011): extractiveness and theater rise monotonically as the engineered-seawall era and then the heritage-tourism era substitute proxies for the directive; suppression_requirement falls as the normative enforcement machinery — headman authority, communal pressure on siting — eroded into ceremony. The falling suppression series is the enforcement-decay signature, which is why it is tracked despite the static-looking end state; the end-state scalar matches the series terminus.
 *
 * PERSPECTIVAL GAP:
 *   The keeper and heritage seats should compute as low-extraction beneficiaries: from inside, the arrangement is living heritage, an honored debt to ancestors, and its costs are devotion. The hazard-zone household and taxpayer seats should compute as targets: from outside, the same stones are scenery adjacent to unpriced lethal exposure, and the upkeep is a fee for a service not rendered. The researcher seat sees both at once. The engine computes these divergent per-seat classifications from the structural data; this story does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the heritage authority (arbitrage exit, institutional power) sits nearest the subsidized end — it draws budget, staffing, and prestige from the arrangement it administers; tourism operators monetize attention and can leave; elder keepers collect identity continuity and cannot leave. Victim declarations map to high directionality: hazard-zone households bear the complacency premium with constrained exit (fishing livelihoods and ancestral graves tie them to the coast), and taxpayers fund upkeep with no compensating service. The structural derivation chain handles every seat; no directionality overrides are declared.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — keep dwellings above the water's reach — is dead under this reading: the arrangement no longer attempts it, and the mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges is the zombie signature the genealogy battery exists to catch. Classifying the husk as pure predation would be wrong: nothing is coerced, no exit is suppressed, and the arrangement would not survive a determined attempt to repurpose it — it persists because no one is hurt enough to fix it and its keepers are fused to it. Classifying it as live protective coordination would be equally wrong: its surviving coordination content is remembrance, not protection, and the protection it commemorates is precisely what it fails to deliver. The piton claim locates the truth between: an atrophied mandate maintained by inertia and performance, with a small captured material stream (upkeep funds accruing to the administering seat) riding on a large uncaptured one (the complacency premium, which accrues to no seat at all).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_siting_causality,
    'Is this constraint the commemorative husk reading or the behavioral competence reading of the aneyoshi_stone_commitment kernel — that is, did any postwar siting decision in the affected settlements route through the stone directive?',
    'Micro-historical reconstruction: building permits, lot transaction records, and oral histories of relocation decisions coded against stelae elevation lines; a siting decision demonstrably attributable to the directive''s presence would instantiate the sibling reading.',
    'If behavioral force is found, this file''s high epsilon, piton claim, and husk axioms fail and the sibling''s low-extraction protective-rule structure stands; if decisions were independent of the directive, the husk reading holds and the sibling''s premise is overridden by evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_siting_causality, empirical, 'Which reading of the aneyoshi stone kernel instantiates the actual constraint.').

omega_variable(
    survival_attribution_2011,
    'Was Aneyoshi''s survival in the 2011 tsunami caused by directive-guided siting, or by topography, settlement history, and the configuration of the 2011 runup — factors that would spare the hamlet regardless of any directive?',
    'Counterfactual geomorphic comparison: reconstructed 2011 runup elevations against dwelling elevations, benchmarked against destroyed neighboring settlements that had comparable markers; survival explained by elevation alone supports the husk attribution.',
    'Topographic/luck attribution confirms this reading''s high epsilon (the stone''s fame is retrospective myth-making); a demonstrated compliance margin would transfer credit to the directive and collapse this reading''s extraction estimate toward the sibling''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_attribution_2011, empirical, 'Whether 2011 survival evidences compliance or confounds it.').

omega_variable(
    complacency_premium_realism,
    'Does symbolic observance of the stones measurably displace live risk governance — insurance uptake, evacuation drilling, setback advocacy, relocation — or are remembrance and risk management orthogonal in these communities?',
    'Comparative panel of Sanriku municipalities with and without prominent markers, scored on preparedness indicators before and after 2011; a negative association between marker prominence and preparedness would size the premium.',
    'A sized premium validates the cognitive component of epsilon and the target status of hazard-zone households; orthogonality would strip epsilon to mere upkeep costs and move the classification toward benign neglect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complacency_premium_realism, empirical, 'Magnitude of the belief-level cost the husk imposes on governed populations.').

omega_variable(
    decay_versus_transformation_framing,
    'Is the arrangement a decayed protective commitment (a husk of what the directive was) or a successfully transformed institution whose criterion was always remembrance-continuity rather than siting control?',
    'Criterion specification from the founding record: if 1896/1933-era framings tie the stone''s purpose explicitly to dwelling placement, decay is the right frame; if they tie it to memory-keeping as such, transformation is.',
    'Under the transformation frame the arrangement is a functioning remembrance coordinator with low extraction and the classification moves toward rope; under the decay frame the piton reading stands. The framing choice moves epsilon by a wide margin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_versus_transformation_framing, conceptual, 'Framing under-determination: husk-of-a-rule versus succeeded-at-something-else.').

omega_variable(
    keeper_identity_fusion_depth,
    'Do the elder keepers'' benefits reflect genuine coordination value of the observance, or identity fusion that would prevent honest decommissioning even if the community judged the husk worthless?',
    'Cohort succession study: whether younger hamlet members accept keeper roles when offered; rapid decline would indicate cohort-specific fusion rather than durable communal value.',
    'Fusion explains persistence independent of value and predicts the husk outliving its last defenders; genuine value would support the transformation frame in decay_versus_transformation_framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(keeper_identity_fusion_depth, empirical, 'Identity-lock mechanism sustaining the observance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_husk_tr_t0, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t0, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t13, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 13, 0.18).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t13, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t26, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 26, 0.28).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t26, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t39, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 39, 0.4).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t39, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t52, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 52, 0.52).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t52, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t65, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 65, 0.66).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t65, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t78, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 78, 0.8).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(aneyoshi_husk_be_t0, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t0, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t13, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 13, 0.24).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t13, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t26, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 26, 0.34).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t26, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t39, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 39, 0.45).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t39, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t52, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 52, 0.54).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t52, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t65, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 65, 0.62).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t65, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t78, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 78, 0.7).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t78, observed).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_husk_su_t0, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t0, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t13, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 13, 0.5).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t13, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t26, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 26, 0.46).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t26, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t39, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 39, 0.42).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t39, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t52, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 52, 0.38).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t52, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t65, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 65, 0.34).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t65, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t78, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 78, 0.3).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t78, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Aneyoshi tsunami stone' conflates two structurally distinct claims about one kernel: that the directive constrained siting behavior (behavioral_competence_reading) and that it decayed to symbol (this file). Per the epsilon-invariance principle they are separate constraints with separate epsilon values, beneficiary structures, and classifications, linked here. The upstream reading (claimed behavioral force, the popular post-2011 account) influences the downstream husk assessment because the compliance narrative is the principal evidence cited against decay; this file exists to test that narrative against siting-record evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
