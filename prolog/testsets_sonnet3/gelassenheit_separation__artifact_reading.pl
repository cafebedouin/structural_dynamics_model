% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__artifact_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__artifact_reading, []).

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
 *   constraint_id: gelassenheit_separation__artifact_reading
 *   human_readable: Ordnung Artifact-Resemblance Ban (Visible Separation Reading)
 *   domain: religious/technology governance
 *
 * SUMMARY:
 *   This story instantiates the artifact_reading of the
 *   gelassenheit_separation kernel: separation is read as requiring visible
 *   distinction from English society, such that technology is forbidden if it
 *   resembles worldly artifacts REGARDLESS of what function it performs or
 *   how it is deployed. Under this reading, an off-grid solar panel is barred
 *   not because it creates dependency on outside power grids (it does not)
 *   but because it visually resembles a technology associated with English
 *   modernity. Likewise, a synthetic-blend fabric is barred not because it
 *   entangles the wearer in commercial supply chains any more than plain
 *   cloth does, but because of its resemblance to worldly dress. The reading
 *   produces maximal suppression because the test is legible and total:
 *   appearance, not function, is dispositive, and appearance-based tests
 *   admit essentially no functional counterargument.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.81).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.88).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Ordnung Artifact-Resemblance Ban (Visible Separation Reading)").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious/technology governance").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, '2643fffd-827d-4cd4-af17-1e932f72b005').
narrative_ontology:cs_kernel_codification('2643fffd-827d-4cd4-af17-1e932f72b005', distributed).
narrative_ontology:cs_authority_grounding('2643fffd-827d-4cd4-af17-1e932f72b005', lineage).
narrative_ontology:cs_interpretation_layer_present('2643fffd-827d-4cd4-af17-1e932f72b005').
narrative_ontology:cs_reading_relation('2643fffd-827d-4cd4-af17-1e932f72b005', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_reading_relation('2643fffd-827d-4cd4-af17-1e932f72b005', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('2643fffd-827d-4cd4-af17-1e932f72b005', foundational, appearance_is_the_locus_of_separation).
narrative_ontology:cs_axiom_status(appearance_is_the_locus_of_separation, holdable).
narrative_ontology:cs_axiom_grounding('2643fffd-827d-4cd4-af17-1e932f72b005', appearance_is_the_locus_of_separation, conventional).
narrative_ontology:cs_axiom('2643fffd-827d-4cd4-af17-1e932f72b005', secondary, function_is_theologically_irrelevant_to_permissibility).
narrative_ontology:cs_axiom_status(function_is_theologically_irrelevant_to_permissibility, holdable).
narrative_ontology:cs_axiom_grounding('2643fffd-827d-4cd4-af17-1e932f72b005', function_is_theologically_irrelevant_to_permissibility, conventional).
narrative_ontology:cs_reference_frame('2643fffd-827d-4cd4-af17-1e932f72b005', visible_nonconformity_as_witness).
narrative_ontology:cs_drift_state('2643fffd-827d-4cd4-af17-1e932f72b005', post_rural_electrification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2643fffd-827d-4cd4-af17-1e932f72b005', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, ordained_ministers).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, bishop_council).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, off_grid_households).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, young_adult_members).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, small_farm_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the Ordnung's technology rules district by district, deciding case-by-case whether a given implement 'looks too English' regardless of what it does or how it is powered. Retains sole interpretive authority; can grant or deny exceptions, and members who are refused have no appeal outside the council's own hierarchy.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, bishop_council, agenda_setter,
    institutional, generational, arbitrage, regional).

% Draw pastoral authority and communal deference from being the recognized custodians of what counts as visibly separate. Their standing depends on the artifact-resemblance standard remaining the operative test, since consequence-based or function-based readings would relocate the discernment work away from clergy toward household-level judgment.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, ordained_ministers, beneficiary,
    institutional, generational, identity_locked, regional).

% Already draw no power from the grid and pose no entanglement risk by any functional measure, yet are forbidden from installing solar panels because panels visually resemble a worldly technological artifact. Must instead run costlier, harder-to-maintain diesel generators or do without, purely to avoid the appearance of a rooftop panel.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, off_grid_households, payer,
    powerless, biographical, constrained, local).

% Face baptism and church membership decisions shaped by whether they can commit to visible-marker compliance for life. Modern synthetic-blend fabrics, even when cheaper and more durable than plain cloth, are barred for looking like English clothing; the appearance rule, not any functional entanglement, becomes a primary sorting mechanism for who stays and who leaves the community.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, young_adult_members, payer,
    powerless, biographical, trapped, local).

% Need efficient, low-maintenance equipment to keep farms viable against rising land costs, but functionally identical tools are approved or rejected based on how closely they resemble English farm machinery in appearance, not on what dependency they create. Operators bear real economic cost from being denied tools that would not connect them to outside systems at all.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, small_farm_operators, payer,
    powerless, biographical, constrained, local).

% Households and ministers elsewhere in the same tradition who hold that separation means avoiding structural entanglement in worldly systems, not avoiding visual resemblance, would argue an off-grid solar panel should be approved on functional grounds. They are not seated in this district's Ordnung interpretation and have no standing to contest the artifact-resemblance test here.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, principle_reading_households, excluded,
    moderate, biographical, constrained, local).

% Sociologists of religion and departed members who document how appearance-based technology rules function as a boundary-maintenance and retention mechanism independent of any stated theological rationale about worldly entanglement.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, outside_scholars_and_former_members, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__artifact_reading, diffuse).
narrative_ontology:fixing_cost_class(gelassenheit_separation__artifact_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright, low-ambiguity, easily-inspected marker of communal boundary that requires no case-by-case functional analysis — anyone can see at a glance whether a barn roof has a panel or a dress is polyester, which keeps enforcement cheap and boundary maintenance visible to the whole community, not just to technical adjudicators.
% TRANSFER_FUNCTION: Moves discretionary interpretive power and pastoral authority to the bishop council and ordained ministry, and moves real economic and practical cost — generator fuel, harder farm labor, higher fabric costs, exit pressure on youth — onto households and individuals who pose no functional entanglement risk by any other measure.
% ABSENT_VOICES: Households within the same tradition who hold the consequence-based or principle-based reading are not represented in this district's Ordnung deliberations; their view that a solar panel poses no entanglement risk once installed off-grid is not entered into this council's discernment process.
% DISAPPEARANCE_RATIONALE: Ministers and the bishop council would say the visible-marker system is essential to communal identity and its removal would dissolve separation into indistinguishability from English society. Off-grid households, young adults on the margin of membership, and outside sociologists would say the world would barely rearrange functionally — the entanglement risks the rule claims to guard against are already absent in these cases — while economic and retention costs would measurably fall.
% FOUNDING_PROBLEM: Early Anabaptist and Amish communities needed a durable, transmissible way to maintain separation from a surrounding society whose norms, employment, and material culture threatened to gradually absorb church members generation over generation.
% FOUNDING_PROBLEM_CORROBORATION: Ordained ministers and the bishop council attest the founding problem of gradual worldly absorption remains fully live and requires the visible-marker standard specifically. Outside sociologists of religion and departed former members, corroborated by comparative accounts from principle-reading and consequence-reading communities within the same broader tradition, attest that the appearance-based test has drifted from guarding against actual entanglement toward guarding clerical interpretive authority and retention leverage over youth — a function distinct from, and sometimes contrary to, the original separation problem.
narrative_ontology:disappearance_verdict(gelassenheit_separation__artifact_reading, contested).
narrative_ontology:founding_problem_status(gelassenheit_separation__artifact_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__artifact_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__artifact_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__artifact_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__artifact_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__artifact_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 at interval end) because the artifact-resemblance test extracts real economic and practical cost from households who pose zero functional entanglement risk by any other standard the tradition holds — the off-grid solar case is the clearest instance: full compliance with the underlying non-entanglement goal, full prohibition anyway. Suppression is authored at the top of the scale (0.88) because there is no functional escape valve; a household cannot argue its way to an exception on non-entanglement grounds, only on appearance grounds, and appearance is the one thing that cannot be functionally mitigated. Theater ratio rises across the interval (0.30 to 0.62) as more of the enforcement activity becomes visibly about maintaining the marker system itself — inspecting rooftops and hemlines — rather than about the entanglement the marker was originally supposed to indicate.
 *
 * DIRECTIONALITY LOGIC:
 *   The bishop council and ordained ministry are the structural beneficiaries: the artifact-resemblance standard is precisely the standard whose administration requires their ongoing interpretive discretion, since it cannot be resolved by household-level functional reasoning the way a principle-reading or consequence-reading standard could be. Off-grid households, young adult members facing baptismal commitment, and small farm operators are the targets: they bear cost that tracks no entanglement risk they actually pose, only a resemblance risk assessed by others. Households elsewhere in the tradition holding the principle or consequence reading are excluded from this district's discernment process entirely — their functional counterarguments are not heard here because the artifact-resemblance test is precisely the reading that forecloses functional counterargument as relevant.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — gradual absorption into English society — was live when material culture and technology adoption reliably tracked assimilation. The artifact-resemblance reading generalizes this into a rule that fires on appearance independent of the underlying mechanism, which is exactly how a coordination device (a legible, cheap-to-enforce boundary marker) accumulates extractive residue: cases arise (off-grid solar, functionally-isolated equipment) where the appearance test and the entanglement test diverge, and the reading resolves every such case in favor of appearance. This is not evidence the founding problem is dead — visible distinction genuinely does support communal cohesion — but it is evidence that this specific reading has decoupled its instrument (appearance) from its stated target (non-entanglement) in a way the sibling principle_reading has not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artifact_vs_principle_boundary_location,
    'Is the boundary between ''visibly worldly artifact'' and ''functionally isolated technology'' a stable category distinction, or does it collapse under scrutiny once off-grid, functionally-isolated cases (solar panels, battery equipment) are examined closely?',
    'Comparative case review across districts holding the artifact_reading versus districts holding the principle_reading, tracking whether functionally identical off-grid installations are approved or denied purely as a function of which reading the local Ordnung applies.',
    'If the category collapses (i.e., the only real difference is appearance, not entanglement), the artifact_reading''s coordination justification becomes almost entirely aesthetic/boundary-marking rather than protective, strengthening the tangled_rope classification toward snare-like territory for the specific case of off-grid households.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artifact_vs_principle_boundary_location, conceptual, 'Whether appearance-based and function-based separation tests are actually tracking different underlying risks or the same one measured differently.').

omega_variable(
    reading_selection_provenance,
    'Why does a given district''s Ordnung council adopt the artifact_reading rather than the principle_reading or consequence_reading of the same kernel, and is that selection itself made by parties who benefit from the interpretive discretion the artifact_reading preserves?',
    'Historical and sociological tracing of which bishop councils adopted stricter appearance-based readings and correlating adoption with retention of centralized interpretive authority versus devolution of technology judgment to household or committee level.',
    'If reading-selection correlates with preservation of clerical interpretive authority rather than with theological argument, this supports reading the artifact_reading as partially self-serving to the agenda_setter seat rather than purely a doctrinal inheritance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_provenance, conceptual, 'Whether the choice among kernel readings is doctrinally driven or authority-preservation driven.').

omega_variable(
    youth_retention_causal_weight,
    'How much of youth attrition (departures during or before baptismal commitment) is causally attributable to the appearance-based technology rules specifically, versus other factors in the separation kernel generally?',
    'Exit interviews and longitudinal retention data comparing districts with strict artifact_reading enforcement against districts with looser consequence_reading or principle_reading enforcement, controlling for other community factors.',
    'High attributable weight would strengthen the case that the artifact_reading''s cost falls disproportionately and specifically on young_adult_members as an extraction mechanism rather than incidental friction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(youth_retention_causal_weight, empirical, 'Causal contribution of the artifact-resemblance rule specifically to youth departure rates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__artifact_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(gela_tr_t8, gelassenheit_separation__artifact_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(gela_tr_t16, gelassenheit_separation__artifact_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(gela_tr_t24, gelassenheit_separation__artifact_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(gela_tr_t32, gelassenheit_separation__artifact_reading, theater_ratio, 32, 0.58).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__artifact_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__artifact_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gela_be_t8, gelassenheit_separation__artifact_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(gela_be_t16, gelassenheit_separation__artifact_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(gela_be_t24, gelassenheit_separation__artifact_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(gela_be_t32, gelassenheit_separation__artifact_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__artifact_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__artifact_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(gela_su_t8, gelassenheit_separation__artifact_reading, suppression_requirement, 8, 0.75).
narrative_ontology:measurement(gela_su_t16, gelassenheit_separation__artifact_reading, suppression_requirement, 16, 0.79).
narrative_ontology:measurement(gela_su_t24, gelassenheit_separation__artifact_reading, suppression_requirement, 24, 0.83).
narrative_ontology:measurement(gela_su_t32, gelassenheit_separation__artifact_reading, suppression_requirement, 32, 0.86).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__artifact_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__artifact_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__artifact_reading, 0.08).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposed from the single natural-language label 'the Ordnung's separation requirement.' Each reading instantiates a structurally distinct constraint with its own epsilon: artifact_reading (this story) authors high extraction and maximal suppression because the appearance test forbids off-grid solar and functionally-isolated equipment regardless of entanglement risk; principle_reading is expected to author substantially lower extraction because functionally-isolated technology is approved on its own terms; consequence_reading is expected to author extraction calibrated to actual community-practice effects (visiting, mutual aid, rootedness) rather than to appearance or abstract entanglement. The three do not share one epsilon — per the epsilon-invariance principle, they are three constraints linked here by network edges, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
