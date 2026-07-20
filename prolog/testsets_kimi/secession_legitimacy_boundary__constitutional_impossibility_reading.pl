% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__constitutional_impossibility_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Constitutional Impossibility of Unilateral Secession (Federal Authority Reading)
 *   domain: political economy / federalism / resource politics
 *
 * SUMMARY:
 *   The constitutional impossibility reading of the secession legitimacy
 *   boundary holds that unilateral secession by constituent units is
 *   categorically impermissible under the federal constitutional order, and
 *   that territorial change is legitimate only through negotiated
 *   constitutional amendment. In resource-rich federations, this rule
 *   operates as a structural lock-in: resource-producing regions are bound to
 *   the center by legal and coercive machinery that forecloses exit, while
 *   the center and fiscally dependent regions benefit from retained
 *   territorial integrity and fiscal flows. This constraint story models the
 *   constitutional rule as instantiated in political economies where
 *   federalism and resource extraction intersect; it is authored as one
 *   reading of a contested kernel, with sibling readings that locate
 *   sovereignty in popular majorities, grievance thresholds, or indigenous
 *   treaty rights.
 *
 * KEY AGENTS:
 *   - federal_government: Primary beneficiary/agenda_setter (institutional/arbitrage) â controls amendment agenda and captures resource rents
 *   - constitutional_judiciary: Secondary agenda_setter (institutional/analytical) â interprets constitutional silence as prohibition
 *   - resource_rich_constituent_units: Primary target (powerful/trapped) â bear extraction through foregone independence and resource redistribution
 *   - secessionist_political_movements: Secondary target (moderate/trapped) â face legal suppression and criminalization
 *   - fiscal_transfer_recipient_regions: Secondary beneficiary (organized/constrained) â receive redistributed resources from union maintenance
 *   - excluded_confederal_reformers: Excluded voice (moderate/constrained) â advocate for constitutional reform but are outside the amendment window
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.72).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Impossibility of Unilateral Secession (Federal Authority Reading)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political economy / federalism / resource politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, '5af6b60b-b6fe-42b9-b29d-db87c6c347fa').
narrative_ontology:cs_kernel_codification('5af6b60b-b6fe-42b9-b29d-db87c6c347fa', formalized).
narrative_ontology:cs_authority_grounding('5af6b60b-b6fe-42b9-b29d-db87c6c347fa', lineage).
narrative_ontology:cs_interpretation_layer_present('5af6b60b-b6fe-42b9-b29d-db87c6c347fa').
narrative_ontology:cs_reading_relation('5af6b60b-b6fe-42b9-b29d-db87c6c347fa', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('5af6b60b-b6fe-42b9-b29d-db87c6c347fa', secession_legitimacy_boundary__grievance_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('5af6b60b-b6fe-42b9-b29d-db87c6c347fa', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('5af6b60b-b6fe-42b9-b29d-db87c6c347fa', foundational, unilateral_secession_categorically_impermissible).
narrative_ontology:cs_axiom_status(unilateral_secession_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('5af6b60b-b6fe-42b9-b29d-db87c6c347fa', unilateral_secession_categorically_impermissible, conventional).
narrative_ontology:cs_axiom('5af6b60b-b6fe-42b9-b29d-db87c6c347fa', foundational, constitutional_amendment_sole_legitimate_path).
narrative_ontology:cs_axiom_status(constitutional_amendment_sole_legitimate_path, holdable).
narrative_ontology:cs_axiom_grounding('5af6b60b-b6fe-42b9-b29d-db87c6c347fa', constitutional_amendment_sole_legitimate_path, conventional).
narrative_ontology:cs_reference_frame('5af6b60b-b6fe-42b9-b29d-db87c6c347fa', constitutional_federal_supremacy).
narrative_ontology:cs_drift_state('5af6b60b-b6fe-42b9-b29d-db87c6c347fa', contemporary_secessionist_mobilization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5af6b60b-b6fe-42b9-b29d-db87c6c347fa', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, fiscal_transfer_recipient_regions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_rich_constituent_units).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_political_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the constitutional amendment agenda, appoints judges, commands security forces, and collects fiscal revenues from all constituent regions. Frames the prohibition on unilateral secession as essential to national unity and constitutional supremacy, while capturing resource rents from regions that would otherwise exit.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Interprets constitutional silence or ambiguity on secession as implied prohibition. Issues rulings that invalidate secessionist legislation and frame the amendment process as the only legitimate path to territorial change, thereby administering the constraint's enforcement.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Generate disproportionate resource rents that are redistributed through federal fiscal arrangements. Seek greater autonomy or independence but are blocked by the constitutional impossibility of unilateral secession. Bear the cost of foregone self-determination and captured resource wealth.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, resource_rich_constituent_units, payer,
    powerful, generational, trapped, regional).

% Political movements and parties advocating independence for their region. Face legal prohibition, disqualification from office, and criminal penalties under constitutional and penal frameworks that render unilateral secession categorically impermissible.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_political_movements, payer,
    moderate, generational, trapped, regional).

% Receive fiscal transfers and federal program benefits funded in part by resource-rich regions. Benefit from the constitutional rule that prevents secession of donor regions and maintains the redistributive federal framework.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, fiscal_transfer_recipient_regions, beneficiary,
    organized, biographical, constrained, regional).

% Constitutional scholars and political actors who advocate for explicit constitutional recognition of secession procedures or confederal arrangements. Their proposals are structurally excluded from the constitutional amendment agenda because the dominant reading treats unilateral secession as categorically impermissible and non-negotiable outside amendment.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, excluded_confederal_reformers, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__constitutional_impossibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents disorderly territorial fragmentation and maintains the economic and political integrity of the federal union by ensuring any territorial change occurs only through consensual constitutional amendment.
% TRANSFER_FUNCTION: Moves resource wealth and political autonomy from secessionist-prone constituent units to the federal center and to fiscally dependent regions, by legally foreclosing unilateral exit.
% ABSENT_VOICES: Secessionist political movements in resource-rich regions and confederal constitutional reformers are formally present in politics but structurally excluded from the constitutional amendment agenda; their preference for unilateral or negotiated non-amendment exit is categorically ruled illegitimate by the constitutional framework.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, resource-rich regions could unilaterally declare independence, the federal fiscal and resource-transfer system would collapse, and the territorial boundaries of the state would be subject to renegotiation under radically different power conditions.
% FOUNDING_PROBLEM: The fragility of federal unions and the risk of violent or economically catastrophic breakup following unilateral declarations of independence by constituent units.
% FOUNDING_PROBLEM_CORROBORATION: Federal governments and constitutional courts attest the problem remains live, citing historical precedents of civil war and economic disruption. Secessionist movements and external constitutional scholars attest the problem has evolved into a mechanism for central resource extraction; independent comparative federalism studies from outside the beneficiary set note that orderly secession procedures in other federations have avoided violent breakup without producing the feared chaos.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint captures resource-rich regions in a fiscal union against their expressed preferences, transferring wealth to the federal center and net beneficiary regions. Suppression (0.72) is higher than extractiveness because the rule's persistence depends on active judicial enforcement, criminalization of secessionist advocacy, and potential security response. Theater ratio (0.30) reflects moderate performative maintenance: constitutional courts frame rulings in technical legal language, but the 'indivisible nation' rhetoric often exceeds the functional coordination need. Accessibility collapse (0.80) is high because once the constitutional framework is accepted, unilateral exit disappears as a legal possibility entirely. Resistance (0.50) captures sustained but contained secessionist mobilization. The measurement series tracks rising extraction and suppression as resource politics intensified over the interval, with theater spiking during constitutional crises.
 *
 * PERSPECTIVAL GAP:
 *   The federal government and judiciary experience this constraint as necessary coordination to prevent balkanization and protect interdependent economies; their seats compute toward rope or mountain framing. The resource-rich constituent units and secessionist movements experience the same structure as extraction and enclosure; their seats compute toward snare or tangled_rope. The engine produces this divergence from identical structural data because directionality inverts extractiveness for beneficiaries and amplifies it for trapped targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government and fiscal_transfer_recipient_regions are declared beneficiaries: the federal government gains resource rents and territorial control; recipient regions gain fiscal stability. Their exit options (arbitrage, constrained) and beneficiary status drive directionality toward the low-extraction end. Resource_rich_constituent_units and secessionist_political_movements are declared victims: they bear the costs of foregone self-determination and captured wealth. Their trapped exit status and victim classification drive directionality toward the full-target end. The constitutional_judiciary is an agenda_setter without direct material benefit; its directionality is structurally intermediate, reflecting authority maintenance rather than rent collection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing chaotic breakup of federal unions â was plausibly live at the constraint's origin. However, in the contemporary interval, the persistence of absolute prohibition in resource-politics contexts suggests mandate drift: the coordination function (orderly change) is used to justify an arrangement that now primarily serves extraction (resource capture). The T17 abductive trigger would fire on the rising base_extractiveness series, generating the hypothesis that a coordination scaffold has accumulated extraction. The mandatrophy is contested: beneficiaries claim the breakup risk remains live; victims and external scholars claim the problem has been superseded by democratic maturity and that amendment-only exit is now a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the constitutional impossibility reading foreclose alternative legitimacy conditions (popular sovereignty, grievance threshold, treaty primacy) or merely coexist with them as parallel constitutional traditions?',
    'Jurisprudential analysis of whether a single constitutional framework can incorporate both amendment supremacy and unilateral or sovereignty-based exit conditions without logical contradiction.',
    'If foreclosed, the constitutional_impossibility reading is a closed commitment system; if coexistent, it is one competing reading among many within a pluralist constitutional order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship between constitutional impossibility and sibling readings').

omega_variable(
    coordination_extraction_boundary,
    'Is the constitutional impossibility rule structurally separable from the resource-extraction function observed in its operation, or is federal territorial integrity inseparable from central resource capture?',
    'Comparative analysis of federations with and without constitutional secession prohibitions: if resource-rich units in prohibition federations show systematically worse fiscal terms than in secession-tolerant federations, the extraction frame is supported; if prohibition federations show superior stability without extraction asymmetry, the coordination frame is supported.',
    'Would reclassify between rope/tangled_rope and snare, shifting the victim set from contested to determinate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal and coercive barriers to secession) or internalized (national identity formation that makes secession unthinkable regardless of legal rules)?',
    'Post-crisis suppression trajectory: if secessionist mobilization collapses solely due to legal penalties, structural; if it collapses despite legal opportunity due to identity shift, internalized.',
    'If internalized, effective suppression is higher than the structural measure suggests and resistance may be understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in federal secession prohibitions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(sece_tr_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 32, 0.32).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(sece_be_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(sece_su_t32, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four competing readings of the secession_legitimacy_boundary kernel. The constitutional_impossibility reading treats federal authority as absolute and secession as categorically impermissible without amendment; the sibling readings instantiate alternative sovereignty claims (popular, grievance-based, treaty-based) that are structurally incompatible with this reading's core premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
