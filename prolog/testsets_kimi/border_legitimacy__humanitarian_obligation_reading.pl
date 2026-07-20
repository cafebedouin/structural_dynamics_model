% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation with Economic Migrant Exclusion
 *   domain: political/migration_law
 *
 * SUMMARY:
 *   This constraint instantiates the humanitarian_obligation_reading of the
 *   contested border_legitimacy kernel. It models the post-1951 Refugee
 *   Convention framework under which states bear an obligation to admit and
 *   not refoule individuals fleeing persecution, while retaining sovereign
 *   discretion to exclude those categorized as economic migrants. The
 *   constraint creates a bifurcated population: recognized refugees receive
 *   coordinated international protection, while economic migrants and
 *   rejected asylum seekers bear the costs of border militarization,
 *   detention, and categorical exclusion. The claim/metric independence is
 *   maintained: the constraint is claimed as a tangled rope (genuine
 *   coordination for refugees, asymmetric extraction from migrants) and the
 *   metrics describe moderate but rising extractiveness driven by enforcement
 *   intensification.
 *
 * KEY AGENTS:
 *   - destination_states: Primary agenda_setter (institutional/constrained) â administers the distinction and enforces borders
 *   - recognized_refugees: Primary beneficiary (powerless/trapped) â receives protection contingent on proof
 *   - economic_migrants: Primary payer/target (powerless/trapped) â bears the costs of categorical exclusion
 *   - rejected_asylum_seekers: Secondary payer (powerless/trapped) â falls through the procedural cracks of the distinction
 *   - humanitarian_institutions: Organizational beneficiary (organized/constrained) â derives mandate from the framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.58).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.72).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation with Economic Migrant Exclusion").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political/migration_law").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, '1df87038-5222-4111-93a8-77001e8c2068').
narrative_ontology:cs_kernel_codification('1df87038-5222-4111-93a8-77001e8c2068', formalized).
narrative_ontology:cs_authority_grounding('1df87038-5222-4111-93a8-77001e8c2068', lineage).
narrative_ontology:cs_interpretation_layer_present('1df87038-5222-4111-93a8-77001e8c2068').
narrative_ontology:cs_reading_relation('1df87038-5222-4111-93a8-77001e8c2068', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('1df87038-5222-4111-93a8-77001e8c2068', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_axiom('1df87038-5222-4111-93a8-77001e8c2068', foundational, persecution_based_admission_obligation).
narrative_ontology:cs_axiom_status(persecution_based_admission_obligation, holdable).
narrative_ontology:cs_axiom_grounding('1df87038-5222-4111-93a8-77001e8c2068', persecution_based_admission_obligation, conventional).
narrative_ontology:cs_reference_frame('1df87038-5222-4111-93a8-77001e8c2068', state_sovereignty_with_humanitarian_exception).
narrative_ontology:cs_drift_state('1df87038-5222-4111-93a8-77001e8c2068', contemporary_mixed_migration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1df87038-5222-4111-93a8-77001e8c2068', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, recognized_refugees).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, humanitarian_institutions).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, rejected_asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer asylum determination, operate border controls, and negotiate bilateral readmission agreements. They enforce the categorical distinction between refugees and economic migrants through legislation, physical infrastructure, and carrier sanctions. They bear the cost of refugee admission and processing but retain sovereign control over labor markets and fiscal outlays through the exclusion of economic migrants.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, destination_states, agenda_setter,
    institutional, generational, constrained, global).

% Receive legal protection and admission when they successfully meet the refugee definition under the 1951 Convention. Their safety depends on state recognition of the obligation and their ability to prove persecution. They cannot return home and have no alternative pathway to status if the refugee framework collapses.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, recognized_refugees, beneficiary,
    powerless, biographical, trapped, global).

% Derive institutional mandate and operational funding from the refugee protection framework created by this constraint. They administer camps, conduct status determination in some contexts, and advocate for adherence to the convention. They do not control the border regime but their organizational existence is tethered to its continuation.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, humanitarian_institutions, beneficiary,
    organized, generational, constrained, global).

% Are categorically excluded from admission despite fleeing poverty, climate stress, or generalized violence that falls outside the persecution definition. They bear the costs of border militarization, detention, family separation, debt to smugglers, and death during irregular crossing. No legal pathway exists for their admission under this framework.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, biographical, trapped, global).

% Are individuals who may have fled persecution but fail to prove it within the procedural and evidentiary standards of the asylum system, or who are excluded by safe-third-country rules and narrow interpretations. They end up in the same excluded position as economic migrants, facing detention, destitution, or forced return after having sought protection.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, rejected_asylum_seekers, payer,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared international framework for protecting individuals fleeing persecution, creating a standardized obligation for states to admit non-citizens who meet the refugee definition and preventing refoulement to persecution.
% TRANSFER_FUNCTION: Moves the obligation to admit and protect from destination states to recognized refugees who meet the legal definition; moves the costs of border exclusion, categorical denial, and irregularized migration from destination states to economic migrants and rejected asylum seekers.
% ABSENT_VOICES: Economic migrants are formally present in the system only as excluded persons with no seat at the norm-setting table; frontline border communities, smuggled migrants, and origin-state labor unions are structurally excluded from the legal conversation that defines the categories.
% DISAPPEARANCE_RATIONALE: If the obligation to admit refugees and the categorical exclusion of economic migrants both vanished, global migration governance would reorganize around entirely different logics: the refugee protection architecture would dissolve, border enforcement would lose its primary legal justification, and millions would move or be protected under alternative arrangements.
% FOUNDING_PROBLEM: Post-WWII displacement crisis and the need to prevent repetition of states returning refugees to persecution, while preserving state control over general migration flows and labor markets.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and humanitarian institutions attest the refugee protection problem remains live. Independent migration researchers, the International Organization for Migration, and Global South states attest that the founding problem has shifted: the categorical distinction now serves to exclude those fleeing structural violence and climate disaster who fall outside the persecution definition, and the arrangement persists as much for sovereign control as for humanitarian protection.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint genuinely coordinates protection for recognized refugees while asymmetrically extracting mobility, life chances, and family unity from economic migrants and rejected asylum seekers. Suppression is high (0.72) because the distinction requires active border enforcement, carrier sanctions, extraterritorial processing, and pushbacks to hold. Theater is moderate (0.40): asylum procedures have become increasingly performative, with procedural fairness masking narrowing recognition rates and externalized deterrence. Accessibility collapse (0.60) reflects the marginalization of open-border alternatives in mainstream policy discourse. Resistance (0.50) captures migrant irregularization strategies, sanctuary movements, and some state non-compliance. The temporal series share one grid, showing enforcement intensification and extraction accumulation from interval start to end.
 *
 * PERSPECTIVAL GAP:
 *   From the destination_states seat, the constraint appears as a hard-won compromise between humanitarian duty and sovereign prerogative â a coordination mechanism that prevents refoulement while preserving order. From the economic_migrants and rejected_asylum_seekers seats, the same structure operates as violent categorical exclusion that legitimates border deaths and detention through procedural formality. From the recognized_refugees seat, it is precarious protection that depends on successfully performing victimhood before a skeptical bureaucracy. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Recognized refugees are declared beneficiaries with trapped exit (no return, no alternative status), placing them at the full-beneficiary end of the directionality spectrum. Economic migrants and rejected asylum seekers are declared victims with trapped exit (border closures, detention, deportation), placing them at the full-target end. Destination_states are agenda_setters with constrained exit (bound by convention but able to derogate, externalize, or narrow interpretation), sitting nearer the beneficiary side because they capture sovereign control and labor-market closure from the exclusion function. Humanitarian_institutions are beneficiaries with constrained exit (mandate tied to the regime), receiving institutional subsidy from the constraint's existence.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two opposed errors. Mislabeling the constraint as pure rope would ignore the asymmetric extraction from economic migrants and the active enforcement required to maintain the categorical exclusion. Mislabeling it as pure snare would erase the genuine coordination function that has protected millions from refoulement. The reading is specifically not a scaffold because it carries no sunset clause â the 1951 Convention has no expiration, and the enforcement infrastructure has become permanent. It is not a piton because the beneficiary set (destination states and humanitarian institutions) still profits sufficiently from the arrangement to maintain it actively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persecution_economics_boundary_sustainability,
    'Can the persecution/economics distinction be maintained empirically when climate disaster, state collapse, and structural violence produce mixed migration motives that do not fit the convention''s categories?',
    'Comparative analysis of asylum recognition rates across jurisdictions with similar origin-country conditions; empirical studies of migrant motive heterogeneity; tracking of climate-displacement jurisprudence.',
    'If the distinction collapses empirically, the constraint''s bifurcation becomes arbitrary, shifting the reading toward a snare (pure exclusion under humanitarian cover) or requiring expansion to a broader humanitarian scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecution_economics_boundary_sustainability, empirical, 'Empirical sustainability of the refugee/migrant categorical boundary').

omega_variable(
    enforcement_cost_or_extraction,
    'Does the enforcement apparatus required to maintain the economic-migrant exclusion represent necessary coordination cost or extractive overhead?',
    'Cost analysis of border externalization, deterrence policies, and detention infrastructure versus the administrative cost of status determination and refugee resettlement alone.',
    'If enforcement costs substantially exceed coordination needs, measured extractiveness rises and the tangled rope classification tilts toward snare; if proportionate, the extraction remains moderate and the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_or_extraction, conceptual, 'Whether border enforcement is coordination cost or extractive overhead').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bl_hum_tr_t0, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bl_hum_tr_t10, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(bl_hum_tr_t20, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(bl_hum_tr_t30, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(bl_hum_tr_t40, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(bl_hum_tr_t55, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 55, 0.38).
narrative_ontology:measurement(bl_hum_tr_t70, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 70, 0.4).

% Extraction over time
narrative_ontology:measurement(bl_hum_be_t0, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bl_hum_be_t10, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(bl_hum_be_t20, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(bl_hum_be_t30, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(bl_hum_be_t40, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(bl_hum_be_t55, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 55, 0.54).
narrative_ontology:measurement(bl_hum_be_t70, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 70, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bl_hum_su_t0, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(bl_hum_su_t10, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(bl_hum_su_t20, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(bl_hum_su_t30, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(bl_hum_su_t40, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(bl_hum_su_t55, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 55, 0.68).
narrative_ontology:measurement(bl_hum_su_t70, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 70, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, freedom_of_movement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the border_legitimacy kernel. The kernel decomposes into at least three structurally distinct claims: sovereignty_reading (territorial exclusion as legitimate right), humanitarian_obligation_reading (obligation to admit refugees but not economic migrants), and freedom_of_movement_reading (borders presumptively illegitimate for all). Each reading has a distinct epsilon, beneficiary/victim structure, and classification. This reading models the post-1951 Refugee Convention framework and its enforcement requirements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
