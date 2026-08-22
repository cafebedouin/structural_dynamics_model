% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__repudiation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__repudiation_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Versailles Reparations Clauses â Repudiation Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Treaty of Versailles (1919) imposed on Germany the obligation to pay
 *   war reparations to the Allied powers, grounded in Article 231's war-guilt
 *   clause. This constraint story instantiates the repudiation_reading of the
 *   versailles_reparations_clauses kernel: the reading that holds the treaty
 *   was imposed under duress and Germany consequently bears no binding
 *   obligation beyond token gestures. From this reading's perspective, the
 *   standing arrangement â the reparations regime enforced from 1919 to
 *   1932 â was a coercive extraction structure masquerading as a peace
 *   settlement. The story authors high extraction and suppression metrics to
 *   reflect the massive resource transfer and military enforcement (Ruhr
 *   occupation 1923), while acknowledging the regime's formal coordination
 *   function (ending the war) to capture the structural complexity that
 *   prevents simple snare classification. The metrics and claimed type are
 *   authored independently: the claim is tangled_rope because the regime
 *   combined genuine peace-coordination with asymmetric extraction, while the
 *   metrics describe an operation that grew increasingly theatrical as
 *   enforcement capacity collapsed after 1929.
 *
 * KEY AGENTS:
 *   - allied_reparation_commission: Primary agenda-setter (institutional/mobile) â administered enforcement and distribution
 *   - german_reich: Primary target (powerful/trapped) â bore the legal obligation under threat of military occupation
 *   - french_republic: Primary beneficiary (powerful/mobile) â received the largest share and demanded rigorous enforcement
 *   - british_empire: Secondary beneficiary (powerful/mobile) â collected transfers but shifted toward moderation and cancellation
 *   - german_taxpayers_and_industry: Diffuse payer (organized/trapped) â actually generated the transferred wealth through taxes and output
 *   - german_revolutionary_nationalists: Excluded voice (moderate/trapped) â demanded total repudiation, absent from commission frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.82).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.85).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Reparations Clauses â Repudiation Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, 'ea2b3e3e-f067-4031-9a63-7f52932b1c69').
narrative_ontology:cs_kernel_codification('ea2b3e3e-f067-4031-9a63-7f52932b1c69', formalized).
narrative_ontology:cs_authority_grounding('ea2b3e3e-f067-4031-9a63-7f52932b1c69', lineage).
narrative_ontology:cs_interpretation_layer_present('ea2b3e3e-f067-4031-9a63-7f52932b1c69').
narrative_ontology:cs_reading_relation('ea2b3e3e-f067-4031-9a63-7f52932b1c69', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('ea2b3e3e-f067-4031-9a63-7f52932b1c69', versailles_reparations_clauses__limited_responsibility_reading, influences).
narrative_ontology:cs_axiom('ea2b3e3e-f067-4031-9a63-7f52932b1c69', foundational, duress_voids_all_obligation).
narrative_ontology:cs_axiom_status(duress_voids_all_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ea2b3e3e-f067-4031-9a63-7f52932b1c69', duress_voids_all_obligation, deontological).
narrative_ontology:cs_axiom('ea2b3e3e-f067-4031-9a63-7f52932b1c69', secondary, reparation_commission_lacks_legitimate_authority).
narrative_ontology:cs_axiom_status(reparation_commission_lacks_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('ea2b3e3e-f067-4031-9a63-7f52932b1c69', reparation_commission_lacks_legitimate_authority, conventional).
narrative_ontology:cs_reference_frame('ea2b3e3e-f067-4031-9a63-7f52932b1c69', sovereign_consent_framework).
narrative_ontology:cs_drift_state('ea2b3e3e-f067-4031-9a63-7f52932b1c69', interwar_enforcement_peak, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ea2b3e3e-f067-4031-9a63-7f52932b1c69', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, french_republic).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, british_empire).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, allied_reparation_commission).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_reich).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_taxpayers_and_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the collection and distribution of German reparations under the treaty. Set payment schedules, verified transfers, and adjudicated disputes about German capacity. Composed of representatives from creditor powers and derived its authority from the treaty text.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_reparation_commission, agenda_setter,
    institutional, generational, mobile, continental).

% Legally obligated to deliver reparations under the treaty. Faced military occupation of industrial territories when deliveries fell short, as in the Franco-Belgian occupation of the Ruhr in 1923. Attempted to balance domestic reconstruction with transfer obligations, eventually resorting to foreign borrowing to fund payments.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_reich, payer,
    powerful, generational, trapped, national).

% Primary beneficiary of reparations as the power whose northern industrial regions had suffered direct wartime devastation. Insisted on rigorous enforcement including military occupation, and collected reparations directly as well as through the Commission.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, french_republic, beneficiary,
    powerful, generational, mobile, national).

% Recipient of reparations shares, though increasingly concerned that excessive extraction destabilized European trade and debt service to the United States. Shifted toward moderation and eventual cancellation, treating reparations as a diplomatic instrument rather than a revenue source.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, british_empire, beneficiary,
    powerful, generational, mobile, national).

% Bore the actual cost of reparations through taxation, inflation, and output extraction. During the Ruhr occupation, engaged in government-funded passive resistance that destroyed savings through hyperinflation. No individual or firm could exit the national obligation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_taxpayers_and_industry, payer,
    organized, biographical, trapped, national).

% Demanded total repudiation of the treaty and all reparations. Structurally excluded from the Reparation Commission frameworks, which assumed a baseline of German compliance and negotiated only the schedule of payments, not their legitimacy.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_revolutionary_nationalists, excluded,
    moderate, immediate, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a formal mechanism to end the state of war and transfer resources from the defeated power to rebuild devastated Allied territories, replacing unilateral post-conflict seizure with a scheduled legal obligation.
% TRANSFER_FUNCTION: Moved gold, foreign currency, coal, steel, and manufactured goods from the German state and economy to the French Republic, British Empire, and other Allied powers, with the Allied Reparation Commission controlling the schedule and distribution.
% ABSENT_VOICES: German nationalists and anti-treaty constituencies were present in domestic politics but structurally excluded from Commission frameworks, which treated compliance as given. American commercial interests dependent on European recovery, and future German generations bearing the credit stigma, had no direct seat.
% DISAPPEARANCE_RATIONALE: Without the reparations clauses, the Allied powers would have lacked their primary revenue stream for war-damage repair and inter-Allied debt service; German fiscal and monetary policy would have operated without transfer constraint; and the entire interwar circular flow of American loans funding German reparations funding Allied debt service would never have arisen.
% FOUNDING_PROBLEM: How to finance the reconstruction of war-devastated regions, compensate civilian damage, and stabilize Allied public finances after four years of total war, without collapsing the defeated economies entirely.
% FOUNDING_PROBLEM_CORROBORATION: John Maynard Keynes, writing as an independent observer in The Economic Consequences of the Peace (1919), attested that the reparations demands exceeded any reasonable reconstruction need and would destabilize Europe. Later British Treasury memoranda and American diplomatic assessments corroborated that the Dawes and Young Plans had transformed the mechanism into circular debt service rather than genuine reconstruction finance.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__repudiation_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__repudiation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__repudiation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the regime transferred roughly 2-3 percent of German national income annually at peak, denominated in foreign currency during a collapsed economy, constituting a massive extraction of surplus. Suppression (0.85) is driven by the London Ultimatum (1921), the Ruhr occupation by French and Belgian troops (1923), and the Dawes Plan's international financial oversight, which collectively prevented German exit. Theater_ratio (0.45) reflects that while the economic transfers were real, an increasing share of diplomatic activity after 1924 concerned maintaining the fiction of German willingness and capacity to pay rather than actual recovery of war costs. Accessibility_collapse (0.70) is high because Germany's only exit options were total default (inviting occupation) or hyperinflation (destroying domestic savings), both catastrophic. Resistance (0.85) is among the highest in the corpus because German policy from 1919 to 1932 was a continuous sequence of passive resistance, negotiated revision, and covert default.
 *
 * PERSPECTIVAL GAP:
 *   From the Commission's seat, the Dawes and Young Plans represented technical adjustments to make a sustainable coordination mechanism work. From the German payer seats, those same plans were simply rescheduling of an illegitimate debt. The engine computes this divergence from the structural asymmetry in power and exit: institutional agenda-setters with mobile exit versus trapped national payers.
 *
 * DIRECTIONALITY LOGIC:
 *   The Allied creditor powers and the Reparation Commission sit at the beneficiary end: they collected the transfers, set the rules, and could exit by renouncing claims (low d). The German Reich and German taxpayers sit at the target end: they paid, had no unilateral exit short of national catastrophe, and experienced the constraint as coercive extraction (high d). The divergence is extreme because the same institutional structure that coordinated peace also enforced a unilateral extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â financing post-war reconstruction and stabilizing Europe â was substantially dead by 1924, when private American loans began circulating to fund German transfers in a circular flow. Yet the constraint persisted until 1932 because the Allied powers needed the political cover of German payments to service their own war debts to the United States. This is classic mandatrophy: the original coordination rationale expired, and the constraint became a theatrical transfer mechanism whose real function was circular debt service.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duress_nullification_validity,
    'Does the coercion exercised at Versailles (blockade continuation, threat of renewed invasion) legally and morally nullify the treaty obligations, or merely render them politically illegitimate while remaining enforceable under positive international law?',
    'Historical jurisprudence on treaties concluded under duress; analysis of pre-UN legal scholarship and subsequent state practice on coerced agreements.',
    'If duress fully nullifies, the constraint is illegitimate extraction ab initio, pushing classification toward snare; if duress only creates political illegitimacy without legal voidness, the constraint remains a tangled rope with asymmetric enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_nullification_validity, conceptual, 'Whether duress voids the obligation or merely delegitimizes it').

omega_variable(
    german_capacity_vs_strategic_default,
    'To what extent was German non-payment driven by genuine economic incapacity versus strategic repudiation intended to force treaty revision?',
    'Comparative economic analysis of German fiscal capacity 1919-1923 versus transfer burdens; diplomatic archives on German government deliberations.',
    'If capacity existed, the extraction was sustainable and the repudiation reading overstates the case; if incapacity was genuine, the regime was structurally non-viable and functioned as coercive extraction regardless of German intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(german_capacity_vs_strategic_default, empirical, 'Economic capacity versus strategic default in German resistance').

omega_variable(
    kernel_reading_boundary,
    'Does the repudiation reading''s zero-obligation premise logically foreclose the limited responsibility reading, or can both coexist as alternative negotiating postures within a single diplomatic framework?',
    'Analysis of Weimar-era diplomatic records to determine whether the same actors advanced both positions simultaneously or sequentially as strategic alternatives.',
    'If foreclosed, the repudiation reading is a hard binary rejecting all obligation; if coexistent, it is one end of a revisionist spectrum, affecting how the kernel''s constraint family is modeled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between repudiation and limited responsibility readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 0, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t0, versailles_reparations_clauses__repudiation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(vers_tr_t2, versailles_reparations_clauses__repudiation_reading, theater_ratio, 2, 0.35).
narrative_ontology:measurement(vers_tr_t4, versailles_reparations_clauses__repudiation_reading, theater_ratio, 4, 0.5).
narrative_ontology:measurement(vers_tr_t6, versailles_reparations_clauses__repudiation_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement(vers_tr_t8, versailles_reparations_clauses__repudiation_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(vers_tr_t10, versailles_reparations_clauses__repudiation_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(vers_tr_t12, versailles_reparations_clauses__repudiation_reading, theater_ratio, 12, 0.6).
narrative_ontology:measurement(vers_tr_t13, versailles_reparations_clauses__repudiation_reading, theater_ratio, 13, 0.8).

% Extraction over time
narrative_ontology:measurement(vers_be_t0, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(vers_be_t2, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 2, 0.9).
narrative_ontology:measurement(vers_be_t4, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 4, 0.88).
narrative_ontology:measurement(vers_be_t6, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(vers_be_t8, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 8, 0.7).
narrative_ontology:measurement(vers_be_t10, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(vers_be_t12, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(vers_be_t13, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 13, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t0, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(vers_su_t2, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 2, 0.9).
narrative_ontology:measurement(vers_su_t4, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 4, 0.95).
narrative_ontology:measurement(vers_su_t6, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(vers_su_t8, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(vers_su_t10, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(vers_su_t12, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(vers_su_t13, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 13, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, limited_responsibility_reading).

% DUAL FORMULATION NOTE:
% The versailles_reparations_clauses kernel decomposes into three structurally distinct constraints: punitive_liability_reading (high obligation), limited_responsibility_reading (bounded obligation), and repudiation_reading (no obligation). Each reading carries a different Îµ, beneficiary structure, and normative premise. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
