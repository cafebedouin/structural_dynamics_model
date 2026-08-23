% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__repudiation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Versailles Reparations Repudiation Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The repudiation reading of the Versailles reparations clauses asserts
 *   that the treaty was imposed under duress (threat of renewed blockade and
 *   occupation) and is therefore void ab initio under international law. This
 *   reading was championed by German nationalist parties throughout the
 *   Weimar period and became state policy under the Nazi regime from 1933. It
 *   functions as a constraint that actively suppresses Allied creditor claims
 *   while coordinating German domestic mobilization for rearmament. The
 *   claimed type is tangled_rope: genuine coordination for German national
 *   recovery (beneficiaries: German state, taxpayers, military-industrial
 *   complex) combined with asymmetric extraction from Allied creditors and
 *   bondholders (victims), maintained through active enforcement (propaganda,
 *   legal denunciation, military buildup, diplomatic pressure).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.85).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.9).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Reparations Repudiation Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, 'bb09c3e2-73ad-4a84-b312-442c82e5e982').
narrative_ontology:cs_kernel_codification('bb09c3e2-73ad-4a84-b312-442c82e5e982', formalized).
narrative_ontology:cs_authority_grounding('bb09c3e2-73ad-4a84-b312-442c82e5e982', extraction).
narrative_ontology:cs_interpretation_layer_present('bb09c3e2-73ad-4a84-b312-442c82e5e982').
narrative_ontology:cs_reading_relation('bb09c3e2-73ad-4a84-b312-442c82e5e982', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('bb09c3e2-73ad-4a84-b312-442c82e5e982', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_axiom('bb09c3e2-73ad-4a84-b312-442c82e5e982', foundational, treaties_under_duress_are_void).
narrative_ontology:cs_axiom_status(treaties_under_duress_are_void, holdable).
narrative_ontology:cs_axiom_grounding('bb09c3e2-73ad-4a84-b312-442c82e5e982', treaties_under_duress_are_void, conventional).
narrative_ontology:cs_axiom('bb09c3e2-73ad-4a84-b312-442c82e5e982', secondary, german_rearmament_is_sovereign_right).
narrative_ontology:cs_axiom_status(german_rearmament_is_sovereign_right, holdable).
narrative_ontology:cs_axiom_grounding('bb09c3e2-73ad-4a84-b312-442c82e5e982', german_rearmament_is_sovereign_right, deontological).
narrative_ontology:cs_reference_frame('bb09c3e2-73ad-4a84-b312-442c82e5e982', versailles_treaty_as_imposed_settlement).
narrative_ontology:cs_drift_state('bb09c3e2-73ad-4a84-b312-442c82e5e982', nazi_consolidation_1933_1935, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('bb09c3e2-73ad-4a84-b312-442c82e5e982', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_state).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_taxpayers).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_military_industrial_complex).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, allied_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, private_bondholders).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, reparations_commission).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_political_leadership).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, treaties_under_duress_are_void).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, national_sovereignty_absolves_external_debt).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, rearmament_is_sovereign_prerogative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Weimar right-wing parties and later Nazi regime advocate and implement repudiation; uses 'duress' narrative to unify domestic politics, justify rearmament, and consolidate power; bears political risk of Allied retaliation but controls state apparatus
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_political_leadership, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__repudiation_reading, german_political_leadership, beneficiary).

% Relieved of crushing reparations burden that consumed 2-3% of GDP annually; benefit from redirected fiscal capacity to domestic recovery, social programs, and rearmament; constrained exit as individuals cannot opt out of national fiscal policy
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_taxpayers, beneficiary,
    organized, biographical, constrained, national).

% Directly profits from rearmament enabled by repudiation; gains massive state contracts, expanded production, and technological development; mobile exit as firms could serve other states if regime falls but deeply embedded in German political economy
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_military_industrial_complex, beneficiary,
    powerful, generational, mobile, national).

% France, UK, Belgium, Italy lose expected reparations revenue critical for war debt repayment to US and domestic reconstruction; bear cost of German rearmament threat; constrained exit as individual enforcement requires coalition unity that fractures over time
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_creditor_states, payer,
    institutional, generational, constrained, continental).

% Hold German government bonds and reparations securities issued under Dawes/Young Plans; lose principal and interest when Germany defaults; trapped exit with no sovereign enforcement mechanism and secondary markets collapsing to pennies on the mark
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, private_bondholders, payer,
    organized, biographical, trapped, global).

% Inter-allied administrative body tasked with collecting and supervising payments; rendered impotent by German non-compliance and Allied disunity; trapped exit as mandate dissolves with systematic non-compliance and loss of great power backing
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, reparations_commission, agenda_setter,
    institutional, generational, trapped, continental).

% League of Nations framework and treaty law system; repudiation undermines credibility of enforced settlements and creates precedent for unilateral treaty termination; analytical seat observes structural precedent for future revisionist powers
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, international_legal_order, observer,
    institutional, civilizational, analytical, universal).

% Dependents in Allied nations whose pensions were funded by reparations receipts; structurally excluded from repudiation narrative; trapped exit with no political voice and total dependence on state redistribution
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_war_widows_orphans, excluded,
    powerless, biographical, trapped, continental).

% Weimar democrats, socialists, and pacifists who opposed rearmament and sought fulfillment of treaty obligations through negotiation; excluded from repudiation narrative as 'traitors'; identity-locked as their self-concept is bound to republican legality that the repudiation destroys
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_pacifists_republicans, excluded,
    moderate, biographical, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates German national recovery and rearmament by providing a unifying narrative that rejects the Versailles settlement as illegitimate, enabling fiscal and military mobilization across class and regional lines
% TRANSFER_FUNCTION: Moves the burden of war costs from Germany (taxpayers, economy) to Allied creditor states and private bondholders, by nullifying the legal obligation to pay approximately 132 billion gold marks in reparations
% ABSENT_VOICES: War widows and orphans in Allied nations who depended on reparations-funded pensions; colonial subjects whose labor and resources were extracted to service German debts; German pacifists and republicans who opposed rearmament and sought treaty compliance - all structurally excluded from the repudiation narrative
% DISAPPEARANCE_RATIONALE: Repudiation is the active constraint suppressing creditor claims and enabling German fiscal-military freedom; its removal reactivates the Versailles payment machinery, forces German budgetary retrenchment, slows rearmament, and alters the European security balance that led to WWII
% FOUNDING_PROBLEM: The Versailles Treaty imposed a reparations burden that exceeded German economic capacity and was negotiated under threat of continued blockade and occupation, creating a legitimacy crisis for the Weimar Republic that extremist parties exploited
% FOUNDING_PROBLEM_CORROBORATION: John Maynard Keynes (The Economic Consequences of the Peace, 1919) attested the burden was unsustainable; the Dawes Committee (1924) and Young Committee (1929) formally restructured payments based on German capacity; the Lausanne Conference (1932) effectively ended reparations by international agreement with only token final payment - all sources outside the German nationalist beneficiary set
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__repudiation_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is very high (0.85) because the constraint nullifies ~132 billion gold marks in obligations, transferring the entire war cost burden to creditors. Suppression is extreme (0.90) because maintaining repudiation required not just diplomatic refusal but active dismantling of the reparations commission, default on Dawes/Young bonds, and ultimately military remilitarization of the Rhineland to prevent enforcement. Theater ratio is moderate (0.40): the 'duress' legal argument has genuine doctrinal basis (Vienna Convention Art. 52), but the Nazi regime's implementation far exceeded any good-faith interpretation, using repudiation as cover for expansionist rearmament. Accessibility collapse is high (0.75) for creditors - once Germany commits to total non-payment, legal and diplomatic alternatives collapse. Resistance is high (0.80) reflecting Allied diplomatic, economic, and ultimately military opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the German political leadership seat, the constraint appears as rope/mountain: a legitimate coordination mechanism throwing off an illegitimate imposition. From Allied creditor seats, it appears as snare: pure extraction suppressing their legal claims. From bondholder seats, it is snare with no coordination veneer. The engine computes this divergence from the structural data - the same constraint is coordination for one coalition, extraction for another.
 *
 * DIRECTIONALITY LOGIC:
 *   German political leadership is the structural agenda-setter and beneficiary (d near 0.0 - constraint subsidizes their power). German taxpayers and military-industrial complex are beneficiaries with constrained/mobile exit (d ~0.15-0.25). Allied creditor states are payers with constrained exit - they need coalition unity to enforce (d ~0.75). Private bondholders are trapped payers with no exit (d ~0.95). Reparations commission is trapped agenda-setter of the old regime (d ~0.90). International legal order is analytical observer (d ~0.50). Excluded voices (Allied dependents, German republicans) are trapped/identity-locked payers of the constraint's externalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unpayable burden under duress) was substantially resolved by 1932 through the Dawes/Young restructurings and Lausanne Conference - the economic crisis that birthed the repudiation narrative was addressed by capacity-based payments, not total rejection. The repudiation reading persisted beyond its founding problem's resolution because it had been captured by the Nazi regime as an instrument for rearmament and expansion. This is mandatrophy: the coordination function (economic relief) atrophied, but the constraint persisted and intensified as an extraction tool for a new purpose (military mobilization). The theater ratio rise from 0.20 to 0.45 tracks this transition from genuine grievance to performative cover for aggression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the repudiation reading a distinct constraint from the kernel''s other readings, or a rhetorical stance on the same constraint?',
    'Apply epsilon-invariance test: if measuring the constraint via ''German fiscal burden'' gives low ε but ''Allied creditor recovery'' gives high ε, they are distinct constraints. The repudiation reading''s ε is assessed against the standing Versailles arrangement from the reading''s own lights (high extraction of German resources), while its implementation creates a NEW constraint with high extraction FROM Allies.',
    'If distinct, the repudiation reading instantiates its own constraint story with ε ≈ 0.85 (suppression of creditor claims). If same constraint, the kernel''s ε would be observer-relative, violating ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the repudiation reading constitutes a separate constraint from the kernel''s other readings per ε-invariance principle').

omega_variable(
    suppression_mechanism_allies,
    'Is the suppression of Allied creditor claims structural (military/diplomatic power) or internalized (Allied willingness to accept defaults)?',
    'Post-1933 trajectory: if Allied resistance collapses without German military action (e.g., 1935 Rhineland non-response), suppression has internalized component. If resistance persists until military conquest, primarily structural.',
    'If internalized, effective suppression is higher than structural measure suggests - Allies carry the suppression with them after exit opportunities vanish. Affects χ computation for allied_creditor_states seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_allies, empirical, 'Structural vs. internalized suppression mechanism for Allied creditors').

omega_variable(
    coordination_extraction_boundary,
    'Is the German coordination function (national recovery, rearmament) genuine or a cover for extraction from Allies?',
    'Counterfactual: if Germany had repudiated but NOT rearmed, would domestic coordination persist? The 1932 Lausanne near-termination of reparations without rearmament suggests coordination function was real but captured.',
    'If genuine coordination captured, constraint is tangled_rope. If coordination was always pretext, constraint is snare. Affects claimed_type and Boltzmann floor for identity_coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether German national coordination was genuine function or extraction cover').

omega_variable(
    founding_problem_timing,
    'Did the founding problem (unsustainable burden) die in 1932 (Lausanne) or persist until 1933 (Nazi repudiation)?',
    'Economic history: German reparations payments effectively ended at Lausanne (1932) with only 3 billion gold marks token final payment. The Nazi repudiation (1933) repudiated even this token. Compare fiscal burden pre/post Lausanne.',
    'If founding problem died in 1932, mandatrophy is confirmed - constraint persisted 7 years after its justification vanished. If founding problem persisted, mandatrophy weaker. Affects founding_problem_status and theater_ratio interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_timing, empirical, 'Timing of founding problem resolution relative to full repudiation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(versailles_repudiation_tr_t0, versailles_reparations_clauses__repudiation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(versailles_repudiation_tr_t5, versailles_reparations_clauses__repudiation_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(versailles_repudiation_tr_t10, versailles_reparations_clauses__repudiation_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(versailles_repudiation_tr_t13, versailles_reparations_clauses__repudiation_reading, theater_ratio, 13, 0.35).
narrative_ontology:measurement(versailles_repudiation_tr_t14, versailles_reparations_clauses__repudiation_reading, theater_ratio, 14, 0.4).
narrative_ontology:measurement(versailles_repudiation_tr_t16, versailles_reparations_clauses__repudiation_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(versailles_repudiation_tr_t20, versailles_reparations_clauses__repudiation_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(versailles_repudiation_be_t0, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(versailles_repudiation_be_t5, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(versailles_repudiation_be_t10, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(versailles_repudiation_be_t13, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 13, 0.7).
narrative_ontology:measurement(versailles_repudiation_be_t14, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 14, 0.8).
narrative_ontology:measurement(versailles_repudiation_be_t16, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 16, 0.85).
narrative_ontology:measurement(versailles_repudiation_be_t20, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(versailles_repudiation_su_t0, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(versailles_repudiation_su_t5, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(versailles_repudiation_su_t10, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(versailles_repudiation_su_t13, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 13, 0.8).
narrative_ontology:measurement(versailles_repudiation_su_t14, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 14, 0.85).
narrative_ontology:measurement(versailles_repudiation_su_t16, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 16, 0.9).
narrative_ontology:measurement(versailles_repudiation_su_t20, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__repudiation_reading, 0.08).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition: versailles_reparations_clauses splits into three readings with divergent ε. Punitive reading: ε high for Germany (extraction). Limited reading: ε moderate (coordinated burden-sharing). Repudiation reading: ε high for Allies (extraction of creditor claims). The three readings form a constraint family linked by mutual exclusion and historical succession.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__repudiation_reading, institutional, 0.75).
constraint_indexing:directionality_override(versailles_reparations_clauses__repudiation_reading, organized, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
