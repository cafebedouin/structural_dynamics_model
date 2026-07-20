% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Treaties as Relational Stewardship Pacts
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the stewardship reading of the
 *   historical treaty substrate kernel: treaties are relational pacts for
 *   shared territorial stewardship that do not cede Indigenous sovereignty,
 *   instead imposing mutual obligations of coexistence and joint resource
 *   governance on the settler state. The constraint operates within
 *   comparative constitutional theory and indigenous legal systems as an
 *   interpretive framework that reallocates territorial authority from
 *   unilateral Crown control to shared stewardship regimes. It is claimed as
 *   tangled_rope because the framework carries a genuine coordination
 *   functionâpreventing violent conflict over territoryâwhile also
 *   generating asymmetric extraction: Crown institutional capacity typically
 *   dominates joint management apparatuses, and Indigenous nations are drawn
 *   into governance structures that compromise exclusive territorial
 *   decision-making despite preserving foundational sovereignty.
 *
 * KEY AGENTS:
 *   - Indigenous nations (organized/generational/constrained): Primary beneficiaries of territorial jurisdiction recognition, though they bear the costs of shared governance compromise.
 *   - Crown governments (institutional/generational/constrained): Agenda-setters and payers who administer treaty relationships and lose unilateral sovereignty through consent and joint management obligations.
 *   - Resource extraction industry (powerful/biographical/mobile): Payers who lose unilateral resource access and face increased regulatory burden under joint management.
 *   - Treaty commissions (institutional/generational/analytical): Observers that interpret and adjudicate treaty disputes within the stewardship frame.
 *   - Extinguishment advocates (moderate/biographical/constrained): Excluded voices marginalized when the stewardship reading governs legal interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.5).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.4).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Treaties as Relational Stewardship Pacts").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, 'ed22e746-3953-444f-9434-a20c49ba5c7a').
narrative_ontology:cs_kernel_codification('ed22e746-3953-444f-9434-a20c49ba5c7a', fixed_text).
narrative_ontology:cs_authority_grounding('ed22e746-3953-444f-9434-a20c49ba5c7a', lineage).
narrative_ontology:cs_interpretation_layer_present('ed22e746-3953-444f-9434-a20c49ba5c7a').
narrative_ontology:cs_reading_relation('ed22e746-3953-444f-9434-a20c49ba5c7a', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('ed22e746-3953-444f-9434-a20c49ba5c7a', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('ed22e746-3953-444f-9434-a20c49ba5c7a', foundational, territorial_sovereignty_non_ceded).
narrative_ontology:cs_axiom_status(territorial_sovereignty_non_ceded, holdable).
narrative_ontology:cs_axiom_grounding('ed22e746-3953-444f-9434-a20c49ba5c7a', territorial_sovereignty_non_ceded, deontological).
narrative_ontology:cs_axiom('ed22e746-3953-444f-9434-a20c49ba5c7a', foundational, mutual_obligation_coexistence).
narrative_ontology:cs_axiom_status(mutual_obligation_coexistence, holdable).
narrative_ontology:cs_axiom_grounding('ed22e746-3953-444f-9434-a20c49ba5c7a', mutual_obligation_coexistence, conventional).
narrative_ontology:cs_reference_frame('ed22e746-3953-444f-9434-a20c49ba5c7a', relational_stewardship_covenant).
narrative_ontology:cs_drift_state('ed22e746-3953-444f-9434-a20c49ba5c7a', contemporary_constitutional_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ed22e746-3953-444f-9434-a20c49ba5c7a', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, crown_governments).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, resource_extraction_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold territorial jurisdiction recognized through treaties as relational stewardship pacts. Retain foundational sovereignty without cession, but must engage in ongoing joint management and shared governance with the Crown over territorial resources, which preserves jurisdiction while requiring continuous negotiation and compromise.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_nations, beneficiary,
    organized, generational, constrained, regional).

% Administer treaty relationships and are constitutionally bound to obtain consent and engage in shared governance over treaty territories. Bears the cost of regulatory processes, consultation requirements, and the loss of unilateral resource allocation authority that the stewardship reading imposes.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, crown_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, crown_governments, payer).

% Commercial developers and extractive firms that lose unilateral access to treaty territories under the stewardship reading. Must negotiate joint management regimes, profit-sharing, or consent protocols, facing increased regulatory uncertainty and operational costs.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, resource_extraction_industry, payer,
    powerful, biographical, mobile, regional).

% Specialized bodies and courts that oversee treaty implementation, interpret disputes between the Crown and Indigenous nations, and attempt to reconcile competing readings of treaty obligations within the stewardship framework.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, treaty_commissions, observer,
    institutional, generational, analytical, national).

% Legal scholars, officials, and political actors who argue for the extinguishment reading of treaties. Structurally marginalized when courts or commissions adopt the stewardship reading as the governing interpretive lens for specific treaty relationships.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, extinguishment_advocates, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a permanent inter-societal framework for shared territorial governance, enabling coexistence between autonomous Indigenous nations and settler states without requiring either party to surrender its foundational sovereignty.
% TRANSFER_FUNCTION: Moves decision-making authority over territorial resources from unilateral Crown control to joint stewardship regimes, and transfers obligations of consultation, consent, and mutual benefit from the settler state into the treaty relationship itself.
% ABSENT_VOICES: Advocates of the extinguishment reading and some Indigenous factions who reject any shared governance framework in favor of exclusive territorial sovereignty are structurally marginalized when courts or commissions adopt the stewardship reading as the governing interpretive lens.
% DISAPPEARANCE_RATIONALE: If the stewardship reading vanished as an operative constraint, treaty territories would revert to unilateral Crown management or extinguishment logics, Indigenous territorial jurisdiction would lose its primary legal shield, and the entire architecture of modern treaty implementation and land-claims agreements would collapse into adversarial sovereignty disputes.
% FOUNDING_PROBLEM: How to establish peaceful coexistence and resource-sharing between incoming settler populations and existing Indigenous nations without annihilating either society or triggering perpetual warfare.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous legal historians and some Crown commissions attest to the founding problem's persistence. However, settler governments often treat the problem as resolved and the treaties as completed transactions. Independent anthropological and historical scholarship outside both the Crown and extractive beneficiary sets corroborates the ongoing nature of the coexistence problem.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.5, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.50 because the stewardship framework has genuine coordination valueâshared governance prevents territorial conflictâbut its implementation is structurally asymmetric: Crown institutions supply the bureaucratic infrastructure for joint management, fund the processes, and retain de facto veto capacity through regulatory capture. Theater_ratio at 0.55 reflects the heavy performative load of reconciliation rhetoric, ceremony, and procedural compliance that masks the persistence of Crown-dominant decision-making. Suppression at 0.40 captures the marginalization of extinguishment and exclusive-sovereignty alternatives within legal forums that adopt the stewardship reading, without fully foreclosing them. Resistance at 0.60 registers sustained Indigenous critique of the stewardship frame as insufficiently decolonizing, alongside extractive-industry lobbying against consent requirements. Accessibility_collapse at 0.45 acknowledges that once the stewardship reading is adopted in a legal forum, alternatives become procedurally difficult to advance, though they remain live in political discourse.
 *
 * PERSPECTIVAL GAP:
 *   The Indigenous nations seat and the Crown seat diverge sharply: Indigenous nations experience the constraint as a shield against extinguishment and a floor for territorial jurisdiction (beneficiary-dominant), while also experiencing its costs through the labor of endless negotiation and the dilution of exclusive decision-making. The Crown experiences the same structure as a constitutional burden that limits parliamentary supremacy over territory and imposes procedural obligations. The extraction-industry seat experiences the constraint as near-pure extraction (high d, high effective Ï). The engine computes these divergences from the structural data rather than adjudicating a single correct view.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are declared beneficiaries because the stewardship reading structurally preserves their territorial jurisdiction against extinguishment and mandates Crown obligations toward them. Their directionality is lowered by beneficiary status but raised by constrained exit options, yielding a moderate d. Crown governments are declared victims/payers because the reading extracts unilateral sovereignty and resource allocation authority from them; their institutional power and constrained exit yield a high d toward the target end. The resource extraction industry is a pure target (high d). Treaty commissions are analytical observers (analytical exit, neutral d).
 *
 * MANDATROPHY ANALYSIS:
 *   The stewardship reading prevents mislabeling in two directions. Without the coordination-function gate, one might classify it as a snare (pure extraction) because Crown institutions dominate implementation; but the genuine coordination functionâpreventing territorial warfare and establishing shared governanceâblocks that classification. Conversely, without the victim/asymmetry gate, one might classify it as a rope (pure coordination) because of the mutual-obligation rhetoric; but the structural data showing Crown resistance to obligations, extraction-industry losses, and Indigenous critique of co-optation force the recognition of embedded extraction. Tangled_rope is the only category that admits both the relational covenant and the power asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    joint_management_parity,
    'Is the joint management apparatus under the stewardship reading structurally biased toward Crown institutional procedures, funding, and veto capacity?',
    'Empirical audit of decision-making authority, funding flows, and dispute resolution outcomes in existing joint management regimes under modern treaties.',
    'If Crown capacity dominates, the stewardship reading operates as a tangled rope where Indigenous nations are coordinated into their own dilution; if parity exists, the extraction is symmetric and the reading leans toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(joint_management_parity, empirical, 'Whether joint management is structurally asymmetric.').

omega_variable(
    extinguishment_foreclosure,
    'Does the stewardship reading''s core premise of non-cession logically foreclose the extinguishment reading within a single constitutional framework, or can both readings be held as strategic legal positions?',
    'Jurisprudential analysis of whether courts apply both readings to different treaty regimes, and whether a single treaty can be interpreted under both frames simultaneously.',
    'If both can be held, the forecloses relation is invalid and the kernel functions as a distributed authority; if not, the stewardship reading structurally displaces extinguishment wherever it is adopted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extinguishment_foreclosure, conceptual, 'Logical exclusivity of stewardship and extinguishment readings.').

omega_variable(
    crown_obligation_enforceability,
    'Are Crown obligations of consent and shared governance under the stewardship reading enforceable, or does the Crown''s control over courts and enforcement create an irresolvable structural conflict?',
    'Track enforcement rates of treaty obligations against the Crown, independence of treaty tribunals, and outcomes where Indigenous nations seek specific performance of Crown duties.',
    'If unenforceable, the stewardship reading''s extraction is one-sided (Indigenous nations bear the costs of shared governance without reciprocal Crown performance), supporting a snare classification; if enforceable, the constraint maintains genuine mutuality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_obligation_enforceability, empirical, 'Whether Crown-side obligations are structurally enforceable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hts_stew_tr_t0, historical_treaty_substrate__stewardship_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(hts_stew_tr_t8, historical_treaty_substrate__stewardship_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(hts_stew_tr_t16, historical_treaty_substrate__stewardship_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(hts_stew_tr_t24, historical_treaty_substrate__stewardship_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(hts_stew_tr_t32, historical_treaty_substrate__stewardship_reading, theater_ratio, 32, 0.52).
narrative_ontology:measurement(hts_stew_tr_t40, historical_treaty_substrate__stewardship_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(hts_stew_be_t0, historical_treaty_substrate__stewardship_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hts_stew_be_t8, historical_treaty_substrate__stewardship_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(hts_stew_be_t16, historical_treaty_substrate__stewardship_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(hts_stew_be_t24, historical_treaty_substrate__stewardship_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(hts_stew_be_t32, historical_treaty_substrate__stewardship_reading, base_extractiveness, 32, 0.49).
narrative_ontology:measurement(hts_stew_be_t40, historical_treaty_substrate__stewardship_reading, base_extractiveness, 40, 0.5).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(historical_treaty_substrate__stewardship_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, resource_allocation).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% This constraint is the stewardship reading of the historical_treaty_substrate kernel, distinct from the extinguishment reading (which posits cession and completed transactions) and the nation-to-nation reading (which posits international sovereign equality). The epsilon values and beneficiary/victim structures differ across readings because each instantiates a different structural claim about sovereignty transfer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
