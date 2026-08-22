% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Nation-to-Nation Treaty Framework (Sovereign Equality Reading)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint instantiates the nation-to-nation reading of the
 *   historical treaty substrate: the claim that Indigenous-settler treaties
 *   are binding international agreements between sovereign equals requiring
 *   ongoing consent, subject to modern treaty law principles. It is one
 *   reading of a contested kernel alongside extinguishment (completed
 *   property transactions) and stewardship (relational coexistence pacts).
 *   Under this reading, Indigenous nations enter the beneficiary set as
 *   co-equal sovereigns with territorial consent rights, while settler states
 *   are constrained by international obligations and unilateral resource
 *   extraction becomes a treaty violation. The authored metrics treat the
 *   constraint as a tangled rope: it carries a genuine coordination function
 *   (cross-sovereign peace and shared governance) but operates with
 *   substantial asymmetric extraction through procedural delay, interpretive
 *   capture by settler courts, and consultation theater that channels
 *   Indigenous political energy into state-controlled legal processes.
 *
 * KEY AGENTS:
 *   - Indigenous nations: Primary beneficiary and secondary payer (organized/constrained) â gain formal sovereignty recognition and consent rights, but bear procedural costs and legitimization burdens.
 *   - Settler state governments: Agenda-setter (institutional/constrained) â administer and interpret the treaty framework, bear compliance costs, and capture interpretive authority.
 *   - Resource extraction corporations: Payer (powerful/mobile) â face consent and consultation costs, lobby for narrower interpretations.
 *   - International legal institutions: Observer (analytical/analytical) â supply legitimacy conditions and monitoring without direct administration.
 *   - Radical Indigenous critics: Excluded voice (moderate/identity_locked) â reject the treaty framework as colonial and are marginalized from legal discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.65).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.7).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Nation-to-Nation Treaty Framework (Sovereign Equality Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, '94dbc41b-7298-4ee0-a7cd-d4cd29eab5ee').
narrative_ontology:cs_kernel_codification('94dbc41b-7298-4ee0-a7cd-d4cd29eab5ee', fixed_text).
narrative_ontology:cs_authority_grounding('94dbc41b-7298-4ee0-a7cd-d4cd29eab5ee', lineage).
narrative_ontology:cs_interpretation_layer_present('94dbc41b-7298-4ee0-a7cd-d4cd29eab5ee').
narrative_ontology:cs_reading_relation('94dbc41b-7298-4ee0-a7cd-d4cd29eab5ee', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('94dbc41b-7298-4ee0-a7cd-d4cd29eab5ee', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('94dbc41b-7298-4ee0-a7cd-d4cd29eab5ee', foundational, indigenous_sovereign_equality).
narrative_ontology:cs_axiom_status(indigenous_sovereign_equality, holdable).
narrative_ontology:cs_axiom_grounding('94dbc41b-7298-4ee0-a7cd-d4cd29eab5ee', indigenous_sovereign_equality, deontological).
narrative_ontology:cs_axiom('94dbc41b-7298-4ee0-a7cd-d4cd29eab5ee', foundational, ongoing_consent_obligation).
narrative_ontology:cs_axiom_status(ongoing_consent_obligation, holdable).
narrative_ontology:cs_axiom_grounding('94dbc41b-7298-4ee0-a7cd-d4cd29eab5ee', ongoing_consent_obligation, conventional).
narrative_ontology:cs_reference_frame('94dbc41b-7298-4ee0-a7cd-d4cd29eab5ee', international_sovereign_equality_framework).
narrative_ontology:cs_drift_state('94dbc41b-7298-4ee0-a7cd-d4cd29eab5ee', contemporary_settler_legal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('94dbc41b-7298-4ee0-a7cd-d4cd29eab5ee', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, international_legal_institutions).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_corporations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise nationhood through treaty frameworks that formally recognize sovereign equality and consent rights over territorial changes. Bear the procedural costs of litigation, consultation fatigue, and delayed justice as settler states adopt nation-to-nation vocabulary while resisting its material implications. Bound to the treaty relationship as the primary internationally legitimate avenue for territorial and political claims.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, payer).

% Administer treaty relationships through domestic legal and bureaucratic machinery, controlling the interpretation of consent and consultation. Bear compliance costs and political friction when Indigenous nations successfully assert rights, but retain the capacity to proceduralize and delay implementation. Constrained by international legal rhetoric while maintaining parliamentary sovereignty and plenary power doctrines.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Must obtain Indigenous consent or consultation for projects on treaty territory, facing direct costs from delays, legal negotiations, and potential cancellations. Often lobby for narrower interpretations of consent to reduce uncertainty, and may pass compliance costs to consumers or seek state subsidies.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_corporations, payer,
    powerful, biographical, mobile, regional).

% Monitor and articulate international treaty norms, providing the legal vocabulary and monitoring mechanisms for nation-to-nation relationships. They neither administer nor bear direct costs but shape legitimacy conditions and produce reports that are selectively adopted by domestic courts.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_legal_institutions, observer,
    analytical, civilizational, analytical, global).

% Reject the historical treaty substrate and the nation-to-nation framework as colonial legal instruments that legitimate settler occupation. Argue for Indigenous jurisdiction independent of state treaty processes. Excluded from dominant legal and political discourse which channels all territorial claims into treaty litigation and consultation.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, radical_indigenous_critics, excluded,
    moderate, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mutually recognized legal framework through which sovereign Indigenous nations and settler states can coexist, share territory, and govern resources without resorting to unilateral domination or violent conflict.
% TRANSFER_FUNCTION: Moves territorial governance authority from unilateral settler state control to joint or consent-based decision-making; moves legitimacy from conquest to agreement; moves time and political energy from Indigenous nations into litigation and negotiation processes.
% ABSENT_VOICES: Indigenous nations and thinkers who reject the treaty framework entirely as a colonial imposition, and settler nationalists who reject any Indigenous sovereignty claims, are both excluded from the legal mainstream that channels disputes into treaty interpretation.
% DISAPPEARANCE_RATIONALE: If the nation-to-nation treaty framework vanished, the legal basis for Indigenous territorial consent rights would collapse; settler states would revert to unilateral resource allocation, and Indigenous nations would lose the primary international-law avenue for territorial protection, forcing a fundamental rearrangement of North American and Australasian governance.
% FOUNDING_PROBLEM: How to establish legitimate, peaceful, and mutually binding relationships between distinct sovereign peoples occupying the same territories without a shared overarching authority.
% FOUNDING_PROBLEM_CORROBORATION: Independent comparative constitutional scholars and dissenting settler-state judiciary attest the cross-sovereign coexistence problem remains unresolved. Dominant settler-state executives assert it is settled through domestic constitutional supremacy. Anti-colonial critics outside the treaty-negotiation apparatus argue the framework itself reproduces the problem. No unanimous outside corroboration exists.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects that the nation-to-nation framework channels Indigenous territorial claims into state-controlled litigation and consultation, extracting time, political energy, and legitimacy while delivering incomplete remedies. Suppression (0.70) captures the constraint's active suppression of alternatives such as direct Indigenous jurisdiction or treaty repudiation. Theater ratio (0.58) registers the widespread substitution of procedural consultation for substantive consent. Accessibility collapse (0.60) indicates that once the nation-to-nation frame is accepted, extra-legal or non-treaty avenues for justice lose legitimacy. Resistance (0.50) reflects sustained Indigenous legal mobilization and occasional court victories, offset by state capture of interpretive institutions. The temporal series tracks the performative capture of the framework from 1970-2020.
 *
 * PERSPECTIVAL GAP:
 *   Indigenous nations and settler state governments experience this constraint inversely: from the Indigenous seat, the framework promises sovereign equality but delivers procedural exhaustion and delayed justice; from the settler state seat, the framework offers a legitimacy-generating vocabulary while preserving ultimate interpretive and fiscal control. The engine computes this divergence from the structural data â the dual beneficiary/payer role for Indigenous nations and the agenda_setter role for settler states.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are declared in both beneficiaries and victims because the nation-to-nation reading structurally positions them as beneficiaries of sovereign recognition while the actual operation of the constraint extracts through proceduralization. Settler state governments are not in the beneficiary array because their gains are administrative capture and continued territorial dominance, not direct rent; their role as agenda_setter with constrained exit places them nearer the target end than the operator end, though their institutional power modulates effective extraction downward. Resource extraction corporations are straightforward payers. International legal institutions are observers whose authority is vindicated by the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents the error of treating the nation-to-nation framework as pure extraction (snare) â the coordination function of cross-sovereign peace and mutual recognition is real and historically consequential. It also prevents the error of treating it as pure coordination (rope) â the asymmetric extraction through procedural capture and consultation theater is equally real. If the founding problem of cross-sovereign coexistence were fully solved, the constraint should have atrophied into a simple diplomatic channel; instead, its persistence and intensifying theater indicate it now functions partly to manage Indigenous claims within settler institutional parameters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint best understood as the nation-to-nation reading of the historical treaty substrate, or as the underlying colonial legal structure itself?',
    'Examine whether the classification changes when the referent is shifted from the interpretive principle to the material arrangement of power.',
    'If the referent is the material structure, extraction may read higher and the type may shift toward snare; if the referent is the interpretive principle, coordination functions are more salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Frame ambiguity between reading and material structure').

omega_variable(
    consent_implementation_gap,
    'Does the nation-to-nation framework enforce genuine ongoing consent, or has consultation been substituted for consent in practice?',
    'Comparative case-law analysis tracking the rate at which Indigenous objections are honored versus merely heard across jurisdictions.',
    'If consultation routinely substitutes for consent, the constraint''s theater_ratio understates the gap and the effective extraction on Indigenous nations is higher than measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_implementation_gap, empirical, 'Whether procedural consultation has replaced substantive consent').

omega_variable(
    sovereign_equality_ambiguity,
    'Are Indigenous nations structurally capable of exercising sovereign equality within a treaty framework dominated by settler state courts and parliamentary sovereignty?',
    'Track the extent to which settler domestic courts treat treaties as international versus domestic instruments, and whether Indigenous nations have equal interpretive authority.',
    'If domestic courts retain unilateral interpretive supremacy, the sovereign equality axiom is overridden in practice and the constraint functions as coordination theater masking asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_equality_ambiguity, conceptual, 'Whether sovereign equality is structurally real or doctrinal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hist_tr_t10, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(hist_tr_t20, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(hist_tr_t30, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 30, 0.46).
narrative_ontology:measurement(hist_tr_t40, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hist_be_t10, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(hist_be_t20, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(hist_be_t30, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(hist_be_t40, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 50, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(historical_treaty_substrate__nation_to_nation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, identity_coordination).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the historical_treaty_substrate kernel, decomposed per the Îµ-invariance principle because the kernel label 'historical treaties' conflates structurally distinct claims: extinguishment (transactional cession), nation-to-nation (sovereign equality under international law), and stewardship (relational coexistence). Each reading carries a distinct Îµ, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
