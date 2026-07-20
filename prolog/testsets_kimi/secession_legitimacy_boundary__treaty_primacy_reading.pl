% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Treaty Primacy Reading of Secession Legitimacy Boundary
 *   domain: political/legal/federalism
 *
 * SUMMARY:
 *   This constraint instantiates the treaty_primacy_reading of the contested
 *   kernel secession_legitimacy_boundary. The reading holds that Indigenous
 *   treaty rights predate and supersede both federal and provincial
 *   authority, such that no secession is legitimate without treaty holder
 *   consent. It operates as a normative and constitutional boundary on
 *   federal, provincial, and secessionist actors, enforced through judicial
 *   interpretation and political contestation. The constraint is claimed as
 *   tangled_rope: it coordinates competing sovereignty claims by establishing
 *   a hierarchy, but asymmetrically extracts from Indigenous peoples (who
 *   must constantly defend the boundary) and from governments/secessionists
 *   (who lose unilateral authority).
 *
 * KEY AGENTS:
 *   - Indigenous treaty holders (beneficiary/organized/constrained) â collect enhanced veto authority over territorial reconfiguration
 *   - Indigenous peoples broadly (payer/organized/constrained) â bear costs of secession processes that bypass consultation
 *   - Federal Crown (payer/institutional/constrained) â loses unilateral treaty alteration authority
 *   - Provincial governments (payer/institutional/constrained) â lose unilateral territorial and resource authority
 *   - Secessionist movements (payer/organized/constrained) â blocked from majoritarian legitimacy
 *   - Canadian judiciary (agenda_setter/institutional/constrained) â interprets and administers the boundary
 *   - Constitutional scholars (observer/analytical) â analytical seat assessing legitimacy claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.58).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.48).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Treaty Primacy Reading of Secession Legitimacy Boundary").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political/legal/federalism").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, '3cd261cd-4b9a-4ffc-af3a-ae51c2def950').
narrative_ontology:cs_kernel_codification('3cd261cd-4b9a-4ffc-af3a-ae51c2def950', formalized).
narrative_ontology:cs_authority_grounding('3cd261cd-4b9a-4ffc-af3a-ae51c2def950', lineage).
narrative_ontology:cs_interpretation_layer_present('3cd261cd-4b9a-4ffc-af3a-ae51c2def950').
narrative_ontology:cs_reading_relation('3cd261cd-4b9a-4ffc-af3a-ae51c2def950', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('3cd261cd-4b9a-4ffc-af3a-ae51c2def950', secession_legitimacy_boundary__popular_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('3cd261cd-4b9a-4ffc-af3a-ae51c2def950', secession_legitimacy_boundary__grievance_threshold_reading, influences).
narrative_ontology:cs_axiom('3cd261cd-4b9a-4ffc-af3a-ae51c2def950', foundational, treaty_rights_predate_and_supersede_crown_authority).
narrative_ontology:cs_axiom_status(treaty_rights_predate_and_supersede_crown_authority, holdable).
narrative_ontology:cs_axiom_grounding('3cd261cd-4b9a-4ffc-af3a-ae51c2def950', treaty_rights_predate_and_supersede_crown_authority, conventional).
narrative_ontology:cs_axiom('3cd261cd-4b9a-4ffc-af3a-ae51c2def950', foundational, indigenous_consent_required_for_territorial_secession).
narrative_ontology:cs_axiom_status(indigenous_consent_required_for_territorial_secession, holdable).
narrative_ontology:cs_axiom_grounding('3cd261cd-4b9a-4ffc-af3a-ae51c2def950', indigenous_consent_required_for_territorial_secession, deontological).
narrative_ontology:cs_reference_frame('3cd261cd-4b9a-4ffc-af3a-ae51c2def950', pre_confederation_treaty_supremacy).
narrative_ontology:cs_drift_state('3cd261cd-4b9a-4ffc-af3a-ae51c2def950', contemporary_constitutional_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3cd261cd-4b9a-4ffc-af3a-ae51c2def950', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_peoples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, federal_crown).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, secessionist_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold treaty rights asserted to predate and supersede Crown sovereignty; their consent is structurally required for legitimate secession under this reading. Must continuously assert and defend these rights in courts and constitutional forums against federal, provincial, and secessionist claims that would bypass them.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders, beneficiary,
    organized, generational, constrained, national).

% Bear the costs of territorial reconfiguration when secession processes proceed without meaningful consultation. Their lands, governance systems, and political futures are treated as subordinate to federal-provincial or majoritarian sovereignty in practice, despite the treaty primacy norm.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_peoples, payer,
    organized, generational, constrained, national).

% Loses unilateral authority to alter treaty relationships or to recognize secession without Indigenous consent. Must negotiate with treaty holders as co-sovereign parties rather than administer rights as delegated statutory privileges.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_crown, payer,
    institutional, generational, constrained, national).

% Cannot claim unilateral territorial sovereignty or secede without Indigenous treaty consent. Their authority over natural resources, land use, and territorial boundaries is subordinated to treaty rights under this reading.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments, payer,
    institutional, generational, constrained, regional).

% Cannot achieve legitimate secession through referendum or popular sovereignty alone. Must obtain Indigenous treaty holder consent, which constrains territorial claims and introduces a veto they cannot override through democratic majorities.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, secessionist_movements, payer,
    organized, biographical, constrained, regional).

% Interprets constitutional and treaty texts to enforce the primacy boundary. Acts as the authoritative interpreter of whether federal, provincial, or secessionist actions comply with treaty obligations, effectively administering the constraint.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, canadian_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Analyze the coherence and legitimacy of competing sovereignty claims. Provide the interpretive frameworks that courts and political actors draw upon when assessing secession legitimacy under treaty primacy.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__treaty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves competing claims to territorial sovereignty by establishing a hierarchical legitimacy rule: treaty relationships predate and supersede both federal and provincial constitutional authority, providing a decision procedure for which actors must consent to territorial reconfiguration.
% TRANSFER_FUNCTION: Transfers veto authority over territorial secession from federal and provincial governments (and from simple majority referendum) to Indigenous treaty holders; transfers the burden of legitimation from secessionist movements to include treaty consultation and consent.
% ABSENT_VOICES: Non-treaty Indigenous nations, including some MÃ©tis communities and non-status peoples, are structurally absent from the treaty-consent framework even though their territories may be affected. Settler minorities within secessionist regions who oppose partition are excluded from the Indigenous-federal-provincial tripartite structure. International recognition bodies are absent from the domestic treaty frame.
% DISAPPEARANCE_RATIONALE: Without the treaty primacy boundary, federal and provincial governments could treat treaty rights as ordinary constitutional interests amendable by parliamentary or referendum majorities. Secessionist movements could claim legitimacy via popular sovereignty alone. Indigenous territorial authority would collapse into delegated statutory rights rather than pre-existing sovereignty, and the constitutional order would reorganize around bilateral federalism rather than trilateral treaty federalism.
% FOUNDING_PROBLEM: The problem of legitimate ordering of multiple overlapping sovereignties (Crown, Indigenous, provincial) on shared territory without unilateral subordination of any party.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous legal scholars and treaty historians attest from outside the Crown beneficiary structure that the founding problem of unilateral Crown sovereignty remains unresolved. International human rights bodies (UNDRIP committees) corroborate that Indigenous consent is a live sovereignty issue. Federal and provincial governments assert the problem is managed through modern treaties; Indigenous critics assert it is perpetuated by Crown refusal to acknowledge treaty nations as co-sovereign.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the constraint fundamentally redistributes sovereignty: it extracts unilateral authority from federal, provincial, and secessionist actors and transfers veto power to treaty holders. Suppression (0.48) reflects legal and political enforcement through courts and constitutional interpretation rather than direct coercion. Theater ratio (0.38) captures the partial performativity of modern treaty consultation, where procedural compliance sometimes substitutes for substantive consent. Accessibility collapse (0.35) is moderate because alternatives (popular sovereignty, constitutional impossibility) remain intellectually available. Resistance (0.55) is moderate: federal and provincial governments resist full subordination of their authority, while secessionist movements resist the additional veto.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary and federal/provincial seats, the constraint appears as a necessary coordination mechanism preventing destructive sovereignty conflicts and honoring historical treaties. From the Indigenous peoples seat, the same structure often appears as an unpaid burden: they must perpetually litigate and politick to prevent their rights from being bypassed, while secessionist and government actors treat consultation as a procedural hurdle. The engine computes this divergence from the structural data rather than resolving it.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous treaty holders sit near the beneficiary end (d â 0.15): the constraint subsidizes their sovereignty claims by elevating their consent to a constitutional requirement. Indigenous peoples broadly sit nearer the target end (d â 0.75): despite the normative protection, they bear the ongoing costs of territorial reconfiguration and the labor of defending treaty rights. Federal and provincial governments sit at moderate-high target (d â 0.60): they lose unilateral authority but retain institutional power. Secessionist movements are high target (d â 0.70): their primary pathway to legitimacy is blocked. The judiciary sits near symmetric (d â 0.50): it administers the constraint without being primarily beneficiary or victim.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophic. Its founding problem â ordering overlapping sovereignties without unilateral subordination â remains live and contested. While practice has drifted from the reference frame of nation-to-nation treaty supremacy toward Crown domestication, the arrangement has not outlived its function. Rather, it is blocked from full function by the resistance of institutional payers. A mandatrophy reading would misclassify active political contestation as institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_primacy_domestication_ambiguity,
    'Is the treaty primacy reading a genuine recognition of pre-existing Indigenous sovereignty, or a domestication strategy that absorbs treaty rights into Canadian constitutional law to preserve ultimate Crown authority?',
    'Comparative analysis of judicial remedies: if treaty primacy operates as a trump within Canadian courts without independent Indigenous adjudication, the domestication reading is supported; if it generates parallel or superior Indigenous legal fora, the sovereignty reading is supported.',
    'If domestication, the constraint''s extraction from Indigenous peoples is higher than apparent (they receive procedural veto without substantive sovereignty). If genuine sovereignty, the coordination function is primary and extraction lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_primacy_domestication_ambiguity, conceptual, 'Whether treaty primacy is sovereign recognition or constitutional absorption').

omega_variable(
    consent_as_coordination_or_burden,
    'Does the Indigenous consent requirement function as protective coordination against unwanted territorial reconfiguration, or as a performative burden that extracts through procedural delay and politicization?',
    'Outcome analysis of secession-adjacent processes (resource development, land claims) under treaty consultation regimes: if consent blocks harmful reconfiguration, coordination; if consent is routinely overridden or generates costly process without protective outcome, burden.',
    'If burden, theater_ratio is higher and the constraint approaches snare-like extraction; if coordination, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_as_coordination_or_burden, empirical, 'Whether the consent mechanism coordinates or extracts').

omega_variable(
    kernel_reading_interaction,
    'This constraint is the treaty_primacy_reading of kernel secession_legitimacy_boundary. Sibling readings (constitutional_impossibility, popular_sovereignty, grievance_threshold) coexist in public discourse. Does treaty primacy logically foreclose popular sovereignty, or merely influence its legitimacy conditions?',
    'Logical analysis of framework compatibility: a single actor holding treaty primacy cannot simultaneously hold that a provincial referendum alone legitimizes secession, but different political parties can hold the two readings simultaneously. The relation influences for popular_sovereignty and grievance_threshold; coexists_with for constitutional_impossibility.',
    'If forecloses, the reading is structurally stronger but generates sharper resistance; if influences, it operates as an additional veto layer rather than a logical refutation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_interaction, conceptual, 'Structural relationship between treaty primacy and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(secession_treaty_tr_t0, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(secession_treaty_tr_t10, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(secession_treaty_tr_t20, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(secession_treaty_tr_t30, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(secession_treaty_tr_t40, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(secession_treaty_be_t0, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(secession_treaty_be_t10, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(secession_treaty_be_t20, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(secession_treaty_be_t30, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(secession_treaty_be_t40, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(secession_treaty_su_t0, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(secession_treaty_su_t10, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(secession_treaty_su_t20, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(secession_treaty_su_t30, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement(secession_treaty_su_t40, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
