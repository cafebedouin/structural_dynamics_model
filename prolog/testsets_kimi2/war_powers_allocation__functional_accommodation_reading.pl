% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: War Powers Functional Accommodation (Contextual Allocation Reading)
 *   domain: constitutional law / separation of powers
 *
 * SUMMARY:
 *   This constraint instantiates the functional_accommodation_reading of the
 *   war_powers_allocation kernel. Under this reading, constitutional war
 *   powers are allocated contextually: imminent threats permit unilateral
 *   executive action, while prolonged campaigns require congressional
 *   authorization. The reading creates an ambiguity zone between 'imminent'
 *   and 'prolonged' that both branches contest. Over time, executive
 *   operational framing has expanded the unilateral zone, extracting Article
 *   I authority from Congress while preserving a veneer of inter-branch
 *   accommodation. The constraint coordinates rapid crisis response but
 *   asymmetrically extracts institutional authority through contextual
 *   ambiguity.
 *
 * KEY AGENTS:
 *   - executive_branch: Primary beneficiary and agenda-setter (institutional/global) â gains discretionary authority and administers operational-context framing
 *   - congress: Primary payer (institutional/national) â bears institutional cost of eroded categorical war powers and repeated fait accompli
 *   - federal_judiciary: Analytical observer (institutional/national) â abstains from adjudication under political question and ripeness doctrines, validating the ambiguity
 *   - public_electorate: Excluded party (organized/national) â structurally absent from operational-context determinations despite bearing democratic accountability costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.78).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.8).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "War Powers Functional Accommodation (Contextual Allocation Reading)").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional law / separation of powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, 'd982de54-3364-44c5-8196-b96223410f63').
narrative_ontology:cs_kernel_codification('d982de54-3364-44c5-8196-b96223410f63', fixed_text).
narrative_ontology:cs_authority_grounding('d982de54-3364-44c5-8196-b96223410f63', practice).
narrative_ontology:cs_interpretation_layer_present('d982de54-3364-44c5-8196-b96223410f63').
narrative_ontology:cs_reading_relation('d982de54-3364-44c5-8196-b96223410f63', war_powers_allocation__congressional_primacy_reading, influences).
narrative_ontology:cs_reading_relation('d982de54-3364-44c5-8196-b96223410f63', war_powers_allocation__inherent_executive_reading, influences).
narrative_ontology:cs_axiom('d982de54-3364-44c5-8196-b96223410f63', foundational, contextual_allocation_doctrine).
narrative_ontology:cs_axiom_status(contextual_allocation_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('d982de54-3364-44c5-8196-b96223410f63', contextual_allocation_doctrine, conventional).
narrative_ontology:cs_axiom('d982de54-3364-44c5-8196-b96223410f63', foundational, imminent_threat_executive_prerogative).
narrative_ontology:cs_axiom_status(imminent_threat_executive_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('d982de54-3364-44c5-8196-b96223410f63', imminent_threat_executive_prerogative, conventional).
narrative_ontology:cs_reference_frame('d982de54-3364-44c5-8196-b96223410f63', functional_separation_framework).
narrative_ontology:cs_drift_state('d982de54-3364-44c5-8196-b96223410f63', contemporary_unilateral_presidency, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d982de54-3364-44c5-8196-b96223410f63', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates military operations under claims of imminent threat or limited intervention. Frames operational context to fit within the unilateral action zone through OLC opinions and public messaging. Benefits from ambiguity that allows prolonged operations to be characterized as limited or defensive. Could theoretically seek authorization but chooses not to in order to preserve discretion.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, executive_branch, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, executive_branch, beneficiary).

% Constitutionally vested with war declaration and authorization power. Under functional accommodation, retains theoretical authority over prolonged campaigns but is routinely bypassed or presented with fait accompli. Can withhold funding or enact statutory limits, but political costs, partisan alignment, and collective-action problems prevent effective resistance. Bears the institutional cost of eroded constitutional authority.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congress, payer,
    institutional, biographical, constrained, national).

% Possesses jurisdiction over separation-of-powers disputes but frequently abstains under political question doctrine, ripeness principles, or lack of standing. When it intervenes, it tends to avoid challenging the executive's contextual claims. Sits in an analytical posture with no institutional exit from constitutional jurisdiction.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% Elects both branches but lacks direct mechanism to enforce war powers compliance. Public opinion on military action is reactive and framed by executive messaging. Structurally excluded from the operational-context determination that governs whether congressional authorization is required.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, public_electorate, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__functional_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates rapid national security response without requiring protracted legislative deliberation during imminent threats, while preserving a legislative check over sustained military commitments.
% TRANSFER_FUNCTION: Transfers discretionary authority over the initiation, scope, and termination of military force from Congress to the Executive in operational contexts characterized as imminent or limited, while theoretically reserving prolonged campaigns for legislative authorization.
% ABSENT_VOICES: Foreign populations in target states; service members who bear deployment risks without independent voice in authorization debates; and institutionalist legal scholars advocating categorical war-declaration requirements are present in academic discourse but structurally excluded from the operational-context determination.
% DISAPPEARANCE_RATIONALE: If the functional accommodation framework vanished, the inter-branch equilibrium around military force would destabilize. Reverting to strict congressional primacy would remove executive discretion for rapid response; reverting to inherent executive authority would remove the legislative check for prolonged campaigns. Either shift would rearrange the institutional landscape for war initiation.
% FOUNDING_PROBLEM: The Constitution's divided war powers create institutional friction during fast-moving security threats; rigid categorical allocation (exclusive congressional declaration versus exclusive executive response) could paralyze defense or enable unchecked executive war-making.
% FOUNDING_PROBLEM_CORROBORATION: The executive branch and national security bureaucracy attest the problem is still live, citing need for speed and secrecy. Congressional institutionalists and legal historians attest the accommodation has shifted from solving friction to enabling evasion. Corroboration from outside the benefiting parties includes separation-of-powers scholars and retired judges who document the framework's drift toward unilateralism.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.78) is high because the ambiguity zone between imminent threat and prolonged campaign has been systematically exploited to authorize extended military operations without legislative approval. Suppression (0.80) is high because the constraint's persistence depends on suppressing categorical rule alternatives (formal declarations of war, strict congressional primacy) and on judicial abstention that prevents legal clarification. Theater_ratio (0.52) reflects that an increasing share of inter-branch activity consists of legal performances (OLC opinions, War Powers Resolution reporting) that maintain the appearance of accommodation while actual congressional control has atrophied. Accessibility_collapse (0.75) is high because once the contextual frame is accepted, categorical alternatives become politically inaccessible. Resistance (0.55) is moderate: Congress occasionally asserts itself through funding threats or WPR invocation, but partisan alignment and institutional collective-action costs limit effective pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the executive seat, the functional accommodation framework appears as necessary constitutional flexibility preventing paralysis during fast-moving security threats. From the congressional seat, the same framework appears as a structural transfer of Article I authority to Article II through manipulable temporal distinctions. The federal judiciary sees a non-justiciable political question; the public sees military decisions made without transparent authorization. The engine computes these divergences from the structural data rather than adjudicating them.
 *
 * DIRECTIONALITY LOGIC:
 *   executive_branch is declared as beneficiary and agenda_setter: it collects discretionary authority and administers the operational-context framing. Its exit is mobile within the constitutional order, yielding low directionality (beneficiary side). congress is declared as victim/payer: its constitutional authority over war initiation is extracted through ambiguity and fait accompli. Its exit is constrained by political dynamics, partisan alignment, and institutional inertia, yielding high directionality (target side). federal_judiciary sits at analytical exit with institutional power, observing without paying or collecting.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework was built to solve the founding problem of institutional paralysis during imminent threats (founding_problem_status: contested). If that problem is dead and the arrangement now primarily enables prolonged unilateral campaigns, the (dead Ã world_rearranges) mismatch flags capture or zombie dynamics. However, the coordination functionârapid response capability in genuine emergenciesâremains live, preventing pure snare classification. The tangled_rope classification captures both the genuine coordination and the asymmetric extraction through ambiguity. The theater_ratio (0.52) captures the growing performance of constitutional fidelity without actual congressional control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestedness,
    'Does the functional accommodation reading represent a genuine constitutional middle ground or a rhetorical device that systematically advantages the executive over time?',
    'Historical time-series analysis of executive unilateral action frequency and congressional authorization rates; a monotonic increase in unilateral action under the functional accommodation framework indicates a ratchet effect rather than an equilibrium.',
    'Would reclassify from tangled_rope toward snare if the ambiguity zone is exploited systematically; would support rope classification if contextual allocation genuinely balances across branches over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestedness, conceptual, 'Whether functional accommodation is a stable middle ground or executive ratchet').

omega_variable(
    imminent_prolonged_boundary,
    'Is the boundary between imminent threat and prolonged campaign determinate in practice, or does it collapse under executive framing?',
    'Empirical audit of military operations designated as limited, defensive, or imminent that exceeded 90 days without congressional authorization.',
    'If the boundary routinely collapses, the constraint''s coordination function is weaker than authored and extraction is higher; may trigger false summit or mandatrophy flags.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminent_prolonged_boundary, empirical, 'Operational boundary stability between imminent and prolonged contexts').

omega_variable(
    cs_framing_underdetermination,
    'Does the authority of the functional accommodation framework derive from constitutional text and lineage, from evolved inter-branch practice, or from judicial abstention that enables executive aggrandizement?',
    'Genealogical analysis of war powers jurisprudence tracing the weight given to text, historical practice, and justiciability doctrines in sustaining the framework.',
    'Would shift authority_grounding from lineage/practice to extraction if judicial abstention is the primary sustaining force, altering the commitment-system classification and interpretive-layer assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Epistemic grounding of the functional accommodation authority structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(war__tr_t15, war_powers_allocation__functional_accommodation_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__functional_accommodation_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(war__tr_t45, war_powers_allocation__functional_accommodation_reading, theater_ratio, 45, 0.42).
narrative_ontology:measurement(war__tr_t60, war_powers_allocation__functional_accommodation_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(war__tr_t75, war_powers_allocation__functional_accommodation_reading, theater_ratio, 75, 0.52).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(war__be_t15, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(war__be_t45, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 45, 0.65).
narrative_ontology:measurement(war__be_t60, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(war__be_t75, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 75, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(war__su_t15, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(war__su_t45, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 45, 0.68).
narrative_ontology:measurement(war__su_t60, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(war__su_t75, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 75, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, inherent_executive_reading).

% DUAL FORMULATION NOTE:
% This constraint is the functional_accommodation_reading of the war_powers_allocation kernel, decomposed from the colloquial label 'war powers' per the epsilon-invariance principle. Sibling readings (congressional_primacy_reading, inherent_executive_reading) instantiate structurally distinct claims with different epsilon values, beneficiary/victim structures, and directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
