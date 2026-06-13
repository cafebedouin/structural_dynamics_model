% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter: Secular Democratic Institutions and Civilian Military Control
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   This constraint represents the 'secular democratic' reading of the July
 *   Charter, which mandates secular democratic institutions and strict
 *   military subordination to civilian authority. This reading is actively
 *   enforced by secular political parties and civil society, often with
 *   international backing, and it directly constrains political Islam actors
 *   and the military's historical autonomous authority. The Charter itself is
 *   a contested kernel, with other readings (guided_nationalism_reading,
 *   military_custodian_reading) offering alternative frameworks for sovereign
 *   legitimacy.
 *
 * KEY AGENTS:
 *   - secular_political_parties: Primary beneficiary (organized/constrained) — benefits from secular democratic framework
 *   - civil_society_organizations: Secondary beneficiary (moderate/constrained) — benefits from democratic space
 *   - international_democratic_allies: External beneficiary (institutional/analytical) — supports democratic transition
 *   - political_islam_parties: Primary target (organized/identity_locked) — excluded/constrained by secular mandate
 *   - military_autonomous_authority: Primary target (institutional/constrained) — loses historical power to civilian control
 *   - religious_conservative_factions: Secondary target (moderate/identity_locked) — curtailed influence by secular mandate
 *   - constitutional_court: Agenda setter (institutional/constrained) — interprets and enforces the Charter
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.65).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.7).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter: Secular Democratic Institutions and Civilian Military Control").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, 'ab725f3d-7e8e-4c48-a74e-ad0ac6e85ed6').
narrative_ontology:cs_kernel_codification('ab725f3d-7e8e-4c48-a74e-ad0ac6e85ed6', fixed_text).
narrative_ontology:cs_authority_grounding('ab725f3d-7e8e-4c48-a74e-ad0ac6e85ed6', lineage).
narrative_ontology:cs_interpretation_layer_present('ab725f3d-7e8e-4c48-a74e-ad0ac6e85ed6').
narrative_ontology:cs_reading_relation('ab725f3d-7e8e-4c48-a74e-ad0ac6e85ed6', july_charter_sovereign_legitimacy__guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('ab725f3d-7e8e-4c48-a74e-ad0ac6e85ed6', july_charter_sovereign_legitimacy__military_custodian_reading, forecloses).
narrative_ontology:cs_axiom('ab725f3d-7e8e-4c48-a74e-ad0ac6e85ed6', foundational, state_secularism_mandate).
narrative_ontology:cs_axiom_status(state_secularism_mandate, holdable).
narrative_ontology:cs_axiom_grounding('ab725f3d-7e8e-4c48-a74e-ad0ac6e85ed6', state_secularism_mandate, deontological).
narrative_ontology:cs_axiom('ab725f3d-7e8e-4c48-a74e-ad0ac6e85ed6', foundational, military_subordination_to_civilian_rule).
narrative_ontology:cs_axiom_status(military_subordination_to_civilian_rule, holdable).
narrative_ontology:cs_axiom_grounding('ab725f3d-7e8e-4c48-a74e-ad0ac6e85ed6', military_subordination_to_civilian_rule, conventional).
narrative_ontology:cs_reference_frame('ab725f3d-7e8e-4c48-a74e-ad0ac6e85ed6', post_revolutionary_democratic_ideal).
narrative_ontology:cs_drift_state('ab725f3d-7e8e-4c48-a74e-ad0ac6e85ed6', contemporary_political_contestation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ab725f3d-7e8e-4c48-a74e-ad0ac6e85ed6', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_political_parties).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_society_organizations).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, international_democratic_allies).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_conservative_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These parties derive their legitimacy and operational space from the Charter's secular and democratic mandates. They actively advocate for strict adherence to civilian control over the military and the exclusion of religious parties from governance, seeing this as essential for the nation's democratic future.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_political_parties, beneficiary,
    organized, generational, constrained, national).

% Advocate for human rights, democratic norms, and civilian oversight. They benefit from the Charter's framework as it provides legal grounds for their advocacy and protects their operational space, though they face risks when enforcement weakens.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_society_organizations, beneficiary,
    moderate, biographical, constrained, national).

% Provide diplomatic and financial support to the nation, conditioned on its adherence to democratic principles and civilian rule. They view the Charter's secular-democratic reading as aligning with their foreign policy objectives and a basis for partnership.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_democratic_allies, beneficiary,
    institutional, generational, analytical, global).

% These parties, such as Jamaat-e-Islami, are structurally constrained and often excluded from the political process under this reading of the Charter. Their core ideology, which seeks to integrate religious law into governance, is deemed incompatible with the Charter's secular mandate, leading to their suppression and marginalization.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_parties, payer,
    organized, generational, identity_locked, national).

% The military, under this reading, is explicitly subordinate to civilian authority, losing its historical role as a 'guardian' of the state. This constrains its budget, operational independence, and political influence, which it often resists through covert means or by appealing to alternative interpretations of the Charter.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority, payer,
    institutional, generational, constrained, national).

% These factions, often aligned with traditional religious institutions, find their influence over public life curtailed by the secular mandate. They bear the cost of reduced social authority and political representation, often mobilizing grassroots resistance against the Charter's secular provisions.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_conservative_factions, payer,
    moderate, generational, identity_locked, local).

% The ultimate arbiter of the Charter's meaning. Its rulings on secularism and military subordination shape the operational reality of the constraint. While nominally independent, it faces pressure from both civilian and military factions, making its interpretation a site of ongoing contestation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, constitutional_court, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for a stable, secular, and democratic state by defining the roles of political institutions and subordinating the military to elected civilian leadership, thereby preventing coups and religious authoritarianism.
% TRANSFER_FUNCTION: Transfers political power and legitimacy from religious and military institutions to elected civilian bodies and secular political actors. It also transfers resources and influence away from religious organizations and military command structures.
% ABSENT_VOICES: Hardline religious scholars and military commanders who believe in their inherent right to guide the nation's destiny are excluded. They would argue that the Charter's true spirit mandates a state guided by religious principles or protected by military oversight, not a purely secular democracy.
% DISAPPEARANCE_RATIONALE: If this reading of the Charter vanished, the nation would likely descend into severe political instability. Military factions might reassert control, religious parties could attempt to seize power, and the democratic institutions would collapse, leading to a fundamental reorganization of the state's power structure.
% FOUNDING_PROBLEM: The nation faced a crisis of legitimacy after a period of military rule and religious extremism, with deep divisions over the role of religion and the military in governance. The Charter was intended to establish a new, stable foundation for the state.
% FOUNDING_PROBLEM_CORROBORATION: International observers, human rights organizations, and a significant portion of the populace (as evidenced by public opinion polls and civil society movements) corroborate that the problems of military overreach and religious extremism remain live threats, making the Charter's secular-democratic mandate continuously relevant, despite ongoing contestation from other factions.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the transition to a democratic state (benefiting secular actors and civil society) while simultaneously extracting from and suppressing political Islam and military autonomy. Extraction (0.65) is substantial due to the forced exclusion of significant political forces. Suppression (0.70) is high, requiring active enforcement to maintain the secular-democratic order against strong internal resistance. Theater (0.40) reflects the ongoing performance of democratic legitimacy while significant power struggles continue beneath the surface, often through constitutional interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Secular political parties and international allies experience this as a legitimate Rope, a necessary framework for democratic stability. Political Islam parties and military factions, however, experience it as a Snare, an imposed structure that suppresses their legitimate claims to power and influence. The Constitutional Court, as the agenda-setter, navigates this tension, with its interpretations often reflecting the shifting balance of power.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular political parties and civil society are clear beneficiaries (low d) as the Charter empowers them. International democratic allies also benefit from the alignment with their values (low d). Political Islam parties and military autonomous authority are clear targets (high d) as the Charter directly curtails their power and influence. Religious conservative factions are also targets (high d) due to the secular mandate. The Constitutional Court, while an agenda-setter, is also constrained by the need to maintain a semblance of legitimacy across all factions, placing its d closer to symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (establishing secular democracy) is still live, but its implementation involves significant extraction and suppression, preventing it from being a pure Rope. The ongoing contestation over the Charter's meaning (captured by the omegas and cs_structure) indicates that the coordination function is intertwined with an extractive power struggle, characteristic of a Tangled Rope rather than a degraded Piton or a fully functional Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_reading_legitimacy,
    'Is this secular-democratic reading of the July Charter genuinely accepted by a broad majority of the populace, or is its persistence primarily due to the enforcement capacity of its beneficiaries?',
    'Independent, internationally supervised referendums on key Charter provisions, or long-term, stable electoral outcomes consistently favoring secular democratic parties without significant suppression of alternatives.',
    'If acceptance is broad, the constraint moves closer to a Rope; if enforcement is primary, it remains a Tangled Rope or risks reclassification as a Snare if the coordination function is deemed a cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_reading_legitimacy, empirical, 'The true popular legitimacy of the secular-democratic interpretation.').

omega_variable(
    military_subordination_depth,
    'To what extent is the military''s subordination to civilian authority genuinely institutionalized, versus being a performative compliance that masks continued covert influence?',
    'Audits of military budgets and procurement by civilian bodies, transparency in military appointments and promotions, and the absence of military interference during political crises or electoral transitions.',
    'If subordination is deep, the constraint''s suppression requirement for military autonomy decreases, potentially moving it closer to a Rope. If it''s performative, the theater_ratio is higher, and the constraint remains a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_subordination_depth, empirical, 'The depth of military subordination to civilian authority.').

omega_variable(
    kernel_framing_underdetermination,
    'Given the existence of ''guided_nationalism_reading'' and ''military_custodian_reading'' as alternative interpretations of the July Charter, is the ''secular_democratic_reading'' the most defensible framing, or does the Charter''s text genuinely underdetermine a single interpretation?',
    'Comparative textual analysis by independent constitutional scholars, historical analysis of the Charter''s drafting process, and analysis of judicial precedent across different political eras. If multiple coherent readings persist without one logically foreclosing the others, the kernel is genuinely underdetermined.',
    'If the Charter genuinely underdetermines a single reading, then the ''secular_democratic_reading'' is one of several equally valid (though competing) constraints, highlighting the role of power in selecting which reading is enforced. If one reading is demonstrably more textually grounded, it strengthens that reading''s claim to be the ''true'' constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Ambiguity in the July Charter''s text allowing for multiple, competing interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'july_charter_sovereign_legitimacy' kernel. Its structural properties differ significantly from the 'guided_nationalism_reading' and 'military_custodian_reading' due to distinct beneficiary/victim sets and enforcement mechanisms, necessitating separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
