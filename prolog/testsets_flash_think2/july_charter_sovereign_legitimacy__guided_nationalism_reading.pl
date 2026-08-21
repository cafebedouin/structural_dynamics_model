% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: July Charter: Guided Nationalism Reading (Sovereign Legitimacy)
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   This constraint story describes the 'guided nationalism' reading of a
 *   post-revolutionary July Charter, which establishes an Islamic-nationalist
 *   framework with religious identity as the sovereign legitimacy ground.
 *   This reading asserts that the Charter's primary function is to unify the
 *   nation under a shared religious and national identity, providing
 *   stability. However, its operation involves significant extraction from
 *   secular institutions and religious minorities, whose rights and political
 *   participation are constrained. The claimed type is 'tangled_rope' because
 *   it presents a coordination function (national unity) that is inseparable
 *   from asymmetric extraction and active suppression of alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.85).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.9).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "July Charter: Guided Nationalism Reading (Sovereign Legitimacy)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, '342e8ab6-0d4c-4981-8956-dbd7d16b9555').
narrative_ontology:cs_kernel_codification('342e8ab6-0d4c-4981-8956-dbd7d16b9555', fixed_text).
narrative_ontology:cs_authority_grounding('342e8ab6-0d4c-4981-8956-dbd7d16b9555', lineage).
narrative_ontology:cs_interpretation_layer_present('342e8ab6-0d4c-4981-8956-dbd7d16b9555').
narrative_ontology:cs_reading_relation('342e8ab6-0d4c-4981-8956-dbd7d16b9555', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('342e8ab6-0d4c-4981-8956-dbd7d16b9555', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('342e8ab6-0d4c-4981-8956-dbd7d16b9555', foundational, islamic_identity_as_sovereign_source).
narrative_ontology:cs_axiom_status(islamic_identity_as_sovereign_source, holdable).
narrative_ontology:cs_axiom_grounding('342e8ab6-0d4c-4981-8956-dbd7d16b9555', islamic_identity_as_sovereign_source, theological).
narrative_ontology:cs_axiom('342e8ab6-0d4c-4981-8956-dbd7d16b9555', secondary, national_unity_through_religious_adherence).
narrative_ontology:cs_axiom_status(national_unity_through_religious_adherence, holdable).
narrative_ontology:cs_axiom_grounding('342e8ab6-0d4c-4981-8956-dbd7d16b9555', national_unity_through_religious_adherence, instrumental).
narrative_ontology:cs_reference_frame('342e8ab6-0d4c-4981-8956-dbd7d16b9555', islamic_nationalist_state_ideal).
narrative_ontology:cs_drift_state('342e8ab6-0d4c-4981-8956-dbd7d16b9555', contemporary_post_revolution_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('342e8ab6-0d4c-4981-8956-dbd7d16b9555', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_establishment).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The political and religious leaders who drafted and now enforce the Charter. They benefit from the consolidation of power and the legitimization of their rule through religious identity, suppressing dissent and alternative political visions.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Religious institutions and scholars whose interpretations of Islamic law and national identity are enshrined in the Charter. They gain constitutional status, influence over public life, and resources, becoming integral to the state's legitimacy apparatus.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_establishment, beneficiary,
    institutional, generational, constrained, national).

% Activists, intellectuals, and organizations advocating for secular governance, human rights, and pluralism. They face legal restrictions, censorship, and social pressure, with their institutions often dismantled or co-opted. Exit means abandoning their identity and homeland.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    powerless, biographical, identity_locked, national).

% Non-dominant religious groups whose rights and practices are subordinated to the state-sanctioned Islamic-nationalist framework. They experience discrimination in law and public life, with limited avenues for redress or political participation. Exit often means forced displacement or exile.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, generational, trapped, national).

% Political groups advocating for a secular, democratic state. They are often banned, their leaders imprisoned or exiled, and their platforms delegitimized by the Charter's framework. They would challenge the religious basis of sovereignty but are denied a voice in the official political sphere.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_political_parties, excluded,
    powerless, biographical, constrained, national).

% Organizations monitoring human rights and constitutional developments in the nation. They document abuses, publish reports, and lobby international bodies, but have limited direct power to alter the Charter's enforcement or its legitimacy claims.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies the nation under a shared religious and national identity, providing a foundational framework for law, governance, and social cohesion in the post-revolutionary era.
% TRANSFER_FUNCTION: Transfers ultimate political and legal authority from secular or diverse principles to a specific interpretation of religious identity, consolidating power within the Islamic-nationalist elite and religious establishment, while subordinating secular institutions and religious minorities.
% ABSENT_VOICES: Secular political parties, human rights organizations, and representatives of religious minorities who advocate for a pluralistic, secular state are systematically excluded from the constitutional discourse and political process. They would object to the religious basis of sovereignty and the constraints on civil liberties.
% DISAPPEARANCE_RATIONALE: If the Charter's Islamic-nationalist framework vanished overnight, the entire legal and political system would lose its foundational legitimacy. This would likely lead to a severe power vacuum, widespread civil unrest, and a complete re-founding of the state's constitutional order, as the basis for national identity and governance would be dissolved.
% FOUNDING_PROBLEM: The Charter was established to address post-revolutionary instability, perceived moral decay, and a desire to forge a distinct national identity rooted in religious values, thereby providing a stable and legitimate basis for the new state.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the Islamic-nationalist elite and religious establishment consistently assert that the founding problems of instability and moral decay remain live, requiring the Charter's continued enforcement. International observers and secular civil society groups, however, contest this, arguing that the problems are either manufactured or exaggerated to justify ongoing power consolidation and suppression of dissent; independent analyses from outside the benefiting parties often support the latter view.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the transfer of power and resources to the Islamic-nationalist elite and religious establishment, at the expense of secular civil society and religious minorities. Suppression (0.90) is severe, as the state actively enforces religious identity as the sole legitimate basis for governance, dismantling or marginalizing dissenting voices and institutions. The theater ratio (0.40) indicates that while some genuine coordination (e.g., national identity formation) may occur, a substantial portion of the state's activity is performative enforcement of the religious-nationalist narrative to maintain its legitimacy and suppress alternatives. Accessibility collapse is high (0.75) because secular alternatives are systematically removed, and resistance (0.60) is present but heavily suppressed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Islamic-nationalist elites, the Charter is a legitimate and necessary 'rope' for national unity and stability. From the perspective of secular civil society and religious minorities, it operates as a 'snare' or 'tangled_rope' that extracts their rights and resources under the guise of national cohesion. The engine's classification will highlight this divergence by computing a highly extractive type from the structural data, contrasting with the claimed 'rope' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The Islamic-nationalist elites and religious establishment are clear beneficiaries and agenda-setters, deriving power and legitimacy from the Charter (low directionality). Secular civil society and religious minorities are direct targets, bearing the costs of constrained rights and political exclusion (high directionality). Secular political parties are excluded, their very existence challenging the Charter's premise. International human rights advocates act as observers, documenting the constraint's effects without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_unity_vs_elite_control,
    'Is the Charter''s emphasis on religious identity primarily a genuine mechanism for national unity and stability, or is it a tool for consolidating elite power and suppressing dissent?',
    'Empirical analysis of social cohesion metrics, political participation rates, and human rights records in the presence of the Charter, compared to counterfactual scenarios or similar nations with different constitutional frameworks.',
    'If primarily elite control, the constraint''s effective extractiveness and suppression are higher, and its coordination function is largely theatrical. If genuine unity, the coordination function is stronger, and extraction is a byproduct of necessary enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_unity_vs_elite_control, empirical, 'Ambiguity of the ''Islamic-nationalist'' framework''s primary function.').

omega_variable(
    charter_interpretive_contest,
    'To what extent is the ''guided_nationalism_reading'' the dominant and enforced interpretation of the July Charter, versus other readings (e.g., secular_democratic, military_custodian) that might gain traction?',
    'Analysis of judicial rulings, legislative debates, public discourse, and the political power of various factions over time. Resolution would involve identifying which reading''s axioms are consistently upheld in practice.',
    'If this reading is successfully challenged, the constraint''s structural properties (beneficiaries, victims, extractiveness) would shift dramatically, potentially leading to a reclassification under a different reading''s terms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_interpretive_contest, conceptual, 'The contestability of the Charter''s true intent and dominant interpretation.').

omega_variable(
    religious_identity_unifying_force,
    'Is religious identity, as defined by the Charter, an inherently unifying force for the nation, or does its imposition create deeper divisions and conflict?',
    'Sociological studies on inter-group relations, surveys of national identity across diverse populations, and analysis of internal conflicts or social unrest linked to identity politics.',
    'If divisive, the claimed coordination function is undermined, increasing the effective extractiveness and suppression, as the constraint actively generates the very divisions it claims to resolve. If genuinely unifying, the coordination function is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_identity_unifying_force, empirical, 'The empirical status of religious identity as a unifying vs. divisive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 10, 0.83).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 15, 0.84).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 10, 0.88).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 15, 0.89).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'july_charter_sovereign_legitimacy' kernel. Each reading represents a distinct structural constraint with its own ε value and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
