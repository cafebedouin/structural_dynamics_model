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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter: Secular Democratic Reading
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   This constraint represents the 'secular democratic' reading of the July
 *   Charter, which mandates secular democratic institutions and subordinates
 *   the military to civilian authority. This reading actively constrains
 *   political Islam actors and military autonomous authority, who are
 *   identified as victims. The constraint is claimed as a 'tangled_rope'
 *   because it genuinely coordinates civilian governance while simultaneously
 *   extracting from and suppressing alternative political visions and
 *   military power. The metrics reflect the ongoing struggle to maintain this
 *   interpretation against strong internal resistance.
 *
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
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter: Secular Democratic Reading").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, 'a1077d37-8355-4df9-9fc7-c987db6399b1').
narrative_ontology:cs_kernel_codification('a1077d37-8355-4df9-9fc7-c987db6399b1', fixed_text).
narrative_ontology:cs_authority_grounding('a1077d37-8355-4df9-9fc7-c987db6399b1', lineage).
narrative_ontology:cs_interpretation_layer_present('a1077d37-8355-4df9-9fc7-c987db6399b1').
narrative_ontology:cs_reading_relation('a1077d37-8355-4df9-9fc7-c987db6399b1', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1077d37-8355-4df9-9fc7-c987db6399b1', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('a1077d37-8355-4df9-9fc7-c987db6399b1', foundational, civilian_supremacy_over_military).
narrative_ontology:cs_axiom_status(civilian_supremacy_over_military, holdable).
narrative_ontology:cs_axiom_grounding('a1077d37-8355-4df9-9fc7-c987db6399b1', civilian_supremacy_over_military, conventional).
narrative_ontology:cs_axiom('a1077d37-8355-4df9-9fc7-c987db6399b1', foundational, state_neutrality_on_religion).
narrative_ontology:cs_axiom_status(state_neutrality_on_religion, holdable).
narrative_ontology:cs_axiom_grounding('a1077d37-8355-4df9-9fc7-c987db6399b1', state_neutrality_on_religion, deontological).
narrative_ontology:cs_reference_frame('a1077d37-8355-4df9-9fc7-c987db6399b1', post_revolutionary_democratic_ideal).
narrative_ontology:cs_drift_state('a1077d37-8355-4df9-9fc7-c987db6399b1', contemporary_political_contest, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a1077d37-8355-4df9-9fc7-c987db6399b1', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_political_parties).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_society_organizations).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_actors).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and implements policies aligned with the secular democratic principles of the Charter. Benefits from the Charter's framework which legitimizes their political program and constrains religious and military influence. Their power is contingent on maintaining the Charter's secular interpretation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_political_parties, agenda_setter,
    institutional, generational, constrained, national).

% Supports and promotes the secular democratic ideals enshrined in the Charter. Benefits from the space for civic engagement and human rights advocacy that this reading of the Charter provides. Their influence is tied to the strength of civilian institutions.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_society_organizations, beneficiary,
    organized, biographical, constrained, national).

% Seeks to establish an Islamic state and views the secular democratic framework as illegitimate. They are suppressed and excluded from mainstream political participation under this reading of the Charter, facing legal and political barriers. Their identity is deeply tied to an alternative vision of the state.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_actors, payer,
    organized, generational, identity_locked, national).

% Historically held significant political power and autonomy. Under this reading, the military is strictly subordinate to civilian authority, limiting its political influence and budget. They resist this subordination but are bound by the Charter's formal structure.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority, payer,
    institutional, generational, constrained, national).

% Monitors the implementation of democratic reforms and civilian control. Provides diplomatic and financial support to the secular democratic government, reinforcing this reading of the Charter. Their influence is external but significant.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_democratic_allies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for a secular democratic state, coordinating the roles of civilian government, political parties, and the military under a unified constitutional order, aiming to prevent military coups and religious authoritarianism.
% TRANSFER_FUNCTION: Transfers political power and legitimacy from religious and military institutions to secular civilian institutions, along with associated resources and control over policy-making.
% ABSENT_VOICES: Hardline religious factions and military traditionalists who reject civilian supremacy are actively suppressed or marginalized; they would argue for a state grounded in religious law or military guardianship, respectively.
% DISAPPEARANCE_RATIONALE: If this reading of the Charter vanished, the political landscape would immediately destabilize. Political Islam actors would likely reassert demands for an Islamic state, the military might attempt to reclaim its 'custodian' role, and secular institutions would lose their primary legitimizing framework, leading to a power vacuum and potential civil conflict.
% FOUNDING_PROBLEM: The Charter was established to resolve a period of political instability, military interventions in civilian governance, and the rise of religious extremism, aiming to secure a stable, democratic, and secular future for the nation.
% FOUNDING_PROBLEM_CORROBORATION: Secular political parties and civil society organizations attest the problem is still live, citing ongoing threats from religious extremism and military ambition. Political Islam actors and some military factions contest this, arguing the Charter itself is the problem, imposing an alien system; independent historians and international observers corroborate the original intent but note the persistent contestation.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) and suppression (0.70) are high because this reading of the Charter actively disempowers significant political forces (religious parties, military factions) that hold alternative visions for the state. The 'secular democratic' framework is not universally accepted and requires continuous enforcement to maintain its dominance. The theater ratio (0.40) indicates that while there is genuine democratic function, a substantial portion of the effort goes into performing legitimacy and suppressing dissent rather than pure coordination. Resistance is high (0.75) due to the persistent opposition from those disempowered by this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of secular political parties, the Charter is a legitimate 'rope' for national coordination and progress. From the perspective of political Islam actors, it is a 'snare' designed to exclude them. The engine's classification as 'tangled_rope' captures this hybrid nature, acknowledging both the coordination function and the asymmetric extraction and suppression required to maintain it.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular political parties and civil society organizations are beneficiaries and agenda-setters, as this reading legitimizes their power and provides a framework for their activities. Political Islam actors and military autonomous authority are victims and payers, as their influence and autonomy are directly curtailed. International democratic allies act as observers, reinforcing the legitimacy of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (securing a secular democratic state) is still contested ('founding_problem_status': 'contested'), preventing a clear mandatrophy resolution. The high resistance and suppression indicate that the mandate has not atrophied but is actively being fought over. The classification as 'tangled_rope' prevents mislabeling it as a pure 'snare' by acknowledging its genuine coordination function for its beneficiaries, while also recognizing the significant extraction and suppression it entails for its victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_interpretive_dominance,
    'To what extent does the ''secular democratic'' reading of the July Charter genuinely represent the consensus of the populace, versus being enforced by a dominant political faction?',
    'Independent, internationally monitored referendums or elections on key constitutional principles, coupled with longitudinal surveys of public opinion on secularism and military roles.',
    'If the reading lacks broad popular support, its legitimacy as a ''tangled_rope'' would be weakened, pushing it closer to a ''snare'' for the majority. If it has strong popular support, its coordination function would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_interpretive_dominance, empirical, 'Assesses the popular legitimacy of the secular democratic interpretation.').

omega_variable(
    military_subordination_sustainability,
    'Is the military''s subordination to civilian authority, as mandated by this reading, a stable and institutionalized reality, or a precarious state maintained by active political will?',
    'Analysis of military budget transparency, civilian oversight mechanisms, and the absence of military interference in political crises over a sustained period (e.g., 10+ years).',
    'If subordination is precarious, the ''requires_active_enforcement'' metric is understated, and the constraint''s stability is lower than perceived. If it is institutionalized, the constraint is more robustly a ''rope'' for civilian governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_subordination_sustainability, empirical, 'Evaluates the institutional depth of military subordination.').

omega_variable(
    secularism_definition_ambiguity,
    'Is the concept of ''secular'' in the Charter interpreted as strict separation of religion and state, or as state neutrality towards religions, allowing for public religious expression?',
    'Judicial rulings on religious freedom cases, legislative debates on religious education, and public discourse analysis.',
    'A strict separation interpretation would further constrain political Islam actors, increasing extraction. A neutrality interpretation might allow more space for religious parties, potentially reducing suppression and extractiveness, shifting the constraint closer to a ''rope'' for a broader set of actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secularism_definition_ambiguity, conceptual, 'Clarifies the specific meaning of ''secular'' within the Charter''s framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'july_charter_sovereign_legitimacy' kernel. This 'secular democratic' reading directly influences and is influenced by the 'guided nationalism' and 'military custodian' readings, as they represent competing interpretations of the same foundational document.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
