% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: July Charter: Military as Permanent Custodian
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   This constraint story instantiates the 'military custodian' reading of
 *   the July Charter's sovereign legitimacy kernel. Under this reading, the
 *   Charter is interpreted as permanently ratifying the military as the
 *   ultimate institutional guardian of national stability. This grants the
 *   military extensive powers, including a de facto veto over civilian
 *   governance and the authority to suppress political contestation, all
 *   justified by an ongoing mandate to prevent state collapse. Civilian
 *   institutions are structurally subordinated to this military authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.85).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.9).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, snare).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "July Charter: Military as Permanent Custodian").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "political/constitutional").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, 'fb612078-e196-4725-8a5f-2a399d386194').
narrative_ontology:cs_kernel_codification('fb612078-e196-4725-8a5f-2a399d386194', formalized).
narrative_ontology:cs_authority_grounding('fb612078-e196-4725-8a5f-2a399d386194', extraction).
narrative_ontology:cs_interpretation_layer_present('fb612078-e196-4725-8a5f-2a399d386194').
narrative_ontology:cs_reading_relation('fb612078-e196-4725-8a5f-2a399d386194', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('fb612078-e196-4725-8a5f-2a399d386194', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_axiom('fb612078-e196-4725-8a5f-2a399d386194', foundational, military_as_ultimate_arbiter_of_stability).
narrative_ontology:cs_axiom_status(military_as_ultimate_arbiter_of_stability, holdable).
narrative_ontology:cs_axiom_grounding('fb612078-e196-4725-8a5f-2a399d386194', military_as_ultimate_arbiter_of_stability, conventional).
narrative_ontology:cs_axiom('fb612078-e196-4725-8a5f-2a399d386194', foundational, national_security_supremacy).
narrative_ontology:cs_axiom_status(national_security_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('fb612078-e196-4725-8a5f-2a399d386194', national_security_supremacy, deontological).
narrative_ontology:cs_reference_frame('fb612078-e196-4725-8a5f-2a399d386194', post_revolutionary_military_order).
narrative_ontology:cs_drift_state('fb612078-e196-4725-8a5f-2a399d386194', contemporary_civilian_aspirations, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('fb612078-e196-4725-8a5f-2a399d386194', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_establishment).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, state_security_apparatus).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_government_officials).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civil_society_organizations).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, national_security_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, state_stability_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiary and enforcer of the Charter's military custodian reading. It wields ultimate veto power over civilian decisions, controls significant state resources, and enjoys immunity from civilian oversight, all justified by its role as guardian of national stability.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_establishment, agenda_setter,
    institutional, generational, arbitrage, national).

% Operates under the military's umbrella, benefiting from expanded powers, resources, and legal protections to suppress dissent and maintain the military's authority. Its function is directly tied to enforcing the military's 'guardian' role.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, state_security_apparatus, beneficiary,
    institutional, biographical, constrained, national).

% Hold nominal positions of authority but operate under the constant threat of military intervention or veto. Their policy space is severely limited, and their careers depend on not challenging the military's constitutional role. They bear the cost of limited sovereignty.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_government_officials, payer,
    powerful, immediate, constrained, national).

% Are formally permitted but face severe restrictions on their activities, including arbitrary arrests, media blackouts, and electoral manipulation. Their ability to contest military authority is systematically suppressed, making their political participation largely performative.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    organized, biographical, trapped, national).

% A key source of public dissent, frequently targeted by security forces for protests against military rule. They bear the direct costs of repression, including imprisonment and violence, for challenging the established order.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement, payer,
    moderate, immediate, trapped, local).

% Work for human rights, democracy, and accountability but face legal restrictions, funding limitations, and harassment. Their advocacy is tolerated only within narrow bounds that do not threaten the military's constitutional position.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civil_society_organizations, payer,
    organized, biographical, constrained, national).

% Monitor the country's human rights record and democratic transition, issuing reports and recommendations. While they can exert diplomatic pressure, they have no direct enforcement power over the Charter's provisions.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, international_observers, observer,
    institutional, generational, analytical, global).

% Advocate for a fully civilian-led, secular democratic state with military subordination to elected authority. Their vision is directly foreclosed by the military custodian reading of the Charter, and they are systematically marginalized from political discourse.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, secular_democratic_advocates, excluded,
    organized, generational, trapped, national).

% Advocate for an Islamic-nationalist framework as the basis of sovereign legitimacy. While they may benefit from the stability provided by the military, their primary vision for the state's identity is distinct from the military's self-declared role as ultimate guardian, making them a rival claimant to the Charter's ultimate meaning.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, guided_nationalism_proponents, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for national stability and security in a post-revolutionary context, preventing fragmentation, civil war, or external interference by designating the military as the ultimate guarantor of order.
% TRANSFER_FUNCTION: Transfers ultimate political authority, significant budgetary control, and immunity from civilian oversight to the military establishment, from civilian institutions, political parties, and the populace. It also transfers the burden of maintaining 'stability' (often through repression) to the civilian population.
% ABSENT_VOICES: Autonomous political parties, student movements, and civil society organizations are actively suppressed or marginalized; they would advocate for full civilian control, democratic accountability, and an end to military interference in politics. International human rights bodies also voice strong objections.
% DISAPPEARANCE_RATIONALE: If the military's constitutional guardianship vanished overnight, there would be an immediate power vacuum. This would likely lead to a period of intense political instability, potentially renewed civil unrest, or a rapid, contested transition to full civilian rule. The entire political landscape, including the balance of power among civilian factions, would be fundamentally reorganized.
% FOUNDING_PROBLEM: The Charter was established to address severe post-revolutionary instability, the threat of civil war, perceived external interference, and the weakness of nascent civilian institutions, which were seen as incapable of securing the state.
% FOUNDING_PROBLEM_CORROBORATION: The military establishment and its allies consistently attest that the founding problems of instability and external threats remain live, justifying their continued role. However, independent historians, human rights organizations, and civilian political groups argue that these problems are either substantially resolved or are actively perpetuated by the military's own actions to maintain its power, citing evidence from outside the benefiting parties.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the military's capture of political power, resources, and immunity from oversight, which it leverages from its constitutional role. Suppression (0.90) is severe, as the military actively enforces its authority by limiting political freedoms, controlling media, and repressing dissent. The theater ratio (0.60) indicates that while the military's role may have initially addressed genuine instability, a significant portion of its 'guardian' activities now serve to maintain its own power and privileges, rather than purely functional security provision. The rising trend in extractiveness, suppression, and theater over the interval reflects the military's consolidation of power and the increasing performativity of its 'stability' mandate.
 *
 * PERSPECTIVAL GAP:
 *   From the military's perspective, its role is a necessary, self-sacrificing act of national salvation, ensuring stability and preventing chaos. From the perspective of civilian political actors and the student movement, the same structure is an oppressive, extractive mechanism that stifles democracy and perpetuates military rule under the guise of security.
 *
 * DIRECTIONALITY LOGIC:
 *   The military establishment and state security apparatus are clear beneficiaries (low directionality), as they directly gain power, resources, and immunity. Civilian government officials, autonomous political parties, student movements, and civil society organizations are targets (high directionality), bearing the costs of limited sovereignty, political repression, and restricted freedoms. International observers and secular democratic advocates are analytical or excluded, experiencing the constraint as an object of study or a barrier to their goals.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits strong signs of mandatrophy. While the initial post-revolutionary instability may have warranted a temporary military role, the 'military custodian' reading allows this role to become permanent. The military's continued claim that the founding problem is 'live' (contested by external observers) serves as a cover story for its ongoing extraction of political power, preventing a transition to full civilian governance. The rising theater ratio further suggests that the original function has atrophied, replaced by performative maintenance of power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stability_genuine_vs_pretext,
    'Is the military''s claim of ensuring stability a genuine, ongoing necessity, or primarily a pretext for maintaining its institutional power and privileges?',
    'Independent, longitudinal studies of security threats and political stability under alternative governance scenarios (e.g., full civilian control), coupled with analysis of military spending and asset accumulation versus declared security needs.',
    'If primarily a pretext, the constraint''s extractiveness and suppression are even more unjustified, strengthening its classification as a Snare. If genuinely necessary, a portion of the extraction might be reclassified as a coordination cost, potentially shifting the classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_genuine_vs_pretext, empirical, 'Assesses the true function of military custodianship beyond its self-justification.').

omega_variable(
    permanence_vs_transition,
    'Is the military''s ''guardian'' role intended by the Charter to be a permanent feature of the state, or a transitional measure that has overstayed its mandate?',
    'Legal-historical analysis of the Charter''s drafting intent and subsequent amendments, combined with comparative constitutional law analysis of similar post-revolutionary transitions. This is a conceptual omega because ''intent'' is contested.',
    'If intended as transitional, the constraint''s current persistence is a clear case of mandatrophy, reinforcing its Snare classification. If intended as permanent, the constraint is structurally more ''fixed'' within this reading, though still extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(permanence_vs_transition, conceptual, 'Examines the intended temporal scope of military guardianship.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of political parties and movements structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-transition analysis: if political participation remains low and self-censorship persists after military withdrawal, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the targets carry the suppression with them after exit, making democratic transition harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for political actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(july_tr_t6, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 6, 0.5).
narrative_ontology:measurement(july_tr_t12, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 12, 0.55).
narrative_ontology:measurement(july_tr_t18, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 18, 0.58).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 24, 0.59).
narrative_ontology:measurement(july_tr_t30, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 30, 0.6).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(july_be_t6, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 6, 0.78).
narrative_ontology:measurement(july_be_t12, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 12, 0.81).
narrative_ontology:measurement(july_be_t18, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 18, 0.83).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(july_be_t30, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(july_su_t6, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 6, 0.83).
narrative_ontology:measurement(july_su_t12, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 12, 0.86).
narrative_ontology:measurement(july_su_t18, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 18, 0.88).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(july_su_t30, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_judicial_independence).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, freedom_of_assembly).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, economic_development_policy).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'July Charter Sovereign Legitimacy' kernel. Its structural properties and metrics differ significantly from sibling readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
