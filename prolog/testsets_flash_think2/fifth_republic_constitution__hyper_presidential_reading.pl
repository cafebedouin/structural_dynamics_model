% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__hyper_presidential_reading, []).

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
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Hyper-Presidential Interpretation of Fifth Republic Constitution
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   This constraint is the 'hyper_presidential_reading' of the
 *   'fifth_republic_constitution' kernel, emphasizing the president's direct
 *   mandate and minimal legislative constraint. Sibling readings include
 *   'parliamentary_constraint_reading' and
 *   'cohabitation_equilibrium_reading'. This reading interprets the
 *   constitution as granting the president a direct mandate from the nation,
 *   allowing for strong executive action, often bypassing the legislature.
 *   The high extractiveness and suppression reflect the concentration of
 *   power and the weakening of parliamentary checks.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.85).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.78).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, snare).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Hyper-Presidential Interpretation of Fifth Republic Constitution").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, 'a5ca2f33-0ca5-4573-9daf-093d2a3b83c8').
narrative_ontology:cs_kernel_codification('a5ca2f33-0ca5-4573-9daf-093d2a3b83c8', fixed_text).
narrative_ontology:cs_authority_grounding('a5ca2f33-0ca5-4573-9daf-093d2a3b83c8', practice).
narrative_ontology:cs_interpretation_layer_present('a5ca2f33-0ca5-4573-9daf-093d2a3b83c8').
narrative_ontology:cs_reading_relation('a5ca2f33-0ca5-4573-9daf-093d2a3b83c8', fifth_republic_constitution__parliamentary_constraint_reading, forecloses).
narrative_ontology:cs_reading_relation('a5ca2f33-0ca5-4573-9daf-093d2a3b83c8', fifth_republic_constitution__cohabitation_equilibrium_reading, forecloses).
narrative_ontology:cs_axiom('a5ca2f33-0ca5-4573-9daf-093d2a3b83c8', foundational, presidential_direct_mandate).
narrative_ontology:cs_axiom_status(presidential_direct_mandate, holdable).
narrative_ontology:cs_axiom_grounding('a5ca2f33-0ca5-4573-9daf-093d2a3b83c8', presidential_direct_mandate, conventional).
narrative_ontology:cs_axiom('a5ca2f33-0ca5-4573-9daf-093d2a3b83c8', foundational, executive_efficiency_supremacy).
narrative_ontology:cs_axiom_status(executive_efficiency_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('a5ca2f33-0ca5-4573-9daf-093d2a3b83c8', executive_efficiency_supremacy, instrumental).
narrative_ontology:cs_reference_frame('a5ca2f33-0ca5-4573-9daf-093d2a3b83c8', gaullist_vision_of_strong_state).
narrative_ontology:cs_drift_state('a5ca2f33-0ca5-4573-9daf-093d2a3b83c8', contemporary_political_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a5ca2f33-0ca5-4573-9daf-093d2a3b83c8', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, opposition_parties).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, citizens_seeking_legislative_oversight).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the directly elected head of state, the president is seen as embodying the national will, allowing them to bypass legislative processes through constitutional mechanisms like Article 49.3 (government responsibility on a text without a vote) or Article 16 (emergency powers). This position grants immense policy-making power and minimizes legislative checks.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    institutional, biographical, arbitrage, national).

% The institutional structure of the presidency benefits from this interpretation, accumulating power and prestige over time. Its identity is fused with the idea of a strong, decisive executive, making any reduction in power an existential threat to its self-conception.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution, beneficiary,
    institutional, generational, identity_locked, national).

% The legislative body, whose power to initiate and amend laws is significantly curtailed by the president's ability to invoke special constitutional articles. Its debates and votes can become largely performative when the president chooses to bypass them, leading to a loss of influence and public trust.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    institutional, biographical, constrained, national).

% These parties bear the cost of being unable to effectively counter presidential policy initiatives through parliamentary means. Their primary recourse is public protest or legal challenges, which are often insufficient to alter the president's course.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, opposition_parties, payer,
    organized, biographical, constrained, national).

% Citizens who desire a more robust parliamentary democracy and greater legislative accountability find their preferences suppressed. Their ability to influence policy through their elected representatives is diminished when the executive can act unilaterally.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, citizens_seeking_legislative_oversight, payer,
    powerless, immediate, constrained, national).

% The body responsible for reviewing the constitutionality of laws. While it can check presidential power, its role is primarily legal, not political, and this reading often frames presidential actions as within the bounds of constitutional interpretation, limiting the Council's practical impact on policy direction.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__hyper_presidential_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides strong, decisive executive leadership and governmental stability, particularly in times of crisis or legislative deadlock, by allowing the president to act swiftly and implement policy without prolonged parliamentary debate.
% TRANSFER_FUNCTION: Transfers significant legislative authority and policy-making power from the National Assembly to the Presidency, concentrating decision-making and accountability in the executive branch.
% ABSENT_VOICES: Citizens advocating for stronger parliamentary democracy, civil society groups concerned about executive overreach, and political parties marginalized by presidential dominance are structurally sidelined. They would argue for greater legislative influence and checks on executive power but are often excluded from effective participation by the constitutional mechanisms that empower the president.
% DISAPPEARANCE_RATIONALE: If this hyper-presidential interpretation vanished overnight, the balance of power would fundamentally shift towards the legislature. The president would be compelled to build broader consensus for policy, leading to a more parliamentary-style system with potentially less executive decisiveness but greater legislative influence and accountability. The entire political system would reorganize.
% FOUNDING_PROBLEM: The instability and perceived ineffectiveness of the Fourth Republic's parliamentary system, characterized by frequent government collapses and weak executive authority, which led to a desire for a stronger, more stable executive.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading (e.g., Gaullist political commentators, some constitutional scholars) attest that the founding problem of governmental instability remains a live concern, justifying strong presidential powers. Critics (e.g., opposition politicians, legal academics, civil society groups) argue that the founding problem is largely solved and the current interpretation leads to executive overreach and a democratic deficit, citing legislative debates, public protests, and comparative constitutional analysis from outside the benefiting parties.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__hyper_presidential_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high because the president can unilaterally implement policy, effectively extracting legislative power from the National Assembly. Suppression is also high due to the constitutional mechanisms (e.g., Article 49.3) that limit legislative opposition and debate. The theater ratio is moderate, as legislative processes still exist but can be rendered largely symbolic when the president chooses to exert full authority. The metrics show a slight increase over time, reflecting the entrenchment and expanded use of these presidential powers since the founding of the Fifth Republic.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the incumbent president and the institution of the presidency, this arrangement is a necessary and legitimate exercise of national will, ensuring stability and effective governance. From the perspective of the National Assembly, opposition parties, and citizens seeking legislative oversight, it is an extractive mechanism that suppresses democratic representation and accountability. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The incumbent president and the presidency as an institution are clear beneficiaries, gaining immense power and policy control (low directionality). The National Assembly, opposition parties, and citizens seeking legislative oversight are targets, experiencing a reduction in their influence and ability to shape policy (high directionality). The Constitutional Council acts as an observer, with its checks being legal rather than political, thus having an analytical directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling executive decisiveness as pure coordination by highlighting the asymmetric extraction of legislative power. While proponents argue the strong executive is essential for stability (a coordination function), the consistent use of bypass mechanisms and the resulting suppression of legislative alternatives reveal a substantial extractive component. The 'contested' status of the founding problem further suggests that the original mandate for stability may now serve as cover for sustained executive dominance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    presidential_mandate_legitimacy,
    'Does the president''s direct election genuinely embody the national will, or is this a rhetorical device to justify executive overreach?',
    'Longitudinal studies of public opinion on specific presidential actions versus parliamentary consensus, and comparative analysis with other directly elected presidencies.',
    'If it''s primarily a rhetorical device, the constraint''s legitimacy is weakened, and its extractive nature is more clearly exposed. If genuinely embodying national will, the coordination aspect is stronger, though extraction of legislative power remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(presidential_mandate_legitimacy, conceptual, 'Ambiguity of the president''s mandate as ''national will''.').

omega_variable(
    legislative_bypass_necessity,
    'Are constitutional mechanisms for bypassing the legislature (e.g., Article 49.3) genuinely necessary for governmental stability and efficiency, or are they primarily tools for executive dominance?',
    'Analysis of legislative outcomes in periods where these articles were used versus periods of cohabitation or strong parliamentary majorities, assessing policy effectiveness and stability.',
    'If unnecessary, the suppression of the legislature is purely extractive. If genuinely necessary in specific contexts, the constraint has a stronger coordination function, but its overuse still indicates extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_bypass_necessity, empirical, 'Whether legislative bypass is a functional necessity or an extractive tool.').

omega_variable(
    constitutional_council_effectiveness,
    'To what extent does the Constitutional Council effectively constrain presidential power under this interpretation, beyond purely legalistic checks?',
    'Case studies of Council rulings on presidential initiatives and their political impact, including instances where the Council has pushed back against executive overreach.',
    'If the Council''s checks are largely ineffective in practice, the constraint''s suppression is higher than currently measured, as a key institutional check is neutralized. If effective, it dampens the overall extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_council_effectiveness, empirical, 'Effectiveness of the Constitutional Council as a check on presidential power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1958, 0.3).
narrative_ontology:measurement(fift_tr_t1969, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1969, 0.35).
narrative_ontology:measurement(fift_tr_t1980, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1980, 0.38).
narrative_ontology:measurement(fift_tr_t1991, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1991, 0.4).
narrative_ontology:measurement(fift_tr_t2002, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2002, 0.42).
narrative_ontology:measurement(fift_tr_t2013, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2013, 0.44).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1958, 0.7).
narrative_ontology:measurement(fift_be_t1969, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1969, 0.75).
narrative_ontology:measurement(fift_be_t1980, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1980, 0.78).
narrative_ontology:measurement(fift_be_t1991, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1991, 0.8).
narrative_ontology:measurement(fift_be_t2002, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2002, 0.82).
narrative_ontology:measurement(fift_be_t2013, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2013, 0.84).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1958, 0.65).
narrative_ontology:measurement(fift_su_t1969, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1969, 0.68).
narrative_ontology:measurement(fift_su_t1980, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(fift_su_t1991, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1991, 0.73).
narrative_ontology:measurement(fift_su_t2002, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2002, 0.75).
narrative_ontology:measurement(fift_su_t2013, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2013, 0.77).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
