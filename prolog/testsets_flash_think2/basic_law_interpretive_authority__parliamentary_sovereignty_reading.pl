% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty over Basic Law Interpretation
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'parliamentary sovereignty' reading of
 *   basic law interpretive authority, where the elected legislature holds
 *   final say on constitutional meaning. It is presented as a mechanism for
 *   democratic accountability, ensuring that the will of the people,
 *   expressed through their representatives, is supreme. However, this
 *   finality can come at the cost of judicial independence and the protection
 *   of minority rights, which are subject to legislative override.
 *
 * KEY AGENTS:
 *   - Elected_legislature: Primary beneficiary/agenda_setter (institutional/arbitrage) — benefits from final authority.
 *   - Majority_electorate: Primary beneficiary (organized/mobile) — benefits from direct representation.
 *   - Judicial_branch: Primary payer (institutional/constrained) — bears costs of overridden decisions.
 *   - Rights_minorities: Primary payer (powerless/trapped) — bears costs of legislative overrides on rights.
 *   - Constitutional_scholars: Analytical observer (analytical/analytical) — analyzes the system.
 *   - Opposition_parties: Secondary payer (organized/constrained) — bears costs of majority interpretive dominance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.68).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.75).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty over Basic Law Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'c436864c-1bd3-444e-961e-24e6f212af10').
narrative_ontology:cs_kernel_codification('c436864c-1bd3-444e-961e-24e6f212af10', formalized).
narrative_ontology:cs_authority_grounding('c436864c-1bd3-444e-961e-24e6f212af10', lineage).
narrative_ontology:cs_interpretation_layer_present('c436864c-1bd3-444e-961e-24e6f212af10').
narrative_ontology:cs_reading_relation('c436864c-1bd3-444e-961e-24e6f212af10', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('c436864c-1bd3-444e-961e-24e6f212af10', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('c436864c-1bd3-444e-961e-24e6f212af10', foundational, legislative_supremacy_in_interpretation).
narrative_ontology:cs_axiom_status(legislative_supremacy_in_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('c436864c-1bd3-444e-961e-24e6f212af10', legislative_supremacy_in_interpretation, conventional).
narrative_ontology:cs_axiom('c436864c-1bd3-444e-961e-24e6f212af10', foundational, democratic_accountability_is_final_check).
narrative_ontology:cs_axiom_status(democratic_accountability_is_final_check, holdable).
narrative_ontology:cs_axiom_grounding('c436864c-1bd3-444e-961e-24e6f212af10', democratic_accountability_is_final_check, deontological).
narrative_ontology:cs_reference_frame('c436864c-1bd3-444e-961e-24e6f212af10', westminster_tradition_supremacy).
narrative_ontology:cs_drift_state('c436864c-1bd3-444e-961e-24e6f212af10', contemporary_constitutional_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c436864c-1bd3-444e-961e-24e6f212af10', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, majority_electorate).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_independence).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_branch).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, opposition_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority on interpreting the basic law, translating democratic mandate into legal supremacy. Benefits from unchecked legislative power and avoids judicial vetoes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the direct implementation of its will through elected representatives, without judicial obstruction. Experiences the constraint as a direct expression of popular sovereignty.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, majority_electorate, beneficiary,
    organized, biographical, mobile, national).

% Bears the cost of having its interpretations overridden by the legislature. Its independence and ability to act as a check on legislative power are curtailed, leading to potential gridlock costs when its decisions are challenged.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_branch, payer,
    institutional, generational, constrained, national).

% Are vulnerable to legislative majorities overriding their rights or interests without effective judicial recourse. Bears the cost of potentially unprotected rights.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities, payer,
    powerless, generational, trapped, national).

% Analyze the theoretical and practical implications of parliamentary sovereignty, often highlighting its tension with minority rights and judicial review.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% While part of the legislature, they bear the cost of the majority party's interpretive dominance when not in power. Their ability to challenge constitutional interpretations is limited to political means rather than judicial ones.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, opposition_parties, payer,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the democratic will of the populace, as expressed through their elected representatives, into final and binding interpretations of the basic law, ensuring legislative supremacy.
% TRANSFER_FUNCTION: Transfers final interpretive authority from potentially independent judicial bodies to the elected legislature, and from individual rights claims to collective legislative decisions.
% ABSENT_VOICES: Advocates for robust judicial review, international human rights organizations, and future generations whose interests might not be fully represented by current legislative majorities.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty over basic law interpretation vanished, the constitutional order would become highly unstable. Without a clear final arbiter, different institutions (judiciary, executive, sub-national bodies) would assert competing interpretations, leading to severe institutional gridlock and a breakdown of legal certainty.
% FOUNDING_PROBLEM: To prevent unelected judicial bodies from thwarting the will of the people, ensuring that the ultimate authority in a democracy rests with those accountable to the electorate.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists and historians of constitutional development corroborate the historical and ongoing debate about the proper balance between democratic accountability and judicial review. Public opinion often supports legislative finality on matters of policy, even if the specific mechanism is debated by legal experts.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because while it coordinates democratic will, it also enables the extraction of interpretive power from the judiciary and can impose costs on minority rights. Suppression is high (0.75) as it actively suppresses alternative interpretive authorities (e.g., judicial review) and limits avenues for challenging legislative decisions. Theater ratio is low (0.15) because the system is functional and actively enforced, not merely performative. Accessibility collapse is high (0.70) because once the legislature has spoken, alternatives for constitutional interpretation are largely foreclosed. Resistance is moderate (0.55) from judicial advocates and rights groups, but often channeled through political rather than legal means.
 *
 * PERSPECTIVAL GAP:
 *   The elected legislature and majority electorate perceive this constraint as a legitimate expression of democratic will and a necessary coordination mechanism. Conversely, the judicial branch and rights minorities experience it as a form of extraction, where their interpretive authority or protections are suppressed in favor of majoritarian legislative power. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature and the majority electorate are clear beneficiaries, as the constraint empowers them and ensures their will is paramount. The judicial branch and rights minorities are targets, as their capacity to act as checks or to assert independent rights is diminished. Opposition parties, while part of the legislative process, are also payers when the majority's interpretation prevails.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the clear victims) or a Snare (which would ignore the genuine coordination function of democratic representation). It acknowledges the dual nature: a coordination of democratic will that simultaneously enables asymmetric extraction of interpretive authority and imposes costs on certain groups. The founding problem of ensuring democratic supremacy is still live, but its implementation through parliamentary sovereignty creates ongoing tensions and costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_mandate_vs_majoritarian_oppression,
    'Does the legislative finality truly represent the democratic will, or does it enable majoritarian oppression of minority rights and interests?',
    'Longitudinal studies of legislative outcomes concerning minority groups, comparative analysis with systems employing strong judicial review, and public opinion surveys on constitutional values beyond electoral cycles.',
    'If it primarily enables oppression, the extractiveness and suppression metrics would be re-evaluated upwards, potentially shifting the classification closer to a Snare for affected seats. If it consistently reflects broad public consensus, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_mandate_vs_majoritarian_oppression, conceptual, 'Ambiguity between democratic representation and majoritarian tyranny.').

omega_variable(
    gridlock_cost_comparison,
    'Are the gridlock costs associated with legislative override truly lower than those of strong judicial review, or are they merely shifted to different institutional points?',
    'Empirical studies comparing legislative efficiency and policy stability in parliamentary sovereignty systems versus those with robust judicial review, accounting for all institutional friction points.',
    'If gridlock costs are merely shifted or even higher, the justification for suppressing judicial independence weakens, increasing the perceived extractiveness from the judicial seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gridlock_cost_comparison, empirical, 'Comparison of institutional friction costs under different interpretive authorities.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''parliamentary sovereignty'' reading of basic law interpretive authority, or is it a hybrid or diluted form?',
    'Detailed legal-historical analysis of constitutional practice and jurisprudence in specific jurisdictions, comparing against the ideal-type definition of parliamentary sovereignty.',
    'If it''s a diluted form, its structural properties (e.g., extractiveness, suppression) might be lower than the ideal type, and its relationship to sibling readings (e.g., judicial supremacy) might be ''coexists_with'' rather than ''forecloses''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Verifying the purity of the ''parliamentary sovereignty'' reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1950, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(basi_tr_t1960, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(basi_tr_t1970, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(basi_tr_t1980, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(basi_tr_t1990, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(basi_tr_t2010, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(basi_tr_t2020, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(basi_be_t1950, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(basi_be_t1960, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 1960, 0.62).
narrative_ontology:measurement(basi_be_t1970, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 1970, 0.64).
narrative_ontology:measurement(basi_be_t1980, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(basi_be_t1990, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 1990, 0.66).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement(basi_be_t2010, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(basi_be_t2020, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1950, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(basi_su_t1960, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 1960, 0.71).
narrative_ontology:measurement(basi_su_t1970, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 1970, 0.72).
narrative_ontology:measurement(basi_su_t1980, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 1980, 0.73).
narrative_ontology:measurement(basi_su_t1990, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 1990, 0.74).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(basi_su_t2010, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(basi_su_t2020, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'basic_law_interpretive_authority' kernel, alongside 'basic_law_interpretive_authority__judicial_supremacy_reading' and 'basic_law_interpretive_authority__popular_constitutionalism_reading'. Each reading instantiates a distinct constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
