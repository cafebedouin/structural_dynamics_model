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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Fifth Republic Constitution: Hyper-Presidential Reading
 *   domain: constitutional_law/political_systems/comparative_government
 *
 * SUMMARY:
 *   This constraint describes the 'hyper_presidential_reading' of the Fifth
 *   Republic Constitution, where the President is seen as the direct
 *   embodiment of the national will, minimally constrained by the
 *   legislature. This interpretation leverages constitutional mechanisms like
 *   Article 49.3 (allowing government to pass a bill without a vote) and
 *   Article 16 (emergency powers) to centralize power in the executive. The
 *   claimed type is 'tangled_rope' because it provides a coordination
 *   function (stable governance) but with significant asymmetric extraction
 *   from the legislature and citizens. The high extractiveness and
 *   suppression reflect the executive's ability to bypass democratic checks.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.8).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.75).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Constitution: Hyper-Presidential Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional_law/political_systems/comparative_government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, 'dee5bb3a-3b87-4de2-9cd9-cdd9ea9703a7').
narrative_ontology:cs_kernel_codification('dee5bb3a-3b87-4de2-9cd9-cdd9ea9703a7', fixed_text).
narrative_ontology:cs_authority_grounding('dee5bb3a-3b87-4de2-9cd9-cdd9ea9703a7', lineage).
narrative_ontology:cs_interpretation_layer_present('dee5bb3a-3b87-4de2-9cd9-cdd9ea9703a7').
narrative_ontology:cs_reading_relation('dee5bb3a-3b87-4de2-9cd9-cdd9ea9703a7', fifth_republic_constitution__parliamentary_constraint_reading, forecloses).
narrative_ontology:cs_reading_relation('dee5bb3a-3b87-4de2-9cd9-cdd9ea9703a7', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('dee5bb3a-3b87-4de2-9cd9-cdd9ea9703a7', foundational, presidential_primacy_in_governance).
narrative_ontology:cs_axiom_status(presidential_primacy_in_governance, holdable).
narrative_ontology:cs_axiom_grounding('dee5bb3a-3b87-4de2-9cd9-cdd9ea9703a7', presidential_primacy_in_governance, conventional).
narrative_ontology:cs_axiom('dee5bb3a-3b87-4de2-9cd9-cdd9ea9703a7', foundational, direct_mandate_from_nation).
narrative_ontology:cs_axiom_status(direct_mandate_from_nation, holdable).
narrative_ontology:cs_axiom_grounding('dee5bb3a-3b87-4de2-9cd9-cdd9ea9703a7', direct_mandate_from_nation, deontological).
narrative_ontology:cs_reference_frame('dee5bb3a-3b87-4de2-9cd9-cdd9ea9703a7', de_gaulle_founding_vision).
narrative_ontology:cs_drift_state('dee5bb3a-3b87-4de2-9cd9-cdd9ea9703a7', contemporary_political_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dee5bb3a-3b87-4de2-9cd9-cdd9ea9703a7', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, citizens_as_legislative_principals).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, presidential_supremacy_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, national_unity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The constitutional office of the President, which benefits from expanded powers to implement policy decisively, often bypassing legislative hurdles. This reading sees the institution as the primary driver of national policy.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution, agenda_setter,
    institutional, civilizational, arbitrage, national).

% The individual holding the presidential office, directly exercising the constitutional mechanisms (like Article 49.3 or Article 16) to assert executive will and implement their political agenda, embodying the national will as per this reading.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    powerful, biographical, mobile, national).

% The legislative body whose power to debate, amend, and block legislation is significantly curtailed by the hyper-presidential interpretation, particularly when the president invokes specific constitutional articles. They are forced to accept presidential decrees or face dissolution.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    organized, biographical, constrained, national).

% The electorate whose representative voice, channeled through the National Assembly, is bypassed when the president uses constitutional mechanisms to enact policy. They bear the costs of reduced democratic accountability and legislative oversight.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, citizens_as_legislative_principals, payer,
    organized, biographical, constrained, national).

% The body responsible for reviewing the constitutionality of laws and presidential actions. While it acts as a check, its interpretations are made within the framework of the existing constitution, often affirming the broad scope of presidential powers under this reading.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% Political parties that oppose the hyper-presidential interpretation and its use of constitutional mechanisms. While they can voice dissent and organize protests, they are structurally limited in their ability to block presidential initiatives due to the constitutional design.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, opposition_parties, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__hyper_presidential_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables decisive and stable governance by centralizing executive power, preventing legislative gridlock, and ensuring policy continuity, particularly in times of crisis or when a clear national direction is deemed necessary.
% TRANSFER_FUNCTION: Transfers significant legislative authority and policy-making initiative from the National Assembly to the Presidency, along with the political capital and accountability associated with those functions, concentrating power in the executive.
% ABSENT_VOICES: Opposition parties and civil society groups advocating for stronger parliamentary oversight are present but structurally marginalized; their objections are often overridden by constitutional mechanisms like Article 49.3, which allows the government to pass a bill without a vote.
% DISAPPEARANCE_RATIONALE: If the hyper-presidential interpretation vanished, the balance of power would shift dramatically back to the legislature, requiring new mechanisms for executive-legislative cooperation. This would likely lead to more frequent government instability, increased legislative gridlock, and a fundamental reorganization of the French political system.
% FOUNDING_PROBLEM: The perceived instability and governmental paralysis of the Fourth Republic's parliamentary system, which led to a desire for stronger, more stable executive leadership capable of decisive action, particularly during the Algerian War.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the hyper-presidential reading (e.g., some political scientists, former presidents) argue the problem of instability remains a threat, justifying strong executive powers. Critics (e.g., opposition politicians, some legal scholars) attest that the founding problem is substantially solved and the current interpretation over-empowers the executive, citing historical periods of cohabitation as evidence of parliamentary viability. Legislative-hearing testimony and independent academic analysis from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__hyper_presidential_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.8) reflects the significant transfer of legislative power to the executive. Suppression (0.75) is high due to the constitutional mechanisms that actively curtail legislative alternatives and resistance. The theater ratio (0.4) indicates that while legislative debate and processes exist, their ultimate impact can be performative when the president can override them. Accessibility collapse (0.7) is substantial as legislative avenues for opposition are severely limited. Resistance (0.6) is moderate, reflecting ongoing political and public opposition, which, however, struggles to overcome the constitutional design. The measurement series shows a gradual increase in extractiveness and suppression over time, reflecting the ongoing assertion of presidential power within this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Presidency (both the institution and the incumbent), this arrangement is a legitimate and effective mechanism for stable governance and national unity. From the perspective of the National Assembly and citizens, it is an extractive structure that diminishes democratic accountability and legislative power. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Presidency (institution and incumbent) are clear beneficiaries and agenda-setters, collecting the gains of centralized power. The National Assembly and citizens are victims/payers, bearing the costs of reduced legislative influence and democratic bypass. The Constitutional Council acts as an observer, interpreting the constitution within this framework. Opposition parties are excluded, as their structural position prevents them from effectively challenging the core mechanisms of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, originating from a desire for stable governance after the Fourth Republic's instability, is contested. While proponents argue it still serves to prevent gridlock, critics contend its function has drifted towards rent-seeking (power concentration) rather than pure coordination. The 'tangled_rope' classification acknowledges both the coordination function and the asymmetric extraction, preventing mislabeling it as a 'rope' (ignoring extraction) or a 'snare' (ignoring the coordination story).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_intent_ambiguity,
    'Does the Fifth Republic Constitution genuinely intend a hyper-presidential system, or is this an interpretation that has evolved beyond its original design?',
    'Historical analysis of constitutional debates, originalist legal scholarship, and comparative constitutional studies of similar systems.',
    'If the original intent was less hyper-presidential, this reading''s legitimacy would be weakened, supporting calls for constitutional reform or reinterpretation to restore legislative balance. If it aligns with original intent, the reading is structurally robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_intent_ambiguity, conceptual, 'Ambiguity regarding the original constitutional intent for executive power.').

omega_variable(
    democratic_accountability_tradeoff,
    'Is the increased governmental stability and decisiveness achieved by this hyper-presidential reading a necessary tradeoff for reduced democratic accountability, or can both be achieved?',
    'Empirical study of policy outcomes and public trust in systems with varying executive-legislative balances, and analysis of alternative constitutional designs that aim for both stability and accountability.',
    'If a strong negative correlation between stability and accountability is found, the reading''s justification as a necessary evil is strengthened. If alternatives achieve both, the reading''s extractive nature becomes harder to justify.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_accountability_tradeoff, empirical, 'The inherent tradeoff between executive stability and democratic accountability.').

omega_variable(
    national_will_representation,
    'Does the President, through direct election, genuinely embody the ''national will'' more effectively than a representative legislature, or is this a rhetorical device to justify executive dominance?',
    'Public opinion polling on specific policy issues compared to presidential actions, and analysis of legislative representation of diverse societal interests versus presidential mandates.',
    'If presidential actions consistently diverge from public opinion or legislative consensus, the ''national will'' claim is undermined, highlighting the extractive nature of the reading. If alignment is high, the claim gains empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_will_representation, empirical, 'Whether the President truly embodies the national will or uses the claim for political leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t2000, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(fift_tr_t2005, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2005, 0.29).
narrative_ontology:measurement(fift_tr_t2010, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement(fift_tr_t2015, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(fift_tr_t2020, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(fift_tr_t2025, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2025, 0.39).
narrative_ontology:measurement(fift_tr_t2030, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2030, 0.4).

% Extraction over time
narrative_ontology:measurement(fift_be_t2000, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(fift_be_t2005, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2005, 0.69).
narrative_ontology:measurement(fift_be_t2010, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2010, 0.73).
narrative_ontology:measurement(fift_be_t2015, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement(fift_be_t2020, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement(fift_be_t2025, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2025, 0.79).
narrative_ontology:measurement(fift_be_t2030, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2030, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t2000, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(fift_su_t2005, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2005, 0.64).
narrative_ontology:measurement(fift_su_t2010, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(fift_su_t2015, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(fift_su_t2020, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2020, 0.73).
narrative_ontology:measurement(fift_su_t2025, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2025, 0.74).
narrative_ontology:measurement(fift_su_t2030, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2030, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, french_legislative_process_constraint).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, french_electoral_system_constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
