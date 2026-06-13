% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__parliamentary_primacy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Primacy in Constitutional Interpretation
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'parliamentary primacy' reading of the
 *   constitutional authority boundary, where the elected legislature holds
 *   final authority to define constitutional meaning. This reading positions
 *   the constitutional text as subordinate to the will of the parliament,
 *   with the judiciary's role limited to advisory or easily-overridden
 *   review. It is a specific interpretation of institutional design that
 *   prioritizes democratic legitimacy through elected representatives.
 *
 * KEY AGENTS:
 *   - elected_legislature: Primary beneficiary (institutional/arbitrage) — defines constitutional meaning
 *   - judiciary: Primary victim (institutional/constrained) — limited interpretive authority
 *   - electorate: Secondary beneficiary (organized/mobile) — democratic will is paramount
 *   - minority_groups: Secondary victim (powerless/constrained) — vulnerable to legislative majorities
 *   - constitutional_scholars: Analytical observer (analytical/analytical) — analyze the structural implications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.3).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__parliamentary_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '7ac95a1d-3c92-4975-a6b7-de0282a274ae').
narrative_ontology:cs_kernel_codification('7ac95a1d-3c92-4975-a6b7-de0282a274ae', formalized).
narrative_ontology:cs_authority_grounding('7ac95a1d-3c92-4975-a6b7-de0282a274ae', lineage).
narrative_ontology:cs_interpretation_layer_present('7ac95a1d-3c92-4975-a6b7-de0282a274ae').
narrative_ontology:cs_reading_relation('7ac95a1d-3c92-4975-a6b7-de0282a274ae', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('7ac95a1d-3c92-4975-a6b7-de0282a274ae', constitutional_authority_boundary__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('7ac95a1d-3c92-4975-a6b7-de0282a274ae', foundational, democratic_will_is_supreme).
narrative_ontology:cs_axiom_status(democratic_will_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('7ac95a1d-3c92-4975-a6b7-de0282a274ae', democratic_will_is_supreme, deontological).
narrative_ontology:cs_axiom('7ac95a1d-3c92-4975-a6b7-de0282a274ae', foundational, legislature_is_final_arbiter).
narrative_ontology:cs_axiom_status(legislature_is_final_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('7ac95a1d-3c92-4975-a6b7-de0282a274ae', legislature_is_final_arbiter, conventional).
narrative_ontology:cs_reference_frame('7ac95a1d-3c92-4975-a6b7-de0282a274ae', westminster_parliamentary_tradition).
narrative_ontology:cs_drift_state('7ac95a1d-3c92-4975-a6b7-de0282a274ae', contemporary_human_rights_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('7ac95a1d-3c92-4975-a6b7-de0282a274ae', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, electorate).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, minority_groups).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, democratic_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, parliamentary_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the elected body, it claims the ultimate authority to interpret and define constitutional meaning, either through ordinary legislation or entrenched constitutional amendments. It benefits from the ability to enact its policy agenda without significant judicial impediment.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Its role in constitutional interpretation is subordinate to the legislature. While it may offer advisory opinions or conduct weak-form review, its interpretations can be overridden by parliamentary action. It bears the cost of diminished institutional autonomy and interpretive finality.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary, payer,
    institutional, generational, constrained, national).

% Benefits from the principle that their elected representatives have the final say on constitutional matters, ensuring that the constitution reflects contemporary democratic will. Their power is exercised through elections, holding the legislature accountable.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, electorate, beneficiary,
    organized, biographical, mobile, national).

% Are vulnerable to the legislative majority's interpretation of constitutional rights, as there is no strong, independent judicial check to protect their interests. They bear the cost of potentially being excluded from the constitutional consensus.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, minority_groups, payer,
    powerless, generational, constrained, national).

% Analyze the theoretical and practical implications of parliamentary primacy, comparing it with other models of constitutional authority. They do not directly benefit or pay but provide critical analysis of the system's operation and legitimacy.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__parliamentary_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ultimate authority for constitutional interpretation within a democratic system, ensuring that the will of the elected legislature, representing the people, is supreme.
% TRANSFER_FUNCTION: Transfers final interpretive authority over constitutional meaning from the judiciary to the elected legislature, and potentially transfers the burden of constitutional protection for minority rights from courts to political processes.
% ABSENT_VOICES: Advocates for strong-form judicial review and proponents of entrenched constitutional rights that are beyond ordinary legislative amendment are structurally marginalized. They would argue for a more robust role for courts in protecting fundamental liberties.
% DISAPPEARANCE_RATIONALE: If parliamentary primacy in constitutional interpretation vanished, the balance of power would fundamentally shift. The judiciary would likely assert greater interpretive authority, leading to potential conflicts with the legislature and a re-evaluation of constitutional review mechanisms. The entire institutional framework would need to re-coordinate around a new understanding of constitutional supremacy.
% FOUNDING_PROBLEM: The problem of ensuring that constitutional meaning remains responsive to democratic will and preventing an unelected judiciary from thwarting the legitimate policy choices of the people's representatives.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and legal scholars (outside the legislature itself) attest that the tension between democratic accountability and judicial independence is a persistent, live problem in many constitutional systems. Public opinion polls often show support for elected bodies having final say, corroborating the ongoing relevance of this founding problem from the perspective of the electorate.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).
:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.2) is low because this reading is framed as a legitimate expression of democratic will, with costs primarily borne by the judiciary's interpretive autonomy. Suppression (0.3) is moderate, reflecting the structural limitations on judicial review and the potential for legislative majorities to override dissenting voices. Theater ratio (0.1) is low, as the constraint's operation is largely direct and functional, not performative. The claimed type is 'rope' because it's presented as a coordination mechanism for democratic governance, even if it involves some extraction from other branches.
 *
 * PERSPECTIVAL GAP:
 *   The elected legislature perceives this as a legitimate and necessary coordination mechanism for democratic governance, ensuring the will of the people is supreme. The judiciary, however, experiences it as a constraint on its independence and ability to protect fundamental rights, seeing its interpretive authority diminished. Minority groups may experience it as a vulnerability, as their rights are subject to legislative majorities without strong judicial protection.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature is the primary beneficiary (d=0.0-0.1) as it gains ultimate interpretive authority. The electorate is also a beneficiary (d=0.1-0.2) as their democratic will is prioritized. The judiciary is a primary target (d=0.8-0.9) as its interpretive power is curtailed. Minority groups are also targets (d=0.7-0.8) due to potential vulnerability to legislative majorities. Constitutional scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandate is to ensure democratic accountability in constitutional interpretation. The classification as a 'rope' suggests that, from the perspective of its proponents, it genuinely coordinates democratic governance. However, the presence of victims (judiciary, minority groups) and the 'contested' status of the founding problem (whether it's still about preventing fragmentation or about legislative power) indicate a potential for drift towards a 'tangled rope' or 'snare' if the extractive aspects intensify or the coordination function atrophies. The omegas address this tension directly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parliamentary_primacy_vs_judicial_supremacy,
    'Is this constraint a genuine expression of democratic will, or a mechanism for legislative overreach that forecloses essential checks and balances?',
    'Analysis of legislative actions: if parliamentary interpretations consistently override fundamental rights protections without effective recourse, it leans towards overreach. If judicial review, even if advisory, consistently influences legislative outcomes, it leans towards genuine democratic expression.',
    'If it''s primarily legislative overreach, the constraint''s effective extractiveness on the judiciary and minority groups is higher, potentially reclassifying it as a Tangled Rope or Snare from those seats. If it''s a genuine expression of democratic will, it remains a Rope, with the judiciary''s role understood as advisory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_primacy_vs_judicial_supremacy, conceptual, 'Ambiguity between democratic expression and legislative overreach in constitutional interpretation.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''parliamentary_primacy_reading'' of the ''constitutional_authority_boundary'' kernel. What would change if the ''judicial_supremacy_reading'' were adopted?',
    'A shift in legal precedent or constitutional amendment establishing judicial review as final and unchallengeable.',
    'The primary beneficiary would shift from the legislature to the judiciary, and the judiciary''s exit options would move from ''constrained'' to ''arbitrage'' or ''analytical''. The extractiveness on the judiciary would drop significantly, and the constraint would likely reclassify to a Mountain or Rope from the judicial seat, and a Snare from the legislative seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of adopting the ''judicial_supremacy_reading'' of the ''constitutional_authority_boundary'' kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of judicial and minority voices structural (legislative power, lack of legal avenues) or internalized (judicial deference, public acceptance of parliamentary authority)?',
    'Post-legislative challenge trajectory: if judicial challenges persist and gain public support even after legislative overrides, reclassify as partially internalized. If challenges are consistently suppressed by legal and political means, it''s structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the judiciary and minority groups carry the suppression with them. If structural, the constraint''s persistence depends more on active legislative enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for judicial and minority voices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cons_tr_t20, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cons_be_t10, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(cons_be_t20, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 20, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cons_su_t10, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(cons_su_t20, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'constitutional_authority_boundary' kernel. It is structurally distinct from the 'judicial_supremacy_reading' and 'coordinate_construction_reading' due to differing beneficiary/victim structures and interpretive authority allocations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
