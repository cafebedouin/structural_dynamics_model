% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: UNSC Article 27 Veto Power (Oligopoly Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint models the P5 veto power in the UN Security Council as a
 *   mechanism for geopolitical oligopoly, where the permanent members (P5)
 *   leverage the immutability of the UN Charter to maintain their
 *   disproportionate authority and block institutional reforms that would
 *   redistribute power. This reading emphasizes the extractive nature of the
 *   veto, sustained by the suppression of alternatives for the non-P5
 *   majority, rather than its claimed function as a guarantor of great-power
 *   coordination or national sovereignty. The constraint is classified as a
 *   Snare due to its high extraction, active suppression, and identifiable
 *   victims.
 *
 * KEY AGENTS:
 *   - permanent_five_members: Primary beneficiary (institutional/arbitrage) — extracts authority rents
 *   - non_p5_member_states: Primary target (institutional/trapped) — victims of blocked reform
 *   - global_majority_coalition: Secondary target (organized/constrained) — collective victim of structural entrenchment
 *   - un_secretariat: Agenda setter (institutional/constrained) — administers the system but cannot alter the veto
 *   - international_law_scholars: Observer (analytical/analytical) — analyze the structural effects of the veto
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.85).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.9).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "UNSC Article 27 Veto Power (Oligopoly Reading)").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, '8892d970-eaba-4a91-b67d-f861765c44e2').
narrative_ontology:cs_kernel_codification('8892d970-eaba-4a91-b67d-f861765c44e2', fixed_text).
narrative_ontology:cs_authority_grounding('8892d970-eaba-4a91-b67d-f861765c44e2', extraction).
narrative_ontology:cs_interpretation_layer_present('8892d970-eaba-4a91-b67d-f861765c44e2').
narrative_ontology:cs_reading_relation('8892d970-eaba-4a91-b67d-f861765c44e2', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('8892d970-eaba-4a91-b67d-f861765c44e2', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('8892d970-eaba-4a91-b67d-f861765c44e2', foundational, geopolitical_oligopoly_is_structural).
narrative_ontology:cs_axiom_status(geopolitical_oligopoly_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('8892d970-eaba-4a91-b67d-f861765c44e2', geopolitical_oligopoly_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('8892d970-eaba-4a91-b67d-f861765c44e2', foundational, charter_immutability_blocks_reform).
narrative_ontology:cs_axiom_status(charter_immutability_blocks_reform, holdable).
narrative_ontology:cs_axiom_grounding('8892d970-eaba-4a91-b67d-f861765c44e2', charter_immutability_blocks_reform, empirically_contingent).
narrative_ontology:cs_reference_frame('8892d970-eaba-4a91-b67d-f861765c44e2', post_wwii_great_power_consensus).
narrative_ontology:cs_drift_state('8892d970-eaba-4a91-b67d-f861765c44e2', contemporary_multipolar_world, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8892d970-eaba-4a91-b67d-f861765c44e2', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, permanent_five_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_member_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, global_majority_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five permanent members of the UN Security Council (China, France, Russia, United Kingdom, United States) who possess the veto power. They use this power to protect their national interests, block resolutions they oppose, and prevent any reform of the Security Council that would dilute their authority. They benefit from the structural entrenchment of their position.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, permanent_five_members, beneficiary,
    institutional, generational, arbitrage, global).

% The vast majority of UN member states who do not have veto power. They are subject to the P5's decisions and vetoes, often finding their initiatives blocked and their collective will overridden. They bear the cost of an unreformed, oligarchical security architecture and have no effective means to exit or reform the system.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_member_states, payer,
    institutional, generational, trapped, global).

% Various blocs of non-P5 states (e.g., G77, African Union, Non-Aligned Movement) that collectively advocate for Security Council reform, including limitations on the veto or expansion of permanent membership. Their efforts are consistently suppressed by the P5's ability to veto any such reform.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, global_majority_coalition, payer,
    organized, generational, constrained, global).

% The administrative body of the UN, led by the Secretary-General. While responsible for implementing Security Council resolutions and facilitating diplomatic efforts, the Secretariat operates within the structural constraints imposed by the P5 veto and cannot independently challenge or alter its existence.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, un_secretariat, agenda_setter,
    institutional, biographical, constrained, global).

% Academics and legal experts who analyze the structure, function, and impact of the P5 veto on international law and global governance. They provide critical analysis but have no direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__oligopoly_reading, permanent_five_members).
narrative_ontology:fixing_cost_class(article_27_veto_power__oligopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The veto was originally conceived to ensure that no great power would be compelled by the Security Council into military action against its vital interests, thereby preventing a direct conflict among nuclear-armed states and maintaining a fragile global peace.
% TRANSFER_FUNCTION: Transfers ultimate decision-making authority and the power to block collective action from the broader UN membership to the five permanent members of the Security Council, effectively granting them a structural veto over global security governance.
% ABSENT_VOICES: The voices of states that existed outside the post-WWII power structure, particularly those from the Global South and newly independent nations, were absent during the Charter's drafting. Today, their collective voice, advocating for a more equitable and representative Security Council, is consistently overridden by the P5's veto power.
% DISAPPEARANCE_RATIONALE: If the P5 veto power vanished overnight, the UN Security Council would immediately become a more democratic and potentially more active body. Resolutions on critical global issues, currently blocked by a single veto, would pass. This would fundamentally alter the balance of power in international relations, leading to a significant rearrangement of global governance structures and diplomatic alignments.
% FOUNDING_PROBLEM: The founding problem was to create a global security organization that could prevent future world wars, while simultaneously accommodating the sovereign interests and military capabilities of the victorious great powers of World War II, particularly their ability to avoid being bound by resolutions they opposed.
% FOUNDING_PROBLEM_CORROBORATION: The P5 members and their allies often argue that the founding problem (preventing great-power conflict) remains live, citing ongoing geopolitical tensions. However, the vast majority of non-P5 member states, supported by numerous international law scholars and UN reform advocates, contend that the problem has evolved, and the veto now primarily serves to entrench an outdated power structure, blocking effective responses to new global challenges. This is corroborated by decades of blocked resolutions on humanitarian crises and conflicts where P5 interests diverged from the global consensus.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the P5 members derive significant, ongoing authority rents from their ability to unilaterally block resolutions and reforms, effectively maintaining a global status quo that benefits them. Suppression (0.90) is also very high, as the Charter's immutability and the P5's power make any meaningful reform or exit for non-P5 states virtually impossible. The theater ratio (0.10) is low because the veto is actively and functionally used to protect P5 interests, not merely for performative maintenance. Resistance (0.75) is high, reflecting persistent calls for reform from the non-P5 majority. Accessibility collapse (0.95) is near total, as there are no viable alternative global security architectures that could bypass the P5's entrenched power.
 *
 * PERSPECTIVAL GAP:
 *   The P5 members experience the veto as a legitimate exercise of their sovereign power and a necessary tool for global stability (a Mountain or Rope from their perspective). The non-P5 member states, however, experience it as an arbitrary and extractive mechanism that denies them agency and blocks necessary institutional evolution (a Snare from their perspective). The engine's per-seat classification will reflect this divergence based on the declared roles and attributes.
 *
 * DIRECTIONALITY LOGIC:
 *   The Permanent Five members are the clear beneficiaries (d=0.0-0.1) as the veto directly grants them disproportionate power and protects their interests. Non-P5 member states and the global majority coalition are the victims (d=0.9-1.0) as they are subject to the P5's decisions without recourse to reform. The UN Secretariat, while administering the system, is constrained by the veto's existence and cannot alter its fundamental structure (d=0.5-0.6).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing great-power war) is often cited, but this reading argues that its function has drifted to structural entrenchment of an oligopoly. The high extractiveness and suppression, coupled with the 'contested' status of the founding problem, indicate that the original coordination function has been superseded by an extractive one. This prevents mislabeling it as a Rope or Mountain, which would ignore the active harm and suppressed alternatives for the majority of states.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_function_ambiguity,
    'Is the P5 veto primarily a coordination mechanism to prevent great-power conflict, or a tool for geopolitical oligopoly and rent extraction?',
    'Analysis of veto usage patterns: if vetoes consistently block resolutions that would redistribute power or challenge P5 interests, rather than preventing direct military confrontation among P5, the oligopoly reading is strengthened.',
    'If primarily coordination, the constraint would reclassify as a Tangled Rope or Rope; if primarily oligopoly, it remains a Snare with high extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_function_ambiguity, conceptual, 'Ambiguity between coordination and extraction functions of the veto.').

omega_variable(
    charter_immutability_vs_evolution,
    'Is the UN Charter''s immutability a necessary feature for global stability, or a structural barrier to institutional evolution that reflects changed geopolitical realities?',
    'Historical analysis of proposed reforms and their impact on global stability, counterfactual modeling of alternative institutional designs, and assessment of the costs of non-reform (e.g., Security Council irrelevance).',
    'If immutability is necessary, the constraint''s suppression of reform is a Mountain-like feature; if a barrier, it reinforces the Snare classification by highlighting suppressed alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_immutability_vs_evolution, empirical, 'Whether Charter immutability is a feature or a bug for global governance.').

omega_variable(
    oligopoly_reading_of_article_27,
    'This constraint is the ''oligopoly_reading'' of the ''article_27_veto_power'' kernel. What would change if a ''coordination_reading'' or ''sovereignty_reading'' were adopted?',
    'Adopting the ''coordination_reading'' would emphasize the veto''s role in preventing great-power war, likely reducing perceived extractiveness and suppression, potentially reclassifying it as a Tangled Rope. Adopting the ''sovereignty_reading'' would frame the veto as an extension of state consent, reducing perceived extraction for P5 members but not for non-P5 states.',
    'The classification would shift from Snare to Tangled Rope or Rope under the coordination reading, and remain Snare but with a different justification under the sovereignty reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oligopoly_reading_of_article_27, conceptual, 'Impact of alternative readings of the P5 veto power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__oligopoly_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(arti_tr_t20, article_27_veto_power__oligopoly_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(arti_tr_t40, article_27_veto_power__oligopoly_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__oligopoly_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__oligopoly_reading, theater_ratio, 80, 0.1).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__oligopoly_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(arti_be_t20, article_27_veto_power__oligopoly_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(arti_be_t40, article_27_veto_power__oligopoly_reading, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__oligopoly_reading, base_extractiveness, 60, 0.83).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__oligopoly_reading, base_extractiveness, 80, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__oligopoly_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(arti_su_t20, article_27_veto_power__oligopoly_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(arti_su_t40, article_27_veto_power__oligopoly_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__oligopoly_reading, suppression_requirement, 60, 0.88).
narrative_ontology:measurement(arti_su_t80, article_27_veto_power__oligopoly_reading, suppression_requirement, 80, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, unsc_reform_process).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, global_governance_legitimacy).

% DUAL FORMULATION NOTE:
% The P5 veto power is a contested kernel with multiple readings. This 'oligopoly_reading' focuses on its extractive and entrenching function, while 'coordination_reading' and 'sovereignty_reading' offer alternative interpretations. All three are distinct constraints linked by their common origin in Article 27 of the UN Charter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
