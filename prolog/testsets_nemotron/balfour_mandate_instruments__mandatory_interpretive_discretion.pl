% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__mandatory_interpretive_discretion, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: British Mandatory Interpretive Discretion over Balfour Mandate Instruments
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   The British mandatory power, operating under the League of Nations
 *   mandate for Palestine, reserved for itself the exclusive authority to
 *   interpret the Balfour Declaration's 'national home' provision and the
 *   mandate instrument's dual obligations. This interpretive discretion was
 *   not a background condition but the operational constraint system itself:
 *   each White Paper (1922 Churchill, 1930 Passfield, 1939 MacDonald) and
 *   land regime shift (1920 Land Transfer Ordinance, 1940 Land Transfer
 *   Regulations) reconstituted the legal baseline from which both Arab and
 *   Zionist communities had to negotiate. No external review — not the
 *   Permanent Mandates Commission, not the League Council, not any court —
 *   could bind the mandatory power's interpretation. The communities faced
 *   strategic uncertainty: a policy shift did not merely change present
 *   conditions but rewrote the terms of all future claims. British
 *   administrators benefited from this flexibility, using it to manage
 *   imperial interests, balance competing nationalisms, and maintain control
 *   without fixed commitments. Both communities were victims: neither could
 *   appeal to a fixed textual meaning or external arbitration; each policy
 *   oscillation created path-dependent lock-in where the new baseline became
 *   the starting point for the next negotiation.
 *
 * KEY AGENTS:
 *   - british_colonial_administrators: Primary beneficiary (institutional/arbitrage) — holds interpretive authority, extracts policy flexibility
 *   - arab_palestinian_community: Primary victim (organized/constrained) — bears land loss, demographic displacement, political exclusion; no appeal beyond mandatory power
 *   - zionist_yishuv_community: Primary victim (organized/constrained) — bears immigration restrictions, institutional constraints; no appeal beyond mandatory power
 *   - league_of_nations_mandates_commission: Excluded observer (institutional/analytical) — monitors but cannot bind mandatory interpretation
 *   - british_imperial_government: Agenda setter (institutional/arbitrage) — ultimate principal, sets strategic parameters for mandatory administration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.62).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.78).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "British Mandatory Interpretive Discretion over Balfour Mandate Instruments").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, 'a143b9c0-a7b8-4d03-afba-e3c9c9ae6216').
narrative_ontology:cs_kernel_codification('a143b9c0-a7b8-4d03-afba-e3c9c9ae6216', formalized).
narrative_ontology:cs_authority_grounding('a143b9c0-a7b8-4d03-afba-e3c9c9ae6216', extraction).
narrative_ontology:cs_interpretation_layer_present('a143b9c0-a7b8-4d03-afba-e3c9c9ae6216').
narrative_ontology:cs_reading_relation('a143b9c0-a7b8-4d03-afba-e3c9c9ae6216', balfour_mandate_instruments__jewish_national_home_primacy, influences).
narrative_ontology:cs_reading_relation('a143b9c0-a7b8-4d03-afba-e3c9c9ae6216', balfour_mandate_instruments__dual_obligation_indigenous_rights, influences).
narrative_ontology:cs_axiom('a143b9c0-a7b8-4d03-afba-e3c9c9ae6216', foundational, mandatory_power_final_arbiter).
narrative_ontology:cs_axiom_status(mandatory_power_final_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('a143b9c0-a7b8-4d03-afba-e3c9c9ae6216', mandatory_power_final_arbiter, conventional).
narrative_ontology:cs_axiom('a143b9c0-a7b8-4d03-afba-e3c9c9ae6216', foundational, interpretive_discretion_as_imperial_management).
narrative_ontology:cs_axiom_status(interpretive_discretion_as_imperial_management, holdable).
narrative_ontology:cs_axiom_grounding('a143b9c0-a7b8-4d03-afba-e3c9c9ae6216', interpretive_discretion_as_imperial_management, instrumental).
narrative_ontology:cs_reference_frame('a143b9c0-a7b8-4d03-afba-e3c9c9ae6216', mandate_text_as_authorizing_discretion).
narrative_ontology:cs_drift_state('a143b9c0-a7b8-4d03-afba-e3c9c9ae6216', id_1939_white_paper, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a143b9c0-a7b8-4d03-afba-e3c9c9ae6216', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_palestinian_community).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_yishuv_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise interpretive authority over mandate instruments through White Papers, land regulations, and administrative decisions. Collect policy flexibility: can pivot between Zionist and Arab demands as imperial interests shift. Use discretion to manage violence, maintain imperial control, and avoid fixed commitments. Exit is arbitrage-grade: reassignment to other colonial posts, metropolitan promotion, or retirement with pension.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators, beneficiary,
    institutional, biographical, arbitrage, regional).

% Bears land loss (land transfer regulations), demographic displacement (immigration policy), and political exclusion (denial of representative institutions). Each interpretive shift (1922 Churchill White Paper, 1930 Passfield White Paper, 1939 MacDonald White Paper) rewrites the baseline: concessions extracted under one interpretation become the starting point for the next. No appeal beyond the mandatory power; petitions to League of Nations are advisory only. Exit is constrained: no alternative sovereign, diaspora is exit from the territory not the constraint.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_palestinian_community, payer,
    organized, generational, constrained, regional).

% Bears immigration restrictions (1930 Hope Simpson, 1939 White Paper quotas), land purchase constraints (1940 Land Transfer Regulations), and institutional limits (no independent defense, foreign policy). Each interpretive shift rewrites the baseline: institutional gains under one interpretation (Jewish Agency recognition, Hebrew University) become fixed assets the mandatory power can later constrain. No appeal beyond the mandatory power; petitions to League are advisory. Exit is constrained: no alternative territory for national project, aliyah is entry not exit.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_yishuv_community, payer,
    organized, generational, constrained, regional).

% Monitors mandatory administration through annual reports and petitions. Issues critical findings (e.g., on 1930 Passfield White Paper, 1939 White Paper) but has no binding authority. British government treats findings as advisory; mandatory administration implements or ignores at discretion. The Commission's structural position is symmetric (neither beneficiary nor victim) but powerless — its exit is analytical (it can only observe and report).
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, league_of_nations_mandates_commission, observer,
    institutional, generational, analytical, global).

% Sets the strategic parameters for mandatory administration: imperial defense (Suez, air routes), Arab goodwill (oil, regional stability), Zionist utility (wartime alliance, imperial loyalty). Issues instructions to High Commissioner; approves or initiates White Papers. Collects strategic flexibility from the mandatory power's interpretive discretion. Exit is arbitrage-grade: can replace High Commissioner, revise mandate terms, or refer to League (which it controls).
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_imperial_government, agenda_setter,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__mandatory_interpretive_discretion, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the irreducible conflict between two national movements in one territory by giving the administering power discretion to balance, delay, and recalibrate — preventing either side from locking in a fixed legal advantage that would make the other's position untenable.
% TRANSFER_FUNCTION: Moves interpretive authority (the power to define what the mandate means at any moment) from the fixed text and external review to the mandatory administration. Moves the costs of interpretive oscillation (land, demography, institutional recognition, political standing) from the mandatory power to both national communities. Moves strategic flexibility (the ability to pivot policy without legal consequence) from the communities to the mandatory power.
% ABSENT_VOICES: The Palestinian Arab peasantry (fellahin) who bore land dispossession without representation in the Arab Higher Committee; the Jewish refugee communities desperate for entry who had no voice in Zionist executive negotiations; the League of Nations member states (especially smaller powers) who sponsored the mandate system's principles but had no enforcement mechanism. All are structurally excluded from the interpretive process.
% DISAPPEARANCE_RATIONALE: If mandatory interpretive discretion vanished overnight (e.g., binding external adjudication fixed the mandate's meaning), the strategic uncertainty would collapse: both communities would face a fixed legal baseline, the mandatory power would lose its primary instrument of imperial management, and the conflict would shift from managed oscillation to a determinate legal-political confrontation. The 1939 White Paper's immigration limits, the 1940 Land Transfer Regulations, and the entire structure of British mediation would become legally challengeable.
% FOUNDING_PROBLEM: The League of Nations mandate system required preparing mandated territories for self-government while the Balfour Declaration committed Britain to facilitating a Jewish national home in Palestine — two objectives that were structurally contradictory in a territory with an Arab majority. The mandatory power's interpretive discretion was the mechanism for managing this contradiction without resolving it.
% FOUNDING_PROBLEM_CORROBORATION: The Peel Commission (1937, British royal commission) concluded the mandate was unworkable and recommended partition — attesting the founding problem (dual obligation) could not be solved by interpretive discretion. The 1939 MacDonald White Paper effectively abandoned the 'national home' as a proto-state objective, confirming the dual obligation was dead as a governing framework. Zionist leadership (Ben-Gurion, Weizmann) and Arab leadership (Haj Amin al-Husseini, Arab Higher Committee) both rejected the White Paper from opposite directions — corroborating that neither community saw the founding problem as live. The League of Nations Permanent Mandates Commission questioned the White Paper's compatibility with the mandate — external corroboration that the mandate's own supervisory body saw the founding problem as unresolved, not solved.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects the mandatory power's ability to extract compliance and concession from both communities by controlling the interpretive baseline — each community invests in building facts on the ground (demographic, institutional, legal) that the mandatory power can then ratify or reverse by interpretive fiat. Suppression (0.78) is high because the constraint's persistence depends on the mandatory power's monopoly on authoritative interpretation, backed by military-administrative enforcement; alternatives (appeal to League, fixed textual reading, external arbitration) are structurally suppressed. Theater ratio (0.38) is moderate: the mandatory administration performs 'balanced administration' and 'dual obligation' rhetoric, but a growing share of interpretive acts serve imperial management rather than mandate fulfillment. Accessibility collapse (0.52) and resistance (0.58) reflect that both communities developed sophisticated legal-political strategies within the interpretive frame (petitions, commissions, institutional building) but could not escape the frame itself. The measurement series captures the escalation from 1920 (incipient discretion) to 1939 (near-total interpretive closure via the MacDonald White Paper).
 *
 * PERSPECTIVAL GAP:
 *   From the British administrator's seat, the discretion is genuine coordination: managing irreconcilable claims in a territory where no fixed text could satisfy both parties. From the Arab community's seat, the discretion is a snare: each interpretation facilitates Zionist expansion while Arab rights are 'interpreted' into irrelevance. From the Zionist community's seat, the discretion is also a snare but differently: British interpretations restrict immigration and land purchase precisely when Zionist need is greatest. The engine computes this seat divergence from the structural data — the same interpretive act extracts differently from each community.
 *
 * DIRECTIONALITY LOGIC:
 *   British colonial administrators are structural beneficiaries (d ≈ 0.15): they hold the interpretive pen, collect policy flexibility, and face arbitrage-grade exit (imperial reassignment, metropolitan career progression). Arab Palestinian community and Zionist Yishuv community are structural victims (d ≈ 0.85): they bear the costs of interpretive oscillation (land, demography, institutional recognition), have constrained exit (no alternative sovereign, no binding external forum), and their resistance is channeled into the mandatory power's interpretive process. The League of Nations Mandates Commission is an excluded observer (d = 0.5, analytical): it monitors but its findings are advisory only; its structural position is symmetric but powerless. The British imperial government is the agenda setter (d ≈ 0.1): it sets the strategic parameters (imperial defense, Arab goodwill, Zionist utility) within which the mandatory administration exercises discretion.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate's founding problem (administering a territory toward self-government while implementing the Balfour Declaration) was structurally unresolvable by interpretive discretion alone — the dual obligation demanded contradictory outcomes. The mandatory power's interpretive authority did not solve the founding problem; it managed the irresolution for imperial convenience. The arrangement persists not because it coordinates but because the mandatory power extracts strategic value from the irresolution itself. This is mandatrophy: the coordination function (preparing for self-government) atrophied, the extraction function (imperial management of competing nationalisms) persisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_discretion_vs_textual_fixity,
    'Does the mandatory power''s interpretive discretion reflect genuine administrative necessity in a contested territory, or is it a constructed cover for imperial divide-and-rule that could have been constrained by fixed textual commitments?',
    'Counterfactual comparison: analyze whether the League of Nations Permanent Mandates Commission ever successfully constrained British interpretive choices, and whether alternative mandatory powers (e.g., French in Syria) exercised comparable discretion over fixed-text instruments.',
    'If discretion was structurally necessary, the constraint leans toward tangled_rope (coordination function: managing irreconcilable claims). If discretion was strategically maintained for imperial control, it is a pure snare with no coordination justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_discretion_vs_textual_fixity, conceptual, 'Whether the interpretive discretion at the core of this reading is a functional necessity or an extracted privilege.').

omega_variable(
    kernel_reading_mandatory_interpretive_discretion,
    'How does this reading''s structural profile change if the contested kernel (balfour_mandate_instruments) is resolved by external adjudication rather than mandatory discretion?',
    'Trace the structural delta: this reading instantiates a constraint where the mandatory power is the final arbiter. A sibling reading (e.g., dual_obligation_indigenous_rights) would instantiate a constraint where external legal norms bind the mandatory power. Compare ε, beneficiaries, and victims across the two instantiated constraints.',
    'If external adjudication would substantially lower ε and eliminate the British administrators as beneficiaries, this reading''s snare classification is contingent on the absence of binding external review — a structural feature of the mandate system itself, not the Balfour text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_mandatory_interpretive_discretion, conceptual, 'Commitment-system framing: this constraint is one reading of a contested kernel; its structural metrics depend on which reading is instantiated.').

omega_variable(
    suppression_mechanism_mandate_enforcement,
    'Is the suppression experienced by both communities structural (British military/administrative enforcement of policy shifts) or internalized (communities self-censoring claims because the interpretive frame makes appeal seem futile)?',
    'Post-exit suppression trajectory: examine whether Arab and Zionist political movements continued to petition the League of Nations and British public opinion after major policy shifts (1930 Passfield White Paper, 1939 MacDonald White Paper), or whether they accepted the mandatory power''s interpretation as final.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the communities carry the interpretive closure with them into subsequent negotiations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_mandate_enforcement, empirical, 'Structural vs. internalized suppression mechanism in a colonial interpretive system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 1920, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(balf_tr_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1922, 0.22).
narrative_ontology:measurement(balf_tr_t1929, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1929, 0.28).
narrative_ontology:measurement(balf_tr_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1930, 0.32).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1936, 0.35).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1939, 0.38).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement(balf_be_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1922, 0.48).
narrative_ontology:measurement(balf_be_t1929, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1929, 0.52).
narrative_ontology:measurement(balf_be_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1930, 0.58).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1936, 0.62).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1939, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1920, 0.55).
narrative_ontology:measurement(balf_su_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1922, 0.62).
narrative_ontology:measurement(balf_su_t1929, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1929, 0.7).
narrative_ontology:measurement(balf_su_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1930, 0.75).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1936, 0.78).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1939, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.12).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% Kernel family: balfour_mandate_instruments. This reading (mandatory_interpretive_discretion) is the upstream constraint — the mandatory power's interpretive authority is the structural condition that enables the sibling readings to operate as live contestations rather than settled law. The sibling readings are downstream: they are the competing claims that the mandatory power adjudicates between. If this reading's constraint were removed (external adjudication binding), the sibling readings would become mutually exclusive legal positions resolved by external authority, not managed by imperial discretion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__mandatory_interpretive_discretion, institutional, 0.15).
constraint_indexing:directionality_override(balfour_mandate_instruments__mandatory_interpretive_discretion, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
