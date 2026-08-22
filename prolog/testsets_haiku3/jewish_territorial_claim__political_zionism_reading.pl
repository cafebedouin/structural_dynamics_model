% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__political_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__political_zionism_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: jewish_territorial_claim__political_zionism_reading
 *   human_readable: Jewish Territorial Sovereignty as Antisemitism Solution (Political Zionism Reading)
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   Political Zionism represents a specific reading of the Jewish national
 *   question: one in which the solution to antisemitism, the Jewish Question,
 *   and Jewish statelessness is the creation of a territorial state with
 *   Jewish political sovereignty and majority demographic control. This
 *   reading gained institutional power through the World Zionist
 *   Organization, secured imperial backing (Balfour Declaration), and by 1948
 *   had established the state of Israel. The reading is one of four live
 *   competitors within the broader Jewish territorial claim kernel: cultural
 *   Zionism (cultural center without sovereignty), labor Zionism (socialist
 *   transformation over state apparatus), and revisionist Zionism (maximalist
 *   territorial claims). This constraint story instantiates only the
 *   political Zionism reading — the claim that statehood with majority
 *   control solves antisemitism. The authored metrics (high extractiveness
 *   toward the Palestinian population, high suppression requirement, active
 *   enforcement) reflect the structural costs this reading imposes on the
 *   existing Arab population, independent of the reading's own legitimacy
 *   claims.
 *
 * KEY AGENTS:
 *   - jewish_state_proponents: Political Zionist movement leaders, institutions, and organizations that set the state-building agenda.
 *   - jewish_diaspora_political_organizations: World Zionist Organization, diaspora institutions that benefit from the constraint by gaining political leverage and communal authority.
 *   - palestinian_arab_population: Existing inhabitants whose land, authority, and self-determination are treated as obstacles; structurally excluded from the constraint's beneficiary frame.
 *   - existing_arab_residents: Face immediate displacement or minority status; trapped without exit or alternative political voice.
 *   - jewish_refugees_persecuted_communities: Potential beneficiaries of the constraint as a solution, but identity-locked into the state project.
 *   - imperial_mandate_authorities: British mandate holders and international authorities that set enforcement conditions and extract political leverage.
 *   - antisemitic_european_states: Benefit from the constraint as an outlet for their Jewish Question without addressing their own antisemitism.
 *   - cultural_zionist_alternative: Structurally excluded reading — cultural center without sovereignty.
 *   - labor_zionist_alternative: Coexisting reading emphasizing socialist transformation over state apparatus.
 *   - revisionist_zionist_alternative: Coexisting reading with maximalist territorial claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.82).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.76).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Jewish Territorial Sovereignty as Antisemitism Solution (Political Zionism Reading)").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '9f85407d-cd06-436b-8fbc-3751570245b8').
narrative_ontology:cs_kernel_codification('9f85407d-cd06-436b-8fbc-3751570245b8', distributed).
narrative_ontology:cs_authority_grounding('9f85407d-cd06-436b-8fbc-3751570245b8', extraction).
narrative_ontology:cs_interpretation_layer_present('9f85407d-cd06-436b-8fbc-3751570245b8').
narrative_ontology:cs_reading_relation('9f85407d-cd06-436b-8fbc-3751570245b8', jewish_territorial_claim__cultural_zionism_reading, forecloses).
narrative_ontology:cs_reading_relation('9f85407d-cd06-436b-8fbc-3751570245b8', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f85407d-cd06-436b-8fbc-3751570245b8', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('9f85407d-cd06-436b-8fbc-3751570245b8', foundational, jewish_majority_as_essential).
narrative_ontology:cs_axiom_status(jewish_majority_as_essential, holdable).
narrative_ontology:cs_axiom_grounding('9f85407d-cd06-436b-8fbc-3751570245b8', jewish_majority_as_essential, empirically_contingent).
narrative_ontology:cs_axiom('9f85407d-cd06-436b-8fbc-3751570245b8', foundational, territorial_sovereignty_solves_persecution).
narrative_ontology:cs_axiom_status(territorial_sovereignty_solves_persecution, holdable).
narrative_ontology:cs_axiom_grounding('9f85407d-cd06-436b-8fbc-3751570245b8', territorial_sovereignty_solves_persecution, empirically_contingent).
narrative_ontology:cs_reference_frame('9f85407d-cd06-436b-8fbc-3751570245b8', diaspora_vulnerability_and_statelessness).
narrative_ontology:cs_drift_state('9f85407d-cd06-436b-8fbc-3751570245b8', state_apparatus_consolidation_1948, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f85407d-cd06-436b-8fbc-3751570245b8', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_state_proponents).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_diaspora_political_organizations).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, existing_arab_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_refugees_persecuted_communities).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, imperial_mandate_authorities).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, antisemitic_european_states).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, jewish_refugees_persecuted_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Political Zionist movement leaders, institutions, and political organizations that set the territorial sovereignty agenda: defining boundaries, negotiating with imperial powers, directing migration and settlement, excluding or marginalizing alternative cultural-center framings. They collect political authority and territorial control from the constraint's operation.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_state_proponents, agenda_setter,
    organized, generational, mobile, continental).

% World Zionist Organization, Jewish political institutions, and diaspora fundraising networks that benefit from the constraint by gaining a political frame (Jewish statehood as antisemitism solution) that validates their organizational claim to represent diaspora Jewry. They gain political leverage and communal authority from endorsing and supporting the state-building project without bearing the territorial displacement costs.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_diaspora_political_organizations, beneficiary,
    powerful, generational, arbitrage, global).

% Palestinian Arabs already inhabiting the territory: their land, water, political authority, and self-determination are treated structurally as obstacles to the constraint's operation. The reading's core logic requires their displacement, diminishment to minority status, or exile to make room for Jewish majority — the constraint's persistence depends on suppressing their counterclaim to the same territory.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, regional).

% Arabs living in the designated territory who face immediate displacement through legal, economic, military, or administrative mechanisms justified by the majority requirement. They have no structural role in the constraint's operation except as the material obstacle that must be removed.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, existing_arab_residents, payer,
    powerless, biographical, trapped, local).

% Jewish communities fleeing persecution and pogroms who benefit from the constraint as a solution frame (safe haven, political autonomy, asylum) but simultaneously pay the cost through identity fusion with the state project and potential complicity in displacement mechanisms. Their exit from the constraint is identity-locked — rejecting the state framework risks loss of communal belonging and solidarity claims.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_refugees_persecuted_communities, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, jewish_refugees_persecuted_communities, payer).

% British and later international authorities that hold the mandate and territorial sovereignty: they set enforcement conditions, grant or deny legitimacy to the state-building project, and extract political leverage by managing the territorial question. They benefit from the constraint by positioning themselves as the legitimating authority while deferring the displacement costs onto local populations.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, imperial_mandate_authorities, agenda_setter,
    institutional, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, imperial_mandate_authorities, beneficiary).

% European powers and antisemitic regimes benefit from the constraint as a solution to their 'Jewish Question' — an outlet for Jewish migration and political organization that relieves internal pressure without requiring them to address their own antisemitic structures. They endorse the constraint without bearing displacement costs.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, antisemitic_european_states, beneficiary,
    powerful, biographical, mobile, continental).

% The alternative framing that Jewish national expression could occur through cultural and spiritual institutions without requiring territorial sovereignty or demographic majority. This reading is structurally excluded from the political Zionist constraint's operation — the reading's core claim forecloses it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, cultural_zionist_alternative, excluded,
    moderate, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(jewish_territorial_claim__political_zionism_reading, cultural_zionist_alternative).

% The labor Zionist reading that prioritizes socialist transformation and 'conquest of labor' over state apparatus and demographic majority. This reading coexists with political Zionism but emphasizes different mechanisms and social structures.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, labor_zionist_alternative, excluded,
    moderate, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(jewish_territorial_claim__political_zionism_reading, labor_zionist_alternative).

% The maximalist reading claiming both banks of Jordan and immediate military sovereignty. This reading influences political Zionism by raising territorial expectations and legitimacy standards but is not foreclosed by it — both can coexist as live positions in contemporary debate.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, revisionist_zionist_alternative, excluded,
    moderate, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(jewish_territorial_claim__political_zionism_reading, revisionist_zionist_alternative).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__political_zionism_reading, jewish_state_proponents).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__political_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Proposed solution to the antisemitism and Jewish Question: creation of a territorial Jewish state where Jews would constitute the majority population and hold political sovereignty, enabling self-determination, safe asylum from persecution, and elimination of diaspora political dependence.
% TRANSFER_FUNCTION: Transfers territorial control, political authority, and land/resource access from the existing Arab population to the new Jewish state and its institutions, justified by the majority requirement. Transfers political legitimacy and communal authority from diaspora organizations to the state apparatus. Transfers the 'Jewish Question' from European states to the territorial solution, allowing antisemitic regimes to externalize the problem.
% ABSENT_VOICES: Palestinian Arab national movements and the indigenous Arab population whose territorial claims and self-determination rights are structurally excluded from the constraint's framing. They would assert counter-claims to the same territory and reject the premise that their displacement is necessary or acceptable. Their absence from the founding consensus is structural, not accidental.
% DISAPPEARANCE_RATIONALE: If this constraint (state-building with Jewish majority as solution to antisemitism) disappeared, the territorial configuration, political authority structure, and the entire subsequent century of regional conflict and displacement would reorganize fundamentally. The constraint is constitutive of the political reality it created.
% FOUNDING_PROBLEM: Antisemitism, pogroms, and persecution across Europe and the Middle East; the 'Jewish Question' — the demand from European states for a solution to Jewish presence and political organization within their territories; Jewish statelessness and vulnerability to displacement.
% FOUNDING_PROBLEM_CORROBORATION: Political Zionist institutions and the Israeli state attest the founding problem was and remains live, citing ongoing antisemitism and the need for Jewish sovereignty as its solution. Palestinian historians and Arab analysts attest the founding problem is a European-generated question that externalized antisemitism rather than addressing it, and that the territorial solution imposed a new dispossession rather than solving the original persecution. Independent historians and human rights organizations document that antisemitism persisted after state formation and that the constraint created new victims. The 'problem' is corroborated as real by antisemitic violence; the claim that territorial statehood solves it is contested by those pointing to displacement outcomes.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__political_zionism_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__political_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__political_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the constraint requires the removal or subordination of the existing Arab population to achieve Jewish majority — the transfer of territory and political authority is involuntary and coercive for the payer seats. Suppression is also high (0.76) because the constraint's persistence depends on suppressing the Palestinian Arab counterclaim to the same territory and preventing their political organization as a majority. Theater ratio is moderate-low (0.28) because the security and self-determination justifications are genuine coordination functions, but an increasing share of enforcement activity defends the displacement mechanism itself rather than the stated goal. Accessibility collapse is high (0.71) because the constraint structurally forecloses alternatives for the Arab population — no non-displacement path to political voice is available within the reading's frame. The measurement series show extraction, suppression, and stakes accelerating from 1880 to 1948 as the movement shifted from ideological framework to institutional power to state apparatus. The coercion grid demonstrates that suppression and stakes inflation rise most sharply at the individual and class levels for Palestinian Arabs, while accessibility collapse is highest at the structural level — the constraint forecloses alternatives at the system level and inflates costs at the lived level.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (Jewish state proponents, diaspora organizations, imperial authorities) experience this constraint as coordination — creation of a solution to persecution and statelessness. The payer seats (Palestinian Arabs) experience the same constraint as pure extraction — loss of territory, political authority, and self-determination without consent. The engine should compute these as divergent types: Rope or Tangled Rope from the beneficiary perspective (coordination function plus enforcement), Snare from the payer perspective (extraction masked as necessity). The divergence is not a measurement error — it is the structural asymmetry the framework exists to capture. The reading's own internal logic treats the Arab population as an obstacle rather than as a negotiating party, which is itself the evidence for the perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   jewish_state_proponents and jewish_diaspora_political_organizations derive directionality near 0.0 (beneficiaries): they gain political authority, territorial control, and communal legitimacy from the constraint. palestinian_arab_population and existing_arab_residents derive directionality near 1.0 (targets): they lose territory, authority, and political voice. jewish_refugees_persecuted_communities sit near 0.2-0.3 (mostly beneficiary, but identity-locked): they gain safety and political community but at the cost of complicity in displacement. imperial_mandate_authorities sit near 0.35-0.45 (near symmetric): they enforce the constraint and extract political leverage, but also bear administrative and military costs. The directionality is derived from beneficiary/victim declarations and exit options: payer seats have trapped or identity_locked exit, powerless power, and are structurally excluded from authority — these stack toward target status. Beneficiary seats have mobile exit, powerful or organized power, and set the agenda — these stack toward beneficiary status.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (antisemitism, persecution, the Jewish Question) is declared as 'contested' status, not 'dead.' The constraint's operation does not resolve the founding problem — antisemitism persists post-1948, and anti-Zionism/anti-Israel sentiment emerges as a new form of conflict. However, the constraint persists because the state apparatus is now self-reinforcing: it collects territory, military power, international recognition, and diaspora support. The mandate for the state itself has shifted from 'solution to antisemitism' to 'Jewish national self-determination and right to exist.' The original problem (antisemitism) remains open while the constraint's institutional machinery (the state, its security apparatus, settlement expansion) continues to grow. This is not classic mandatrophy — the constraint has not yet fully degraded into theater. But it is a trajectory toward mandatrophy: if antisemitism remains unresolved and the constraint shifts entirely to defending the state's existence against Palestinian resistance (rather than providing refuge from persecution), the gap between founding mandate and actual operation widens. The measurement series show theater_ratio rising but not yet dominant (0.28 at endpoint), suggesting the constraint still performs its stated functions (asylum, self-determination) even as it has become primarily an enforcer of territorial control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jewish_majority_requirement_necessity,
    'Is Jewish majority demographic control structurally necessary for a Jewish political solution to antisemitism, or is it a contingent strategic choice that could be substituted with other arrangements (cultural autonomy, federation, binational state)?',
    'Comparative analysis of non-majoritarian Jewish political frameworks (diaspora communities with substantial self-governance, multinational models); historical examination of whether the majority constraint was driven by security logic or by ideological exclusion; testimony from alternative Zionist readings about viability of non-majority scenarios.',
    'If majority is contingent, the constraint''s extraction is revealed as ideologically driven rather than functionally necessary — reclassifies from coordination-plus-extraction to pure extraction. If necessary, part of the measured extractiveness is the unavoidable cost of the coordination function itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jewish_majority_requirement_necessity, conceptual, 'Whether Jewish majority is structurally necessary for the constraint or a contingent ideological choice.').

omega_variable(
    arab_population_as_obstacle_vs_constituent,
    'In the political Zionist reading''s framing, are Palestinian Arabs treated as an obstacle to removal, or as a constituent population whose political rights are negotiable within the state framework?',
    'Analysis of Political Zionist movement documents, leadership statements, and institutional policies regarding Arab residents: do they envision transfer, expulsion, subordination, or some form of political inclusion? Comparison with statements by other readings about the same question.',
    'If Arabs are framed as obstacles to be removed, the constraint is structurally premised on displacement and is purely extractive toward that population. If framed as constituents with negotiable rights, the extraction is asymmetric but not necessarily totalizing. This directly affects the classification of the payer seat''s situation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arab_population_as_obstacle_vs_constituent, empirical, 'Whether Palestinian Arabs are treated as removable obstacles or as constituents with political rights.').

omega_variable(
    antisemitism_solution_mechanism,
    'Does territorial Jewish statehood actually solve antisemitism, or does it displace the antisemitic question to a new geographic and political context where the ''Jewish state'' becomes a target and antisemitism mutates into anti-Zionism and anti-Israel sentiment?',
    'Empirical measurement of antisemitic violence and prejudice before and after state formation; analysis of whether anti-Zionism functions as displaced antisemitism or as distinct political opposition; testimony from diaspora Jewish communities about changes in their security and political standing post-1948.',
    'If state formation fails to reduce antisemitism globally and generates new conflict-driven antisemitism, the constraint''s founding problem remains unresolved and the extraction persists without delivering on its justification. This reclassifies the beneficiary structure: who actually benefits if the stated goal is not achieved?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(antisemitism_solution_mechanism, empirical, 'Whether the constraint achieves its stated founding goal of solving antisemitism.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is this reading''s authoritative kernel the political Zionist movement''s own texts and declarations, or is it the underlying contested claim about Jewish national expression that multiple readings compete to interpret?',
    'Clarification of which framing is ''the kernel'': (a) the political Zionist commitment itself (in which case alternative readings are distinct commitments, not readings of the same kernel), or (b) the broader Jewish national question to which political Zionism is one answer. This determines whether siblings should be modeled as readings of the same kernel or as competing foundational claims.',
    'If (a), the reading_relations should emphasize foreclosure (political Zionism categorically rules out cultural-only solutions). If (b), the reading_relations should emphasize coexistence (different readings of the same kernel can be held by different parties). This affects the CS structure''s axiom classification and how the engine models constraint family dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Ambiguity about what constitutes ''the kernel'' in the Jewish national question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 1880, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1880, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(jewi_tr_t1900, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(jewi_tr_t1920, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1920, 0.16).
narrative_ontology:measurement(jewi_tr_t1930, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1930, 0.21).
narrative_ontology:measurement(jewi_tr_t1940, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1940, 0.26).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1948, 0.28).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1880, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(jewi_be_t1900, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1900, 0.32).
narrative_ontology:measurement(jewi_be_t1920, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1920, 0.48).
narrative_ontology:measurement(jewi_be_t1930, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1930, 0.62).
narrative_ontology:measurement(jewi_be_t1940, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1940, 0.74).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1948, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1880, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1880, 0.22).
narrative_ontology:measurement(jewi_su_t1900, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1900, 0.38).
narrative_ontology:measurement(jewi_su_t1920, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1920, 0.51).
narrative_ontology:measurement(jewi_su_t1930, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1930, 0.62).
narrative_ontology:measurement(jewi_su_t1940, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1940, 0.71).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1948, 0.76).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1880, tn=1948
narrative_ontology:measurement(jewi_grid_01, jewish_territorial_claim__political_zionism_reading, accessibility_collapse(class), 1880, 0.18).
narrative_ontology:measurement(jewi_grid_02, jewish_territorial_claim__political_zionism_reading, accessibility_collapse(class), 1948, 0.68).
narrative_ontology:measurement(jewi_grid_03, jewish_territorial_claim__political_zionism_reading, accessibility_collapse(individual), 1880, 0.12).
narrative_ontology:measurement(jewi_grid_04, jewish_territorial_claim__political_zionism_reading, accessibility_collapse(individual), 1948, 0.62).
narrative_ontology:measurement(jewi_grid_05, jewish_territorial_claim__political_zionism_reading, accessibility_collapse(organizational), 1880, 0.28).
narrative_ontology:measurement(jewi_grid_06, jewish_territorial_claim__political_zionism_reading, accessibility_collapse(organizational), 1948, 0.71).
narrative_ontology:measurement(jewi_grid_07, jewish_territorial_claim__political_zionism_reading, accessibility_collapse(structural), 1880, 0.35).
narrative_ontology:measurement(jewi_grid_08, jewish_territorial_claim__political_zionism_reading, accessibility_collapse(structural), 1948, 0.78).
narrative_ontology:measurement(jewi_grid_09, jewish_territorial_claim__political_zionism_reading, resistance(class), 1880, 0.22).
narrative_ontology:measurement(jewi_grid_10, jewish_territorial_claim__political_zionism_reading, resistance(class), 1948, 0.71).
narrative_ontology:measurement(jewi_grid_11, jewish_territorial_claim__political_zionism_reading, resistance(individual), 1880, 0.18).
narrative_ontology:measurement(jewi_grid_12, jewish_territorial_claim__political_zionism_reading, resistance(individual), 1948, 0.68).
narrative_ontology:measurement(jewi_grid_13, jewish_territorial_claim__political_zionism_reading, resistance(organizational), 1880, 0.28).
narrative_ontology:measurement(jewi_grid_14, jewish_territorial_claim__political_zionism_reading, resistance(organizational), 1948, 0.62).
narrative_ontology:measurement(jewi_grid_15, jewish_territorial_claim__political_zionism_reading, resistance(structural), 1880, 0.32).
narrative_ontology:measurement(jewi_grid_16, jewish_territorial_claim__political_zionism_reading, resistance(structural), 1948, 0.58).
narrative_ontology:measurement(jewi_grid_17, jewish_territorial_claim__political_zionism_reading, stakes_inflation(class), 1880, 0.31).
narrative_ontology:measurement(jewi_grid_18, jewish_territorial_claim__political_zionism_reading, stakes_inflation(class), 1948, 0.72).
narrative_ontology:measurement(jewi_grid_19, jewish_territorial_claim__political_zionism_reading, stakes_inflation(individual), 1880, 0.42).
narrative_ontology:measurement(jewi_grid_20, jewish_territorial_claim__political_zionism_reading, stakes_inflation(individual), 1948, 0.81).
narrative_ontology:measurement(jewi_grid_21, jewish_territorial_claim__political_zionism_reading, stakes_inflation(organizational), 1880, 0.24).
narrative_ontology:measurement(jewi_grid_22, jewish_territorial_claim__political_zionism_reading, stakes_inflation(organizational), 1948, 0.68).
narrative_ontology:measurement(jewi_grid_23, jewish_territorial_claim__political_zionism_reading, stakes_inflation(structural), 1880, 0.18).
narrative_ontology:measurement(jewi_grid_24, jewish_territorial_claim__political_zionism_reading, stakes_inflation(structural), 1948, 0.74).
narrative_ontology:measurement(jewi_grid_25, jewish_territorial_claim__political_zionism_reading, suppression(class), 1880, 0.28).
narrative_ontology:measurement(jewi_grid_26, jewish_territorial_claim__political_zionism_reading, suppression(class), 1948, 0.78).
narrative_ontology:measurement(jewi_grid_27, jewish_territorial_claim__political_zionism_reading, suppression(individual), 1880, 0.35).
narrative_ontology:measurement(jewi_grid_28, jewish_territorial_claim__political_zionism_reading, suppression(individual), 1948, 0.74).
narrative_ontology:measurement(jewi_grid_29, jewish_territorial_claim__political_zionism_reading, suppression(organizational), 1880, 0.22).
narrative_ontology:measurement(jewi_grid_30, jewish_territorial_claim__political_zionism_reading, suppression(organizational), 1948, 0.76).
narrative_ontology:measurement(jewi_grid_31, jewish_territorial_claim__political_zionism_reading, suppression(structural), 1880, 0.15).
narrative_ontology:measurement(jewi_grid_32, jewish_territorial_claim__political_zionism_reading, suppression(structural), 1948, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__political_zionism_reading, 0.18).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, palestinian_national_movement__arab_nationalism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, balfour_declaration__imperial_mandate_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the jewish_territorial_claim kernel. The family decomposes the overloaded natural-language term 'Zionism' into structurally distinct claims: cultural (institution without state), labor (socialism over apparatus), political (majority state solves persecution), and revisionist (maximal territory with military dominance). Each reading instantiates a different constraint with distinct ε, beneficiary/victim structure, and founding justification. Political Zionism influences and partially forecloses the cultural reading (majority requirement subsumes cultural expression within state logic) but coexists with labor and revisionist variants as live positions within the movement's history. The stories are linked via network.affects_constraints to show theoretical dependence and historical sequence: labor and revisionist variants inherit the political reading's state-building frame but diverge on implementation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__political_zionism_reading, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
