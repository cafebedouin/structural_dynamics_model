% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: British Mandatory Interpretive Discretion over Palestine Mandate Instruments
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   The British Mandate for Palestine (1920-1948) created a constraint system
 *   where the mandatory power held exclusive authority to interpret the
 *   Mandate's contradictory obligations — facilitating a 'Jewish national
 *   home' while protecting 'civil and religious rights of existing non-Jewish
 *   communities' and preparing for self-government. Article 27 of the Mandate
 *   vested disputes 'relating to the interpretation or application' of the
 *   Mandate in the mandatory power's discretion, with the Permanent Court of
 *   International Justice available only by mutual consent (never invoked).
 *   British administrators issued a sequence of policy instruments (1922
 *   Churchill White Paper, 1930 Passfield White Paper, 1939 MacDonald White
 *   Paper, 1940 Land Transfer Regulations) that oscillated between Zionist
 *   and Arab expectations, each shift altering the baseline for subsequent
 *   claims. Neither community could appeal to a fixed textual meaning or
 *   external arbitration; both faced strategic uncertainty and path-dependent
 *   lock-in. The discretionary authority itself — not the Mandate text —
 *   operated as the binding constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.62).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.78).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "British Mandatory Interpretive Discretion over Palestine Mandate Instruments").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, '642e5afb-cb31-4289-b41e-d0b75bb71068').
narrative_ontology:cs_kernel_codification('642e5afb-cb31-4289-b41e-d0b75bb71068', formalized).
narrative_ontology:cs_authority_grounding('642e5afb-cb31-4289-b41e-d0b75bb71068', extraction).
narrative_ontology:cs_interpretation_layer_present('642e5afb-cb31-4289-b41e-d0b75bb71068').
narrative_ontology:cs_reading_relation('642e5afb-cb31-4289-b41e-d0b75bb71068', balfour_mandate_instruments__jewish_national_home_primacy, influences).
narrative_ontology:cs_reading_relation('642e5afb-cb31-4289-b41e-d0b75bb71068', balfour_mandate_instruments__dual_obligation_indigenous_rights, influences).
narrative_ontology:cs_axiom('642e5afb-cb31-4289-b41e-d0b75bb71068', foundational, mandatory_power_sole_adjudicator).
narrative_ontology:cs_axiom_status(mandatory_power_sole_adjudicator, holdable).
narrative_ontology:cs_axiom_grounding('642e5afb-cb31-4289-b41e-d0b75bb71068', mandatory_power_sole_adjudicator, conventional).
narrative_ontology:cs_axiom('642e5afb-cb31-4289-b41e-d0b75bb71068', foundational, textual_ambiguity_as_governance_tool).
narrative_ontology:cs_axiom_status(textual_ambiguity_as_governance_tool, holdable).
narrative_ontology:cs_axiom_grounding('642e5afb-cb31-4289-b41e-d0b75bb71068', textual_ambiguity_as_governance_tool, instrumental).
narrative_ontology:cs_reference_frame('642e5afb-cb31-4289-b41e-d0b75bb71068', mandate_text_as_delegated_authority).
narrative_ontology:cs_drift_state('642e5afb-cb31-4289-b41e-d0b75bb71068', post_1939_white_paper, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('642e5afb-cb31-4289-b41e-d0b75bb71068', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_community).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_community).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__mandatory_interpretive_discretion, mandatory_power_full_legislative_authority).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__mandatory_interpretive_discretion, textual_ambiguity_enables_governance_flexibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the Mandate's administrative apparatus — High Commissioner, legal advisors, district officers. They interpret Mandate articles (2, 4, 6, 11, 13, 15, 22) to issue ordinances, White Papers, land regulations, and immigration quotas without binding external review. The League of Nations Permanent Mandates Commission receives reports but cannot veto. They gain policy flexibility to manage contradictory commitments (Balfour Declaration vs. Article 22 self-determination) and maintain strategic control over Suez approaches. Exit is arbitrage: they rotate posts, return to Whitehall, or transfer to other colonies.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators, beneficiary).

% Muslim and Christian Palestinian notables, fellahin, urban merchants, and emerging nationalist leadership. They face land transfers to Jewish agencies (1920 Land Transfer Ordinance, 1940 Land Transfer Regulations), immigration-driven demographic change, and suppression of political institutions (1936-39 revolt crushed). They petition the High Commissioner, the League, and British parliament — all answered by the same discretionary authority. Exit is constrained: emigration means abandoning ancestral land; armed resistance triggers disproportionate force; diplomatic appeals route through the mandatory power itself.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_community, payer,
    organized, generational, constrained, regional).

% Jewish Agency, Histadrut, Haganah, Revisionist and Labor factions. They depend on British immigration certificates (1922 White Paper quotas, 1939 White Paper caps), land purchase legality, and institutional recognition (Article 4 'Jewish Agency'). Policy oscillates: 1922 Churchill White Paper restricts but validates; 1930 Passfield White Paper threatens restriction; 1939 White Paper severely limits immigration and land purchase. Each shift alters the baseline for negotiation. Exit is constrained: illegal immigration (Aliyah Bet) risks British interception; armed revolt (1944-48) triggers crackdown; diplomatic appeals route through the mandatory power.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_community, payer,
    organized, generational, constrained, regional).

% Permanent Mandates Commission in Geneva receives annual British reports, hears petitioners, issues observations. Its findings are advisory only; British government replies without obligation to comply. The Commission's 1930 and 1939 critiques of British policy (Passfield and 1939 White Papers) were acknowledged but not binding. It cannot compel interpretation, impose remedies, or refer disputes to the Permanent Court of International Justice over British objection. Its exclusion is structural: the Mandate text (Article 27) gives Britain sole discretion over disputes 'relating to the interpretation or application' of the Mandate.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, league_of_nations_mandates_commission, excluded,
    institutional, biographical, trapped, global).

% Jurists (Quincy Wright, Hans Kelsen, Mandate law commentators) analyze whether British interpretive practice conforms to Mandate text, League Covenant Article 22, and emerging self-determination norms. They produce doctrinal arguments but hold no enforcement lever. Their analyses inform later UN debates (1947 UNSCOP) but did not constrain British discretion during the Mandate period.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Mandate system presents itself as administering a territory under international supervision, balancing the 'Jewish national home' commitment with 'civil and religious rights of existing non-Jewish communities' and 'self-government' development. British discretionary authority is framed as the mechanism for reconciling these obligations in practice.
% TRANSFER_FUNCTION: Moves interpretive authority over Mandate obligations from fixed textual commitments to British administrative discretion. The Arab community loses predictable land tenure and demographic trajectory; the Zionist community loses predictable immigration and institutional development rights; both lose recourse to external arbitration. British administrators gain policy flexibility to manage strategic interests (Suez, imperial communications, Arab goodwill) without legal constraint.
% ABSENT_VOICES: Palestinian fellahin (peasantry) facing land dispossession without legal standing; Jewish refugees from Europe (1933-39) blocked by immigration quotas with no appeal; Arab and Jewish moderates advocating binational or partition solutions excluded by British refusal to entertain alternatives to discretionary rule; League of Nations Permanent Mandates Commission structurally excluded from binding review by Mandate Article 27.
% DISAPPEARANCE_RATIONALE: If British discretionary authority vanished overnight, the Mandate's legal framework would collapse into three competing claim-structures with no adjudicator: Zionist claim to statehood via Mandate Article 2/4/6; Arab claim to independence via Article 22/Covenant; British claim to strategic retention via imperial interest. The 1947 UN partition process and 1948 war demonstrate the rearrangement: no fixed textual meaning survived to govern the transition.
% FOUNDING_PROBLEM: Administering a territory with contradictory international commitments — the 1917 Balfour Declaration promising a 'Jewish national home,' the 1919 Paris Peace Conference/Covenant Article 22 mandating 'well-being and development' of inhabitants toward self-government, and British strategic imperatives (Suez Canal, imperial communications, Arab alliance system) — while maintaining British control over the adjudication of these contradictions.
% FOUNDING_PROBLEM_CORROBORATION: League of Nations Permanent Mandates Commission annual reports (1921-1939) document British interpretive latitude and Commission's inability to bind it. British Cabinet minutes (1922 Churchill White Paper deliberations, 1939 White Paper decision) record strategic imperatives overriding textual commitments. Palestinian Arab Executive petitions (1921-1936) and Zionist Executive memoranda (1920-1939) both attest to the absence of fixed interpretive standards. The 1947 UNSCOP majority report concludes the Mandate's contradictory obligations made discretionary governance inevitable.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness (0.62) is moderate: British administrators extracted strategic control and policy flexibility, but also incurred real costs (military, administrative, diplomatic). The coordination function (Mandate administration) is real but subordinate to extraction. Suppression (0.78) is high: the constraint's persistence depended on actively suppressing external review (Article 27), crushing the 1936-39 revolt, and preventing either community from establishing fixed legal rights. Theater ratio (0.42) reflects genuine administrative activity (courts, public works, census) alongside performative legal processes (Commission hearings, White Paper consultations) that did not constrain discretion. Accessibility collapse (0.72) is high for both communities: no external forum could override British interpretation. Resistance (0.65) is substantial from both communities (petitions, strikes, revolts, diplomacy) but structurally ineffective due to the discretionary architecture.
 *
 * PERSPECTIVAL GAP:
 *   From the British administrator seat, the discretionary system appears as necessary governance of contradictory commitments — a genuine coordination function (administering the territory) with unavoidable interpretive latitude. From the Arab and Zionist community seats, the same structure operates as enforced extraction: British discretion systematically prevents either community from securing fixed rights, and each policy shift extracts concessions from one community to manage the other. The engine computes this divergence from the structural data; the claimed type (snare) reflects the payer seats' structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   British colonial administrators are the structural beneficiaries (d ≈ 0.15): they collect policy flexibility, strategic control, and divide-and-rule leverage; their exit is arbitrage (rotation, promotion). Arab community and Zionist community are structural payers (d ≈ 0.85): both bear costs of unpredictable policy, land loss or immigration caps, and suppressed political development; their exit is constrained (emigration abandons claims; resistance triggers force; appeals route through the mandatory power). League of Nations Commission is excluded (d ≈ 0.9): it bears the cost of legitimacy performance without influence; its exit is trapped (Mandate system dissolves without it but it cannot change the structure). International legal scholars are observers (d ≈ 0.5): analytical access without material stakes.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mandate's founding problem (reconciling Balfour Declaration with self-determination under British strategic control) was dead by 1947 — the UNSCOP process acknowledged the obligations were irreconcilable. Yet the discretionary governance model persisted until British withdrawal, and its logic (exclusive interpretive authority over contested text) migrated into successor structures: Israeli military administration in 1967 territories (exclusive legal interpretation), Palestinian Authority governance (discretionary rule without fixed constitution). The constraint did not dissolve with its founding problem; it mutated. The snare classification captures this: the coordination cover (Mandate administration) persisted after the function (preparing for self-government) was abandoned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the Mandate text (Articles 2, 4, 6, 11, 13, 15, 22, 27) structurally delegate interpretive discretion to the mandatory power, or does it contain determinate obligations that British practice violated?',
    'Comparative analysis of Mandate drafting history (San Remo 1920, League Council 1922) versus British interpretive practice (White Papers, ordinances, League Commission exchanges). If drafting records show deliberate ambiguity to secure British acceptance, the delegation reading is strengthened; if they show specific commitments understandably breached, the violation reading is strengthened.',
    'If delegation: this reading''s snare classification reflects a designed-in extraction mechanism (discretion as governance tool). If violation: the snare emerged from British practice exceeding textual authority — the constraint is a snare layered on a broken rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the kernel''s ambiguity is a structural delegation of discretion or a violated determinate obligation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.78) primarily structural (British military/administrative/legal monopoly) or partially internalized (communities adapting to discretionary rule, internalizing the absence of fixed rights)?',
    'Post-1948 trajectory analysis: if Palestinian and Israeli legal/political cultures reproduce discretionary governance patterns (emergency regulations, executive discretion, weak judicial review) independent of British presence, internalized component is significant. Comparative study of mandate vs. post-mandate institutional continuity.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — the communities carry the discretionary logic with them after British exit, explaining successor-structure mutation. If purely structural, the snare should have dissolved with British withdrawal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in a colonial discretionary system.').

omega_variable(
    strategic_vs_pragmatic_discretion,
    'Was British interpretive oscillation (1922/1930/1939 White Papers) primarily a divide-and-rule strategy (extracting compliance from both communities) or pragmatic crisis management (responding to revolt, war, refugee pressure)?',
    'British Cabinet and Colonial Office minutes (released under 30/50-year rules) analyzed for decision rationale: strategic calculations (Suez, Arab alliance, imperial communications) vs. administrative necessity (order, budget, international opinion). Zionist and Arab leadership archives for their readings of British intent.',
    'If strategic divide-and-rule: the snare''s extraction is intentional design — discretion as tool of control. If pragmatic crisis management: the snare emerged from structural contradictions the British could not resolve — extraction as byproduct of incoherent mandate. The former supports ''extraction'' authority_grounding; the latter suggests ''practice'' grounding with extraction as drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_vs_pragmatic_discretion, conceptual, 'Whether British discretionary oscillation was intentional extraction strategy or structural crisis response.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balfour_mandate_discretion_tr_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1920, 0.25).
narrative_ontology:measurement(balfour_mandate_discretion_tr_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1922, 0.3).
narrative_ontology:measurement(balfour_mandate_discretion_tr_t1929, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1929, 0.35).
narrative_ontology:measurement(balfour_mandate_discretion_tr_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1930, 0.38).
narrative_ontology:measurement(balfour_mandate_discretion_tr_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1936, 0.45).
narrative_ontology:measurement(balfour_mandate_discretion_tr_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1939, 0.5).
narrative_ontology:measurement(balfour_mandate_discretion_tr_t1945, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1945, 0.48).
narrative_ontology:measurement(balfour_mandate_discretion_tr_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1948, 0.42).

% Extraction over time
narrative_ontology:measurement(balfour_mandate_discretion_be_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement(balfour_mandate_discretion_be_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1922, 0.5).
narrative_ontology:measurement(balfour_mandate_discretion_be_t1929, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1929, 0.55).
narrative_ontology:measurement(balfour_mandate_discretion_be_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1930, 0.58).
narrative_ontology:measurement(balfour_mandate_discretion_be_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1936, 0.65).
narrative_ontology:measurement(balfour_mandate_discretion_be_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1939, 0.7).
narrative_ontology:measurement(balfour_mandate_discretion_be_t1945, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1945, 0.68).
narrative_ontology:measurement(balfour_mandate_discretion_be_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1948, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(balfour_mandate_discretion_su_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1920, 0.55).
narrative_ontology:measurement(balfour_mandate_discretion_su_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1922, 0.6).
narrative_ontology:measurement(balfour_mandate_discretion_su_t1929, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1929, 0.7).
narrative_ontology:measurement(balfour_mandate_discretion_su_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1930, 0.72).
narrative_ontology:measurement(balfour_mandate_discretion_su_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1936, 0.85).
narrative_ontology:measurement(balfour_mandate_discretion_su_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1939, 0.88).
narrative_ontology:measurement(balfour_mandate_discretion_su_t1945, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1945, 0.82).
narrative_ontology:measurement(balfour_mandate_discretion_su_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1948, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.12).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% This constraint family (balfour_mandate_instruments) decomposes the Mandate text into three structurally distinct readings with divergent ε values. mandatory_interpretive_discretion (ε≈0.62, snare) treats discretion as the operational system. jewish_national_home_primacy (ε≈0.75, snare/tangled_rope) treats the text as directing Jewish state-formation. dual_obligation_indigenous_rights (ε≈0.45, tangled_rope) treats the text as protecting Arab rights. The discretionary reading structurally influences both siblings: British practice sometimes advanced Zionist aims (1920-1930), sometimes Arab aims (1939-1944), creating downstream pressure on both claim-structures without foreclosing either.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__mandatory_interpretive_discretion, institutional, 0.15).
constraint_indexing:directionality_override(balfour_mandate_instruments__mandatory_interpretive_discretion, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
