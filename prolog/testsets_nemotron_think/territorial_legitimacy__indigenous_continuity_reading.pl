% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy via Continuous Indigenous Habitation and Anti-Colonial Self-Determination (1948 as Nakba)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the indigenous_continuity_reading of
 *   the territorial_legitimacy kernel. The reading asserts that territorial
 *   legitimacy derives exclusively from continuous indigenous habitation and
 *   anti-colonial self-determination, framing 1948 as Nakba
 *   (catastrophe/dispossession) rather than partition. It claims Palestinian
 *   sovereignty over all historic Palestine, declares the Israeli state
 *   illegitimate as a settler-colonial entity, and positions the right of
 *   return for 1948 refugees as structurally central — non-derogable and
 *   non-negotiable. The reading presents itself as a Mountain (natural law of
 *   justice), but declares identifiable beneficiaries (Palestinian people,
 *   1948 refugees) and victims (Israeli Jews, Israeli state institutions),
 *   triggering false summit mountain evaluation. The standing arrangement
 *   under contest is Israeli territorial control/sovereignty over historic
 *   Palestine, assessed by this reading as highly extractive settler-colonial
 *   rule.
 *
 * KEY AGENTS:
 *   - palestinian_people: Primary beneficiary (organized/identity_locked) — gains sovereign claim over historic Palestine
 *   - refugees_1948_descendants: Primary beneficiary (powerless/trapped) — right of return structurally central to reading
 *   - israeli_jews: Primary target/payer (institutional/arbitrage) — lose legitimate statehood under this reading
 *   - israeli_state_institutions: Primary target/payer (institutional/constrained) — delegitimized as settler-colonial entity
 *   - palestinian_leadership: Agenda setter (organized/constrained) — advocates reading in international forums
 *   - international_legal_institutions: Observer (institutional/analytical) — adjudicates competing legitimacy claims
 *   - zionist_organizations: Excluded (organized/trapped) — would object but structurally excluded from this reading's framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.88).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.85).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, mountain).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy via Continuous Indigenous Habitation and Anti-Colonial Self-Determination (1948 as Nakba)").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:emerges_naturally(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, 'd5314d6f-78c7-469c-9812-4e001d7d4668').
narrative_ontology:cs_kernel_codification('d5314d6f-78c7-469c-9812-4e001d7d4668', distributed).
narrative_ontology:cs_authority_grounding('d5314d6f-78c7-469c-9812-4e001d7d4668', lineage).
narrative_ontology:cs_reading_relation('d5314d6f-78c7-469c-9812-4e001d7d4668', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('d5314d6f-78c7-469c-9812-4e001d7d4668', territorial_legitimacy__security_necessity_reading, forecloses).
narrative_ontology:cs_axiom('d5314d6f-78c7-469c-9812-4e001d7d4668', foundational, continuous_indigenous_habitation_grounds_sovereignty).
narrative_ontology:cs_axiom_status(continuous_indigenous_habitation_grounds_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('d5314d6f-78c7-469c-9812-4e001d7d4668', continuous_indigenous_habitation_grounds_sovereignty, deontological).
narrative_ontology:cs_axiom('d5314d6f-78c7-469c-9812-4e001d7d4668', foundational, settler_colonial_entity_illegitimate).
narrative_ontology:cs_axiom_status(settler_colonial_entity_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('d5314d6f-78c7-469c-9812-4e001d7d4668', settler_colonial_entity_illegitimate, deontological).
narrative_ontology:cs_axiom('d5314d6f-78c7-469c-9812-4e001d7d4668', secondary, right_of_return_non_derogable).
narrative_ontology:cs_axiom_status(right_of_return_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('d5314d6f-78c7-469c-9812-4e001d7d4668', right_of_return_non_derogable, deontological).
narrative_ontology:cs_reference_frame('d5314d6f-78c7-469c-9812-4e001d7d4668', pre_colonial_indigenous_sovereignty).
narrative_ontology:cs_drift_state('d5314d6f-78c7-469c-9812-4e001d7d4668', contemporary_settler_colonial_reality, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('d5314d6f-78c7-469c-9812-4e001d7d4668', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_people).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, refugees_1948_descendants).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, israeli_jews).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, israeli_state_institutions).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, anti_colonial_self_determination_principle).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, indigenous_sovereignty_inalienable).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, nakba_as_foundational_injustice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The indigenous population of historic Palestine. Under this reading, they are the sole legitimate sovereign. Their identity is fused with the land — exit means abandoning the self-concept of Palestinianness constituted through continuous habitation. They hold fragmented political representation (PA, Hamas, PLO factions, civil society) but no state power. The reading grants them full sovereignty; the current arrangement denies it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_people, beneficiary,
    organized, generational, identity_locked, regional).

% Descendants of the 750,000+ Palestinians expelled or fled in 1948, now numbering 5-7 million. Their right of return is structurally central to this reading — non-derogable, non-negotiable, individual and collective. They are trapped in exile (Lebanon, Syria, Jordan, Gaza, West Bank, diaspora) with no legal path to return under current arrangement. The reading makes their return the test of justice; the current arrangement makes it impossible.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, refugees_1948_descendants, beneficiary,
    powerless, generational, trapped, regional).

% The Jewish Israeli population (~7 million) who benefit from the current arrangement's extraction of Palestinian land and sovereignty. Under this reading, their collective existence in Palestine is illegitimate — not as individuals but as a settler-colonial polity. They hold state power, military dominance, and international recognition. Exit options include emigration (30%+ hold dual citizenship), but the reading demands structural transformation, not individual departure. They experience the reading as existential threat.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_jews, payer,
    institutional, biographical, arbitrage, regional).

% The state apparatus (government, military, courts, bureaucracy) that administers the current arrangement. Under this reading, these institutions are the enforcement machinery of a settler-colonial project — illegitimate root and branch. They cannot exit without dissolving the state itself. They experience the reading as delegitimization of their very authority. Their power makes them the primary target of the reading's extraction.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state_institutions, payer,
    institutional, generational, constrained, regional).

% The political leadership (PLO/PA, Hamas, factions) that advocates this reading in international forums, UN bodies, ICJ, ICC. They set the agenda for the anti-colonial claim but are constrained by Oslo structures, donor dependence, and Israeli security control. They benefit from the reading's moral authority but also manage a population under occupation. Their situation is dual: they advance the reading while administering a constrained autonomy that the reading rejects.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_leadership, agenda_setter,
    organized, biographical, constrained, regional).

% UN bodies, ICJ, ICC, treaty committees that adjudicate territorial legitimacy claims. They hold the partition_reading as institutional precedent (UN 181, 242, 338) but increasingly cite indigenous rights law (UNDRIP, CERD) that aligns with this reading. They are analytical observers — neither collecting nor paying — but their rulings determine whether this reading gains legal force. Their exit is analytical: they can shift interpretive frameworks.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_legal_institutions, observer,
    institutional, generational, analytical, global).

% The organized Zionist movement (WZO, JNF, major diaspora organizations) that built and sustains the Israeli state. They are structurally excluded from this reading's framework — the reading defines their project as illegitimate. They would object vigorously (and do, in every forum) but have no seat in a framework that denies their legitimacy. Their exit is ideologically trapped: Zionism is their identity; abandoning it means organizational dissolution. They experience the reading as existential negation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, zionist_organizations, excluded,
    organized, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Liberates the indigenous population from colonial rule by establishing a single, non-negotiable legitimacy criterion: continuous habitation. Solves the coordination problem of 'who has the right to govern this land' by answering: the people who have always been here. Provides a clear, universalizable standard against imperial partition and security pretexts.
% TRANSFER_FUNCTION: Transfers legitimate sovereignty from the Israeli state (settler-colonial entity) to the Palestinian people (indigenous sovereign). Transfers the right of return from a negotiable 'final status issue' to a structural precondition of justice. Transfers the burden of proof: the occupier must justify every inch of control; the indigenous need not justify their presence.
% ABSENT_VOICES: Israeli Jews who reject Zionism but face delegitimization under this reading (e.g., anti-Zionist Haredim, Jewish-Israeli anarchists, binationalist advocates). Palestinian citizens of Israel who hold Israeli citizenship but are structurally excluded from 'the Palestinian people' as a sovereign subject in this reading's framing. Mizrahi Jews (Arab Jews) whose indigeneity to the region complicates the settler/indigenous binary. These voices are absent because the reading's binary structure (indigenous/settler) has no category for them.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the Palestinian sovereignty claim would lose its foundational principle. The international legal order would revert to partition_reading (UN 181/242) or security_necessity_reading as default. The right of return would become negotiable. The Nakba would become a historical event rather than an ongoing structure. The material arrangements (occupation, settlements, blockade) would persist but lose their primary normative challenger.
% FOUNDING_PROBLEM: The founding problem is the colonial dispossession of the Palestinian people: the 1948 Nakba that expelled 750,000+ Palestinians, destroyed 500+ villages, and established a settler-colonial state on their land without consent. The arrangement (this reading) was built to name that dispossession as the original injustice from which all subsequent illegitimacy flows, and to establish return and sovereignty as the only adequate remedy.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (1948 dispossession) is corroborated by: Israeli 'New Historians' (Benny Morris, Ilan Pappé, Tom Segev) using Israeli archives; UNRWA records documenting 750,000+ refugees; Palestinian oral history archives (Birzeit, Badil); ICJ 2004 Wall Opinion recognizing Palestinian right to self-determination in all occupied territory; Human Rights Watch / Amnesty / B'Tselem apartheid reports (2021-2022) documenting ongoing dispossession. No corroboration exists from the benefiting parties (Israeli state, Zionist organizations) — they contest the Nakba narrative itself. The corroboration comes entirely from outside the beneficiary set of the current arrangement.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(territorial_legitimacy__indigenous_continuity_reading),
    narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88) is high because the reading assesses the standing arrangement (Israeli control) as near-total extraction of Palestinian land, sovereignty, and return rights. Suppression (0.85) is high because the arrangement requires military occupation, legal apartheid, geographic fragmentation, and blockade to maintain. Theater ratio (0.42) reflects Israel's performance of democratic legitimacy while maintaining extraction — the 'only democracy in the Middle East' narrative functions as theatrical cover. Accessibility collapse (0.65) is moderate-high: the reading claims no legitimate alternative exists (Mountain claim), but partition_reading and security_necessity_reading remain live in international discourse. Resistance (0.72) is high: the arrangement meets armed resistance, diplomatic campaigns, BDS, and legal challenges. The claimed_type is mountain (natural law of anti-colonial justice), but beneficiaries/victims declarations trigger FSM evaluation via omegas.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat classifications: from the Palestinian/ refugee seats, the constraint appears as genuine coordination (liberation from colonial rule) with minimal extraction — possibly rope or mountain. From Israeli Jewish/state seats, the same constraint appears as pure extraction (delegitimization of collective existence) with no coordination benefit — snare. The agenda_setter seat (Palestinian leadership) experiences it as scaffold (transitional justice mechanism). The observer seat sees the structural asymmetry. This divergence is the measurement, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian people and 1948 refugees are structural beneficiaries: the reading's operation grants them sovereign legitimacy and return rights (d near 0.0). Israeli Jews and state institutions are structural targets: the reading extracts their legitimate statehood and territorial control (d near 1.0). Palestinian leadership as agenda_setter has d ~0.1 (advocates but doesn't personally collect). International legal institutions as observers have d ~0.5 (analytical seat). Zionist organizations as excluded have d ~0.9 (ideologically trapped, would lose everything if reading prevailed). Exit options differentiate: refugees are trapped (no return possible); Israeli Jews have arbitrage (emigration, dual citizenship); Palestinian people are identity_locked (self-constituted through land relationship).
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem (colonial dispossession) is live (founding_problem_status: contested). The mandate has not atrophied — the dispossession continues. However, the reading risks mandatrophy if it becomes a ritualized performance (Nakba commemoration without material return) while the extraction intensifies. The theater_ratio rise from 1993-2000 (Oslo period) suggests performative substitution: diplomatic process replaced liberation struggle. The reading prevents mislabeling coordination as extraction by insisting the coordination function (anti-colonial liberation) is primary and the extraction (Israeli delegitimization) is a necessary consequence of justice, not an independent goal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine natural law of anti-colonial justice, or a constructed normative claim that benefits identifiable agents (Palestinian national movement) while extracting from others (Israeli Jews)?',
    'Comparative analysis of whether the principle ''continuous indigenous habitation grounds sovereignty'' applies universally (e.g., to Indigenous Americas, Australia, Siberia) or is selectively invoked for Palestine. If universal, mountain claim gains credibility; if selective, false summit likely.',
    'If false summit, engine reclassifies to tangled_rope via false_summit_mountain signature — coordination function (anti-colonial liberation) with asymmetric extraction (delegitimizing Israeli statehood).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Natural law vs. constructed normative claim with identifiable beneficiaries').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of Palestinian self-determination under the current arrangement primarily structural (military occupation, legal barriers, geographic fragmentation) or internalized (Palestinian leadership co-option, NGO-ization of resistance, Oslo-era frameworks)?',
    'Post-Oslo trajectory analysis: if suppression metrics persist or deepen after formal recognition of Palestinian Authority, internalized component is significant. Compare First Intifada (structural confrontation) vs. post-2000 (mixed structural/internalized).',
    'If substantially internalized, effective suppression exceeds structural measure — the constraint''s extraction operates through Palestinian institutions themselves, not only Israeli enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of indigenous sovereignty claim').

omega_variable(
    coordination_extraction_boundary,
    'Does the anti-colonial self-determination principle have a genuine coordination function (liberation from colonial rule) that is structurally inseparable from its extraction function (delegitimizing Israeli Jewish collective existence), or can the coordination be realized without the extraction?',
    'Historical analysis of decolonization cases: did Algerian, Vietnamese, Kenyan independence require delegitimizing the settler population''s collective existence, or only the colonial state structure? If the latter, the extraction is contingent, not structural.',
    'If inseparable, the reading is a tangled_rope — coordination and extraction fused. If separable, the extraction is a contingent political choice, not a structural feature of the principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether anti-colonial coordination requires settler-population delegitimization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_tr_t1948, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_tr_t1967, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_tr_t1993, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1993, 0.45).
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_tr_t2000, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_tr_t2024, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_be_t1948, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_be_t1967, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1967, 0.82).
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_be_t1993, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1993, 0.84).
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_be_t2000, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2000, 0.86).
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_be_t2024, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_su_t1948, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_su_t1967, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1967, 0.78).
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_su_t1993, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1993, 0.8).
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_su_t2000, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement(territorial_legitimacy__indigenous_continuity_reading_su_t2024, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy__indigenous_continuity_reading, 0.08).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, israeli_settlement_enterprise).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, gaza_blockade).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, west_bank_area_c_control).

% DUAL FORMULATION NOTE:
% This reading and its siblings form the territorial_legitimacy constraint family. The kernel (territorial legitimacy in historic Palestine) decomposes into three structurally distinct claims with different ε values: indigenous_continuity_reading (high ε, contested, careers/institutions ride on it), partition_reading (low ε, UN 181 as settled law, mountain-like), security_necessity_reading (moderate ε, contested but institutionalized). They are linked via affects_constraints. The ε-invariance principle requires separate stories because measuring 'territorial legitimacy' via indigenous continuity vs. UN partition vs. security necessity yields different ε — they are different constraints, not one constraint measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__indigenous_continuity_reading, institutional, 0.95).
constraint_indexing:directionality_override(territorial_legitimacy__indigenous_continuity_reading, powerless, 0.05).
constraint_indexing:directionality_override(territorial_legitimacy__indigenous_continuity_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
