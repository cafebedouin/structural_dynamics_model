% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: British Mandate Interpretive Discretion Without External Review
 *   domain: international_law/colonial_administration
 *
 * SUMMARY:
 *   The British League of Nations mandate for Palestine (1920-1948) included
 *   two ostensibly equal obligations: to facilitate a Jewish national home
 *   and to preserve the civil and political rights of existing (Arab
 *   Palestinian) inhabitants. The mandate text is textually ambiguous on
 *   priority and reconciliation. Rather than resolving this ambiguity through
 *   fixed interpretation or external arbitration, British practice asserted
 *   administrative discretion to adjudicate between the readings
 *   case-by-case, policy-phase-by-phase. The 1920 settlement favored Jewish
 *   immigration and land acquisition; the 1922 Churchill White Paper
 *   moderated language; the 1930 Passfield White Paper restricted Jewish land
 *   access; the 1939 White Paper reversed course again with restrictive
 *   immigration caps. Each shift left both communities unable to consolidate
 *   gains or appeal to textual meaning. The constraint is not the mandate
 *   text itself (which both communities cite) — it is the British claim to
 *   unilateral interpretive authority without external review, and the
 *   resulting oscillation that creates path-dependent lock-in.
 *
 * KEY AGENTS:
 *   - british_colonial_administration: institutional agenda-setter, holds interpretive discretion without external review; arbitrage exit (transfer mandate to successor state or withdraw)
 *   - arab_palestinian_communities: organized payer, face irreversible territorial/demographic consequences of policy oscillation; identity-locked exit (territorial severance from national community)
 *   - zionist_communities: organized payer with secondary beneficiary role, depend on discretionary decisions for immigration/land rights; identity-locked exit (abandonment of national project)
 *   - league_of_nations_council: excluded nominal authority, lacks enforcement mechanisms to compel British compliance or review
 *   - labour_zionist_institutional_leadership: organized beneficiary, accumulates institutional apparatus renewed by discretionary British grants; mobile exit
 *   - arab_landholding_elites: powerful payer, face asset devaluation under restrictive phases; arbitrage exit through diaspora migration
 *   - international_law_commentators: analytical observers, note textual ambiguity but lack enforcement mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.68).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.72).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "British Mandate Interpretive Discretion Without External Review").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, '198355a7-6574-4c8a-a52d-3f199f6d02aa').
narrative_ontology:cs_kernel_codification('198355a7-6574-4c8a-a52d-3f199f6d02aa', fixed_text).
narrative_ontology:cs_authority_grounding('198355a7-6574-4c8a-a52d-3f199f6d02aa', extraction).
narrative_ontology:cs_interpretation_layer_present('198355a7-6574-4c8a-a52d-3f199f6d02aa').
narrative_ontology:cs_reading_relation('198355a7-6574-4c8a-a52d-3f199f6d02aa', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('198355a7-6574-4c8a-a52d-3f199f6d02aa', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_axiom('198355a7-6574-4c8a-a52d-3f199f6d02aa', foundational, mandatory_power_interpretive_monopoly).
narrative_ontology:cs_axiom_status(mandatory_power_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('198355a7-6574-4c8a-a52d-3f199f6d02aa', mandatory_power_interpretive_monopoly, conventional).
narrative_ontology:cs_axiom('198355a7-6574-4c8a-a52d-3f199f6d02aa', secondary, external_review_foreclosure).
narrative_ontology:cs_axiom_status(external_review_foreclosure, holdable).
narrative_ontology:cs_axiom_grounding('198355a7-6574-4c8a-a52d-3f199f6d02aa', external_review_foreclosure, conventional).
narrative_ontology:cs_reference_frame('198355a7-6574-4c8a-a52d-3f199f6d02aa', unilateral_british_mandate_authority).
narrative_ontology:cs_drift_state('198355a7-6574-4c8a-a52d-3f199f6d02aa', post_1945_decolonization_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('198355a7-6574-4c8a-a52d-3f199f6d02aa', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administration).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_palestinian_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_communities).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, labour_zionist_institutional_leadership).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_settler_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_landholding_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the League of Nations mandate authority and claims the right to adjudicate between competing interpretations of the mandate instruments without appeal. Sets policy through White Papers, administrative directives, and discretionary decisions on land acquisition, immigration quotas, and institutional recognition. Policy oscillates: 1920 settlement favors Jewish immigration and institutional development; 1922 Churchill White Paper moderates language; 1930 Passfield White Paper restricts Jewish land acquisition; 1939 White Paper reverses course with restrictive immigration caps. Each shift reframes the baseline for subsequent negotiations.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administration, agenda_setter,
    institutional, generational, arbitrage, global).

% Face irreversible territorial and demographic consequences of each policy shift. Land sales under 1920-era encouragement to Jewish migration create property transfer precedents that constrain Arab bargaining position when policy reverses. Political participation in advisory structures is nominated rather than elected; representation is reformulated as policy changes. Exit is identity-locked: Palestinian Arab identity is territorially constituted; leaving Palestine means severance from national community. Unable to consolidate territorial gains or appeal to textual mandate equality.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_palestinian_communities, payer,
    organized, generational, identity_locked, regional).

% Depend on British discretionary decisions for immigration quotas, land acquisition rights, and institutional autonomy (Jewish Agency recognition). Each policy reversal creates strategic uncertainty about the mandate's trajectory. The 1939 White Paper constrains future demographic possibilities through immigration caps. Identity is fused with the national-home project; exit means abandonment of the Zionist enterprise itself. Within this constraint, they also benefit from the same discretionary framework that disadvantages Arabs: institutional recognition, immigration facilitation under favorable policy phases, land purchase encouragement.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_communities, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_communities, beneficiary).

% Holds nominal supervisory authority over mandate performance but lacks practical mechanisms to compel British compliance or review British interpretations of the mandate text. British discretion operates behind the structural fact that external review requires political mobilization the League cannot sustain.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, league_of_nations_council, excluded,
    institutional, generational, constrained, global).

% Negotiates directly with British officials and accumulates institutional apparatus (Jewish Agency, Histadrut, kibbutz federations) whose legitimacy is renewed through each British discretionary grant. Has greatest material investment in British favor and highest ability to shift strategy as policy changes. Geographic and professional mobility through diaspora connections gives exit options absent for smaller communities.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, labour_zionist_institutional_leadership, beneficiary,
    organized, biographical, mobile, regional).

% Face land sales under favorable British policy phases and asset devaluation under restrictive ones. They have capital and diaspora networks enabling exit (migration to Syria, Egypt, Iraq) unavailable to peasant communities. Their exit drains political leadership from Arab Palestinian structures, leaving the peasant base without elite representation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_landholding_elites, payer,
    powerful, biographical, arbitrage, regional).

% Benefit from administrative positions, commercial contracts, and the order that British discretion imposes. Accumulate property and institutional standing dependent on British favor. Geographic mobility to other dominions or Britain gives exit superior to local communities.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_settler_communities, beneficiary,
    moderate, biographical, mobile, regional).

% Analyze mandate text for binding obligations and British compliance. Note the tension between the textual instruction to facilitate a Jewish national home and to respect existing Arab rights, but lack enforcement mechanisms. Interpretations are cited by litigants and advocates but do not constrain British administrative discretion.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, international_law_commentators, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administration).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__mandatory_interpretive_discretion, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes a colonial settlement under League authority nominally accountable to international law. The mandate framework solves a coordination problem: post-WWI restructuring of Ottoman territories required agreed mechanisms for transition governance, property rights recognition, and minority protection. Without it, unilateral territorial seizure and ethno-national conflict would have escalated immediately.
% TRANSFER_FUNCTION: Transfers control of interpretation to the mandatory power (Britain). British discretion to define what the mandate instruments mean becomes the central operational mechanism. Land rights, immigration policy, political participation, and institutional recognition flow through British discretionary decision-making. The constraint moves interpretive authority from the text (which is ambiguous and contestable) to the administrator (who is unilateral and unreviewable).
% ABSENT_VOICES: International law experts and League mechanisms that would otherwise adjudicate mandate compliance are structurally excluded by British assertion of interpretive monopoly. Local land courts and Palestinian Arab political institutions that might adjudicate property rights independently. Jewish communities outside the mandate (diaspora) bear consequences of policy shifts but have no formal voice. British Parliamentary opposition and domestic critics are partially excluded from real-time policy influence.
% DISAPPEARANCE_RATIONALE: If British interpretive discretion vanished and the mandate reverted to fixed textual meaning, the baseline for both communities' negotiations would stabilize. Territorial consequences would become path-dependent on text rather than British preference. Both communities would reorganize to secure fixed textual commitments rather than compete for administrative favor. The mandate's institutional apparatus (Jewish Agency, Arab Executive, British civil administration) would reorganize around legal interpretation rather than administrative discretion. The constraint's collapse would not resolve the underlying conflict but would shift it from a British-mediated interpretation game to a direct contest over which sibling reading (dual_obligation_indigenous_rights vs jewish_national_home_primacy) the text supports.
% FOUNDING_PROBLEM: Post-WWI territorial settlement of Ottoman Palestine required governance mechanism acceptable to Allied powers, Ottoman successor states, emerging Zionist institutions, and existing Arab inhabitants. The mandate framework was designed to solve a coordination problem: how to manage simultaneous nation-building projects (Jewish national home, Arab self-determination aspirations) under international transition governance without immediate escalation to coercive partition or ethnic conflict.
% FOUNDING_PROBLEM_CORROBORATION: British administrators attest the founding problem required flexibility to manage conflicting obligations and shifting colonial circumstances; fixed textual interpretation would have paralyzed governance. Arab Palestinian leadership attests the problem was solvable through fixed text (equal protection of existing Arab rights plus minority protections for Jewish settlement) without British discretionary override; British flexibility was tactical dominance, not necessity. Zionist leadership attests the problem required demographic transformation and institutional development that only British discretion could facilitate against Arab resistance. International law scholarship from the era (outside both communities' institutional interests) documents the genuine textual ambiguity: the mandate text instructs Britain to facilitate a Jewish national home AND to protect the civil and political rights of existing (Arab) inhabitants, without resolving the hierarchical relationship if they conflict. All three positions are corroborated from sources outside the others' benefiting parties.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.68 at end): the constraint's core mechanism is that both communities cannot resolve the interpretive contest through textual meaning or external appeal — they must instead negotiate with British discretion on terms the British set. Suppression runs higher (0.72) because maintaining this interpretive monopoly requires blocking external review and discounting textual constraint. Theater is substantial (0.58) because a significant share of British activity is devoted to rhetorical management of the apparent mandate obligations rather than substantive reconciliation of the two readings. The measurement series tracks oscillation across three major policy phases (1920-1930 pro-Zionist, 1930-1939 moderating, 1939-1948 restrictive), with extractiveness and suppression requirements peaking in the 1930-1936 interval when policy oscillation was most destabilizing. By 1948, as the mandate approached collapse and Israeli independence, extractiveness and suppression requirement both declined somewhat because the constraint's operative force was already being superseded by state formation and war. The cyclical pattern (rising then declining) reflects not stable extraction but rather a constraint that generated maximal path-dependent lock-in during its middle phases and then degraded as the political-military situation overwhelmed the administrative structure.
 *
 * PERSPECTIVAL GAP:
 *   The British seat computes as coordination under this reading (interpretive discretion solves a genuine ambiguity-management problem); the Arab and Zionist seats compute as snare (the same interpretive discretion operates as enforced extraction, trapping them in oscillation they cannot resolve by appeal). The divergence reflects asymmetric structural position: the British hold the interpretive frame; the communities compete within it on terms they do not set.
 *
 * DIRECTIONALITY LOGIC:
 *   British colonial administration: d ≈ 0.2 (full beneficiary). Controls the interpretive frame, sets policy, faces no external constraint, holds arbitrage exit (can transfer mandate or withdraw). Extraction accrues to British administrators as policy flexibility and divide-and-rule capacity. Arab Palestinian communities: d ≈ 0.85 (near-full target). Organized and not powerless, but face identity-locked exit (territorial severance) and constrained participation in policy formation. Each policy shift creates irreversible territorial/demographic consequences they cannot reverse. Zionist communities: d ≈ 0.75 (substantial target, with modulation). Also identity-locked, but with higher institutional capacity to negotiate and greater ability to shape policy through organizational activity. Benefit from favorable phases, harmed by restrictive ones; the dual role reflects exposure to both extraction and coordination benefits. Labour Zionist institutional leadership: d ≈ 0.60 (moderate target with mobile exit). Highest ability to shift strategy as policy changes; accumulate institutional standing that depends on British favor but retain professional/diaspora mobility. Arab landholding elites: d ≈ 0.55 (moderate-to-high target, with arbitrage exit). Face asset devaluation but have capital and diaspora networks enabling exit (flight to neighboring states); their exit drains Arab political capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem — how to manage simultaneous nation-building projects under international transition governance — is CONTESTED at termination (1948). British administrators argue they successfully balanced equal obligations and facilitated orderly transition; Arab Palestinian leadership argues the problem required fixed textual protection that British discretion systematically overrode; Zionist leadership argues the problem required demographic transformation that only British discretion could facilitate. The mandatrophy check: founding_problem_status = contested, disappearance_verdict = world_rearranges, theater_ratio = 0.58 (substantial but not dominant). This constellation predicts: the constraint is a snare whose founding coordination function (transition governance) is contested, and whose operative function (interpretive discretion enabling divide-and-rule) persists through theater (White Papers, advisory bodies, promises of future remedy) rather than genuine coordination benefit. The 1948 termination was not mandatrophy resolution through function change — it was the constraint's collapse under external military/political pressure. Mandatrophy_resolved = false: the constraint was never reclassified or remedied by internal recognition of function decay; it was overrun by state formation and war.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_internalization_depth,
    'Is the measured suppression substantially structural/external (British institutional barriers to appeal and review) or partially internalized (both communities have absorbed the expectation of British override and cease appealing to textual meaning)?',
    'Post-mandate institutional behavior: if Palestinian Arab and Israeli legal systems show capacity for independent textual interpretation and self-correction after 1948, suppression is primarily structural; if legal culture remains oriented to external override or authority-deference, suppression has internalized components.',
    'If primarily structural, the constraint degrades rapidly when British authority is removed and interpretation reverts to text-based contestation. If partially internalized, both communities remain constrained even after institutional independence because they carry the expectation of external override into their legal/political cultures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_depth, empirical, 'Structural vs. internalized suppression in mandate-era interpretive discretion.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does this reading (mandatory_interpretive_discretion) logically foreclose either of the sibling readings (dual_obligation_indigenous_rights and jewish_national_home_primacy) within a single unified framework, or do the readings coexist as incompatible but non-contradictory positions held by different parties?',
    'Logical analysis: can a single legal framework simultaneously assert (1) that the mandate text requires equal/superior obligation to Arab rights AND (2) that it directs demographic transformation for Jewish sovereignty, if a third principle permits the mandatory power to adjudicate which applies? If yes, the readings coexist; if no, one forecloses the other.',
    'Foreclosure would indicate a fundamental logical contradiction in the kernel itself; coexistence would indicate a framework that permits multiple readings simultaneously and relies on the mandatory power to choose among them. Coexistence supports this reading''s classification as snare (the discretion to choose creates the constraint); foreclosure would suggest one sibling reading is structurally impossible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Logical relationship between this reading and sibling interpretations of the mandate text.').

omega_variable(
    mandate_text_determinacy,
    'Is the mandate text genuinely ambiguous on the relationship between the two obligations (national home + Arab rights), or does textual analysis favor one sibling reading over the other?',
    'Close reading of the League of Nations Covenant Art. 22, the Palestine Mandate instrument text, and contemporary legal commentary on textual hierarchy and reconciliation principles. Judgment by international law scholars without institutional stake in the outcome.',
    'If the text is genuinely ambiguous, British discretion is a plausible mechanism for managing irresolvable ambiguity, and this reading captures a real constraint. If the text favors one sibling reading, British discretion operates primarily as suppression of the textually subordinate reading rather than as ambiguity management.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_text_determinacy, empirical, 'Degree of textual determinacy on the relationship between mandate obligations.').

omega_variable(
    alternative_dispute_resolution_mechanisms,
    'Could the same coordination problem (post-WWI transition governance) have been solved by a mandate instrument that committed to fixed textual interpretation, with disputes referred to external arbitration (League-appointed commission, international court, or third-party arbitration)?',
    'Counterfactual comparison with League mandates and post-WWI territorial arrangements in other regions that used fixed textual standards and external dispute resolution (e.g., minority-protection provisions in League treaties with Eastern European states).',
    'If alternative mechanisms existed and were not chosen for Palestine specifically, British discretion was a deliberate preference for administrative flexibility rather than a necessity. If alternatives would have been unworkable given the mandate''s unique circumstances (post-Ottoman transition, competing nation-building projects), British discretion becomes more defensible as the only viable mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_dispute_resolution_mechanisms, conceptual, 'Availability and viability of alternative mechanisms for managing interpretive disputes in the mandate framework.').

omega_variable(
    british_interpretive_pattern_asymmetry,
    'Does British interpretive discretion show a consistent directional bias toward one sibling reading over the other (jewish_national_home_primacy vs dual_obligation_indigenous_rights), or does it oscillate without pattern?',
    'Systematic analysis of all major British policy decisions (1920, 1922, 1930, 1939, WWII era) and their textual justifications: do reversals represent genuine reinterpretation or strategic repositioning? Do restrictive and permissive phases show equal legitimacy in textual terms?',
    'If asymmetric bias toward one sibling reading, the discretion operates primarily as suppression of that reading and strategic advantage to the other. If true oscillation, the discretion captures genuine ambiguity management with collateral cost to both communities. Asymmetry would suggest snare (weaponized discretion); oscillation would suggest tangled_rope (genuine coordination with extractive collateral).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(british_interpretive_pattern_asymmetry, empirical, 'Pattern and directionality of British interpretive choices across the mandate period.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1920, 0.42).
narrative_ontology:measurement(balf_tr_t1924, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1924, 0.48).
narrative_ontology:measurement(balf_tr_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1930, 0.55).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1936, 0.62).
narrative_ontology:measurement(balf_tr_t1942, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1942, 0.6).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1948, 0.58).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement(balf_be_t1924, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1924, 0.62).
narrative_ontology:measurement(balf_be_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1930, 0.66).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1936, 0.7).
narrative_ontology:measurement(balf_be_t1942, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1942, 0.68).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1948, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1920, 0.55).
narrative_ontology:measurement(balf_su_t1924, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1924, 0.6).
narrative_ontology:measurement(balf_su_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1930, 0.68).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1936, 0.78).
narrative_ontology:measurement(balf_su_t1942, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1942, 0.75).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1948, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.18).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% This constraint belongs to the 'balfour_mandate_instruments' kernel family. The kernel is the text of the Palestine Mandate (1920) and its implementation. Three structurally distinct claims decompose the kernel: (1) 'jewish_national_home_primacy' — interprets the mandate as directing demographic/territorial transformation toward Jewish sovereignty. (2) 'dual_obligation_indigenous_rights' — interprets the mandate as imposing equal or superior obligation to protect Arab rights and land tenure. (3) 'mandatory_interpretive_discretion' (this reading) — focuses on the structural fact that the mandatory power held authority to adjudicate between the two readings without external review, and that this monopoly on interpretation became the operational constraint system. The three readings are not competing interpretations of one text viewed from different angles; they are three distinct constraints with three distinct epsilon values, beneficiary/victim structures, and operational mechanisms. The first two readings contest which interpretation is textually correct and binding. The third reading analyzes how the mandatory power's claim to unilateral interpretive authority created a different, higher-order constraint on both communities' agency. All three share a common kernel (the mandate text) and a common operational domain (British-administered Palestine 1920-1948), but they have independent ε values and distinct structural positions. Epsilon values expected to differ substantially: (1) jewish_national_home_primacy = high epsilon (the mandate instruments are read as directing extraction from Arab communities for Zionist settlement and state-building; beneficiary is Zionist institutions and British facilitators; victims are Arab Palestinians), (2) dual_obligation_indigenous_rights = moderate epsilon (the mandate instruments contain genuine coordination (international transition governance) and genuine protection obligations; extractiveness arises from their systematic non-enforcement, not from textual direction), (3) mandatory_interpretive_discretion = moderate epsilon snare (both communities face strategic uncertainty from oscillating discretion; neither can resolve the contest through textual appeal; British administrators benefit from flexibility and divide-and-rule capacity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balfour_mandate_instruments__mandatory_interpretive_discretion, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
