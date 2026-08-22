% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Fifth Republic Constitution — Hyper-Presidential Reading
 *   domain: constitutional_law/political_systems/comparative_government
 *
 * SUMMARY:
 *   This constraint story models the hyper-presidential reading of the French
 *   Fifth Republic Constitution (1958). In this reading, the President is the
 *   direct sovereign embodying national will through direct universal
 *   suffrage, with the legislature (National Assembly and Senate) minimally
 *   constraining executive action. Key mechanisms: Article 5 (President as
 *   arbiter ensuring regular functioning), Article 8 (President appoints PM
 *   but can dismiss only on PM's resignation), Article 16 (exceptional crisis
 *   powers), and critically Article 49.3 (government engages responsibility
 *   on a text — adoption without vote unless motion of censure passes). The
 *   reading treats these as a coherent system where presidential dominance is
 *   the constitutional norm, not an exception. Legislative constraint is
 *   weak: censure motions rarely succeed (only 1962 under Pompidou), 49.3
 *   enables legislative bypass, and the President's dissolution power
 *   (Article 12) disciplines the Assembly. The beneficiary is the presidency
 *   as institution and the incumbent president; victims are the legislature,
 *   opposition, and ultimately the citizen electorate whose representative
 *   channel is structurally weakened. This is ONE READING of the contested
 *   kernel 'fifth_republic_constitution' — the sibling readings are
 *   parliamentary_constraint_reading and cohabitation_equilibrium_reading.
 *
 * KEY AGENTS:
 *   - presidency_institution: Primary beneficiary (institutional/analytical) — structural recipient of concentrated executive authority
 *   - incumbent_president: Primary beneficiary (institutional/arbitrage) — personal office-holder exercising concentrated powers
 *   - national_assembly: Primary victim (organized/constrained) — legislative body subject to dissolution, 49.3 bypass, and presidential agenda-setting
 *   - senate: Secondary victim (organized/constrained) — upper house with limited powers vs. Assembly and President
 *   - opposition_parties: Victim (organized/constrained) — structurally excluded from executive power, censure motions rarely viable
 *   - citizen_electorate: Victim (powerless/mobile) — direct suffrage legitimizes presidential dominance but weakens parliamentary accountability
 *   - conseil_constitutionnel: Observer (institutional/analytical) — judicial reviewer with limited ex ante review capacity
 *   - prime_minister: Dual role payer/beneficiary (organized/constrained) — executes presidential policy but bears political responsibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.72).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.68).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Constitution — Hyper-Presidential Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional_law/political_systems/comparative_government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, '756cba54-f821-46ec-a726-84aef40221e9').
narrative_ontology:cs_kernel_codification('756cba54-f821-46ec-a726-84aef40221e9', fixed_text).
narrative_ontology:cs_authority_grounding('756cba54-f821-46ec-a726-84aef40221e9', extraction).
narrative_ontology:cs_interpretation_layer_present('756cba54-f821-46ec-a726-84aef40221e9').
narrative_ontology:cs_reading_relation('756cba54-f821-46ec-a726-84aef40221e9', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('756cba54-f821-46ec-a726-84aef40221e9', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('756cba54-f821-46ec-a726-84aef40221e9', foundational, president_as_direct_sovereign).
narrative_ontology:cs_axiom_status(president_as_direct_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('756cba54-f821-46ec-a726-84aef40221e9', president_as_direct_sovereign, conventional).
narrative_ontology:cs_axiom('756cba54-f821-46ec-a726-84aef40221e9', foundational, direct_universal_suffrage_supremacy).
narrative_ontology:cs_axiom_status(direct_universal_suffrage_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('756cba54-f821-46ec-a726-84aef40221e9', direct_universal_suffrage_supremacy, conventional).
narrative_ontology:cs_axiom('756cba54-f821-46ec-a726-84aef40221e9', secondary, parliamentary_legitimacy_subordinate).
narrative_ontology:cs_axiom_status(parliamentary_legitimacy_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('756cba54-f821-46ec-a726-84aef40221e9', parliamentary_legitimacy_subordinate, conventional).
narrative_ontology:cs_reference_frame('756cba54-f821-46ec-a726-84aef40221e9', gaullist_constitutional_settlement).
narrative_ontology:cs_drift_state('756cba54-f821-46ec-a726-84aef40221e9', post_quinquennat_reforms, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('756cba54-f821-46ec-a726-84aef40221e9', '2026-08-04T12:00:00Z').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_institution).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, senate).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, opposition_parties).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, citizen_electorate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, citizen_electorate).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, prime_minister).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, prime_minister).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, direct_universal_suffrage_legitimacy).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, national_unity_personified).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, decisive_executive_action).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The presidency as a permanent institution holds the concentrated constitutional powers (Articles 5, 8, 12, 16, 49.3 via PM). It benefits from agenda control, appointment authority, dissolution power, and symbolic embodiment of the nation. Its exit is arbitrage-grade: the institution persists across office-holders and could theoretically be reformed, but it controls the reform agenda.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency_institution, beneficiary,
    institutional, generational, arbitrage, national).

% The sitting president personally exercises the institution's powers. Direct universal suffrage (since 1962) provides personal democratic legitimacy that outweighs the PM's parliamentary legitimacy. Can set policy agenda, appoint/dismiss PM, dissolve Assembly, invoke Article 16, and drive 49.3 usage. Exit is arbitrage: term-limited but with immense platform; post-presidency influence is substantial.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter).

% The lower house retains formal legislative power and censure authority (Article 49.2), but in practice: government controls agenda (Article 48), 49.3 allows adoption without vote, dissolution threat (Article 12) disciplines dissent, and quinquennat aligns elections making cohabitation unlikely. Deputies' institutional role is fixed — they cannot exit the constitutional framework without regime change.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    organized, biographical, constrained, national).

% Upper house with limited powers: no censure power, no dissolution target, legislative shuttle but Assembly has final word. Indirect election (grands électeurs) insulates from direct presidential pressure but also from democratic accountability. Institutionally entrenched — reform requires constitutional revision the presidency controls.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, senate, payer,
    organized, generational, constrained, national).

% Structurally excluded from executive power (no coalition necessity under hyper-presidential reading). Censure motions are the only parliamentary weapon but require absolute majority — nearly impossible when president's party controls Assembly. Their exit is constrained: they operate within the system hoping for electoral alternation, but the system favors presidential majority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, opposition_parties, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, opposition_parties, excluded).

% Directly elects the president (beneficiary: clear choice, accountability to one figure). But this same election weakens parliament (legislative elections become 'confirmatory' referendums on president). The citizen bears the cost of reduced parliamentary accountability, weakened local representation, and policy responsiveness channeled through presidential persona. Exit is mobile (vote, abstain, emigrate) but identity-locked to republican citizenship.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, citizen_electorate, payer,
    powerless, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, citizen_electorate, beneficiary).

% Appointed by and responsible to the president (not parliament, except via 49.3 censure). Executes presidential policy agenda, bears political responsibility for unpopular reforms, can be dismissed only by tendering resignation. Gains executive machinery access (beneficiary) but is structurally subordinate (payer). Exit is constrained: resignation is the only exit, which ends political career at that level.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, prime_minister, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, prime_minister, beneficiary).

% Constitutional council with ex ante review (Article 61) of organic laws, parliamentary rules, and treaties. Can be seized by President, PM, presidents of both houses, 60 deputies, 60 senators. No ex post review of laws in force (until 2008 QPC, which is limited). Its constraint on presidential power is real but narrow — it reviews procedure and competence, not political substance.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, conseil_constitutionnel, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__hyper_presidential_reading, presidency_institution).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__hyper_presidential_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides decisive executive authority in a system historically prone to parliamentary paralysis (Fourth Republic). Concentrates legitimacy in a directly elected figure who can act swiftly, especially in crisis (Article 16), and ensures government stability by making the executive responsible to the president rather than a fractious parliament.
% TRANSFER_FUNCTION: Moves policy initiative, appointment power, legislative agenda control, and symbolic national embodiment from the legislature to the presidency. The legislature transfers its capacity to constrain the executive (via 49.3 bypass, dissolution threat, aligned electoral calendar) and receives in return a subordinate role of ratification and limited oversight.
% ABSENT_VOICES: The parliamentary_constraint_reading's natural constituency — those who believe the Prime Minister should be the true executive responsible to parliament — are structurally excluded from power under this reading. The cohabitation_equilibrium_reading's constituency — those who see divided executive as a feature not a bug — were marginalized by the quinquennat reforms. Both absent voices would object to the concentration of extraction in the presidency but have no institutional lever to force their reading.
% DISAPPEARANCE_RATIONALE: If the hyper-presidential constraint vanished overnight, the Fifth Republic would immediately confront its founding problem: how to ensure stable, decisive government without parliamentary fragmentation. The presidency would lose its structural dominance; the Prime Minister and parliament would become the effective executive; cohabitation would become the norm; the constitutional order would reorganize toward a parliamentary or semi-presidential equilibrium. The 1958 settlement would be undone.
% FOUNDING_PROBLEM: The Fourth Republic (1946-1958) suffered chronic government instability — 24 governments in 12 years — due to parliamentary fragmentation, lack of disciplined parties, and the Algerian crisis which the parliamentary system could not resolve. The 1958 Constitution was designed to create a strong executive capable of decisive action and regime stability.
% FOUNDING_PROBLEM_CORROBORATION: The presidency and its supporters (Gaullist tradition, presidential majority parties) attest the founding problem remains live — strong executive prevents paralysis. The parliamentary reading's proponents (Socialist Party pre-1981, some constitutional scholars) and the cohabitation reading's proponents (Chirac/Jospin era observers) attest the problem is dead — stable parliamentary democracies exist (Germany, UK, post-1958 France during cohabitation) and the arrangement persists as presidential rent extraction. The 2000/2002 quinquennat reforms, supported across the political spectrum, implicitly acknowledged the founding problem was solvable without hyper-presidentialism, but the reforms strengthened rather than constrained the presidency.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__hyper_presidential_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constitutional architecture concentrates policy initiative, appointment power, crisis authority, and legislative bypass (49.3) in the presidency. The coordination function exists (decisive executive action, national unity symbol) but is asymmetrically extractive — the legislature pays through structural disempowerment. Suppression (0.68) is substantial but not total: the Assembly retains formal censure power, the Constitutional Council reviews some laws, and cohabitation periods (pre-quinquennat) demonstrated real constraint. Theater ratio (0.42) is moderate: the parliamentary ritual (questions to government, committee work, debates) continues but increasingly performs rather than constrains. Accessibility collapse (0.55) reflects that alternatives (parliamentary systems, cohabitation equilibrium) are known but structurally inaccessible under current electoral calendar. Resistance (0.45) is moderate: opposition uses procedural tools, media, street mobilization, but institutional levers are weak.
 *
 * PERSPECTIVAL GAP:
 *   The presidency seat experiences this as genuine coordination (decisive action, democratic legitimacy from direct suffrage). The legislature seats experience it as enforced extraction (agenda control removed, 49.3 bypass, dissolution threat). The citizen seat is split: direct election of president feels empowering (beneficiary framing) but the resulting legislative weakness reduces accountability (victim framing). The engine computes these divergent per-seat types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Presidency and incumbent president are structural beneficiaries (d near 0.0-0.2): they collect concentrated authority, agenda control, and symbolic capital. National Assembly, Senate, opposition parties are structural victims (d near 0.7-0.9): they bear the cost of disempowerment, constrained exit (cannot leave the constitutional framework), and institutional marginalization. Citizen electorate is near symmetric (d ~0.5): gains direct executive choice but loses parliamentary accountability. Prime Minister is dual-positioned: bears political responsibility (payer) but accesses executive machinery (beneficiary). Conseil Constitutionnel is analytical observer (d=0.5). Exit options differentiate: presidency has arbitrage (can leverage position); legislature is constrained (institutional role fixed); citizens are mobile (can vote, emigrate) but identity-locked to the republic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1958) was regime instability under the Fourth Republic — parliamentary fragmentation, short-lived governments, colonial crisis. The hyper-presidential reading claims this problem remains live (strong executive prevents paralysis). The parliamentary reading claims the problem is dead (stable parliamentary democracies exist) and the arrangement persists as presidential rent extraction. The cohabitation reading claims the problem is contested (cohabitation proved constraint possible). Mandatrophy is unresolved: the 2000/2002 quinquennat reforms (aligning presidential/legislative terms) eliminated the main structural check (cohabitation) without replacing it, suggesting the constraint's current form serves the presidency more than the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the hyper-presidential reading a genuine interpretation of the 1958 Constitution, or a constructed constraint benefiting the presidency as institution?',
    'Comparative analysis of constitutional text (Articles 5, 8, 16, 49.3), historical practice since 1958, and Conseil Constitutionnel jurisprudence on presidential vs. parliamentary powers. Cross-kernel validation with sibling readings'' structural profiles.',
    'If constructed, the constraint is a false summit mountain or extractive tangled rope with identifiable beneficiaries (presidency). If genuine interpretation, extraction may be lower and coordination function stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the reading''s beneficiary structure reveals constructedness or genuine constitutional design').

omega_variable(
    article_49_3_extraction_measurement,
    'How much of the measured extractiveness comes specifically from Article 49.3 (government responsibility) invocations versus the constitutional structure as a whole?',
    'Historical counting of 49.3 uses by government, survival rates of governments after invocation, and policy outcomes enacted without vote vs. with vote. Compare pre-2008 (unlimited 49.3) and post-2008 (limited to one per session except budget) regimes.',
    'If 49.3 accounts for most extraction, the constraint is modular — reform of 49.3 could substantially reduce χ without constitutional replacement. If extraction is structural, reform requires deeper change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_49_3_extraction_measurement, empirical, 'Attribution of extraction to specific constitutional mechanisms vs. systemic design').

omega_variable(
    cohabitation_as_discipline_mechanism,
    'Does the cohabitation equilibrium reading function as a genuine constraint on hyper-presidential extraction, or is cohabitation itself a product of the same constitutional structure?',
    'Analysis of the three cohabitation periods (1986-88, 1993-95, 1997-2002): policy outcomes, presidential vs. prime ministerial initiative, and whether cohabitation reduced or merely redistributed extraction. Test against the 2000/2002 electoral calendar reforms (quinquennat) that made cohabitation structurally unlikely.',
    'If cohabitation was a genuine discipline mechanism, its elimination via quinquennat increased systemic extraction. If cohabitation was theater, extraction was stable across periods.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cohabitation_as_discipline_mechanism, conceptual, 'Whether alternating executive arrangements genuinely constrain extraction or merely perform constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fifth_republic_hyper_pres_tr_t1958, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1958, 0.2).
narrative_ontology:measurement(fifth_republic_hyper_pres_tr_t1962, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1962, 0.25).
narrative_ontology:measurement(fifth_republic_hyper_pres_tr_t1974, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1974, 0.22).
narrative_ontology:measurement(fifth_republic_hyper_pres_tr_t1981, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1981, 0.3).
narrative_ontology:measurement(fifth_republic_hyper_pres_tr_t1986, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1986, 0.35).
narrative_ontology:measurement(fifth_republic_hyper_pres_tr_t1993, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1993, 0.38).
narrative_ontology:measurement(fifth_republic_hyper_pres_tr_t1997, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1997, 0.36).
narrative_ontology:measurement(fifth_republic_hyper_pres_tr_t2002, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2002, 0.4).
narrative_ontology:measurement(fifth_republic_hyper_pres_tr_t2008, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2008, 0.42).
narrative_ontology:measurement(fifth_republic_hyper_pres_tr_t2017, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2017, 0.41).
narrative_ontology:measurement(fifth_republic_hyper_pres_tr_t2024, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(fifth_republic_hyper_pres_be_t1958, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1958, 0.48).
narrative_ontology:measurement(fifth_republic_hyper_pres_be_t1962, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1962, 0.52).
narrative_ontology:measurement(fifth_republic_hyper_pres_be_t1974, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1974, 0.5).
narrative_ontology:measurement(fifth_republic_hyper_pres_be_t1981, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1981, 0.55).
narrative_ontology:measurement(fifth_republic_hyper_pres_be_t1986, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1986, 0.45).
narrative_ontology:measurement(fifth_republic_hyper_pres_be_t1993, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1993, 0.43).
narrative_ontology:measurement(fifth_republic_hyper_pres_be_t1997, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1997, 0.44).
narrative_ontology:measurement(fifth_republic_hyper_pres_be_t2002, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2002, 0.62).
narrative_ontology:measurement(fifth_republic_hyper_pres_be_t2008, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2008, 0.65).
narrative_ontology:measurement(fifth_republic_hyper_pres_be_t2017, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2017, 0.7).
narrative_ontology:measurement(fifth_republic_hyper_pres_be_t2024, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(fifth_republic_hyper_pres_su_t1958, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1958, 0.55).
narrative_ontology:measurement(fifth_republic_hyper_pres_su_t1962, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1962, 0.6).
narrative_ontology:measurement(fifth_republic_hyper_pres_su_t1974, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1974, 0.58).
narrative_ontology:measurement(fifth_republic_hyper_pres_su_t1981, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1981, 0.62).
narrative_ontology:measurement(fifth_republic_hyper_pres_su_t1986, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1986, 0.5).
narrative_ontology:measurement(fifth_republic_hyper_pres_su_t1993, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1993, 0.48).
narrative_ontology:measurement(fifth_republic_hyper_pres_su_t1997, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1997, 0.52).
narrative_ontology:measurement(fifth_republic_hyper_pres_su_t2002, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2002, 0.65).
narrative_ontology:measurement(fifth_republic_hyper_pres_su_t2008, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2008, 0.67).
narrative_ontology:measurement(fifth_republic_hyper_pres_su_t2017, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2017, 0.68).
narrative_ontology:measurement(fifth_republic_hyper_pres_su_t2024, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__hyper_presidential_reading, 0.12).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, french_electoral_system_quinquennat).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, conseil_constitutionnel_jurisdiction).

% DUAL FORMULATION NOTE:
% The Fifth Republic Constitution kernel decomposes into three structurally distinct readings with different ε values and beneficiary/victim structures. This hyper-presidential reading has high extraction (0.72) and identifiable beneficiaries (presidency). The parliamentary reading would show lower extraction and stronger coordination. The cohabitation reading would show cyclical extraction correlated with electoral alignment. All three are linked via network.affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__hyper_presidential_reading, institutional, 0.15).
constraint_indexing:directionality_override(fifth_republic_constitution__hyper_presidential_reading, organized, 0.75).
constraint_indexing:directionality_override(fifth_republic_constitution__hyper_presidential_reading, powerless, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
