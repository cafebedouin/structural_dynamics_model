% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Fifth Republic Constitution: Hyper-Presidential Reading
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   The Fifth Republic Constitution (1958) established a bicephalous
 *   executive: president and prime minister. The hyper-presidential reading
 *   interprets Article 4 and Article 8 to mean the president embodies the
 *   national will and can govern with minimal legislative constraint, while
 *   the competing readings (parliamentary_constraint_reading,
 *   cohabitation_equilibrium_reading) treat the legislature and prime
 *   minister as co-equal deliberative partners. This story instantiates only
 *   the hyper-presidential reading: a single, coherent account of how the
 *   Constitution legitimates executive authority and what extracted from whom
 *   under this framing. The reading's core claim is that the people's direct
 *   election of the president creates an authority that supersedes
 *   legislative deliberation; legislators enter the victim set when they
 *   obstruct this direct presidential mandate. The measurement series tracks
 *   how the reading's extractiveness and theater ratio accumulated over 66
 *   years, particularly through the presidency of de Gaulle (who authored the
 *   reading), Chirac, and Macron.
 *
 * KEY AGENTS:
 *   - presidency_as_institution: sets constitutional interpretation, claims direct mandate from electorate
 *   - incumbent_president: exercises unilateral authority via Articles 49.3, 16, referendum power
 *   - national_assembly: nominally legislative, structurally subordinate; pays through loss of function
 *   - legislative_minority: pays through constrained exit options and dissolution threat
 *   - constitutional_court: pays through undermined review authority when president controls legitimacy narrative
 *   - electorate: receives direct appeals, benefits from referendum possibility, pays through legislative subordination
 *   - parliamentary_opposition_reading: excluded from apparatus; held by reformers and scholars
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.78).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.71).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Constitution: Hyper-Presidential Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, 'ce2704a1-ddc4-495e-bf35-28cc99034f7c').
narrative_ontology:cs_kernel_codification('ce2704a1-ddc4-495e-bf35-28cc99034f7c', fixed_text).
narrative_ontology:cs_authority_grounding('ce2704a1-ddc4-495e-bf35-28cc99034f7c', lineage).
narrative_ontology:cs_interpretation_layer_present('ce2704a1-ddc4-495e-bf35-28cc99034f7c').
narrative_ontology:cs_reading_relation('ce2704a1-ddc4-495e-bf35-28cc99034f7c', fifth_republic_constitution__parliamentary_constraint_reading, forecloses).
narrative_ontology:cs_reading_relation('ce2704a1-ddc4-495e-bf35-28cc99034f7c', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('ce2704a1-ddc4-495e-bf35-28cc99034f7c', foundational, presidency_embodies_general_will).
narrative_ontology:cs_axiom_status(presidency_embodies_general_will, holdable).
narrative_ontology:cs_axiom_grounding('ce2704a1-ddc4-495e-bf35-28cc99034f7c', presidency_embodies_general_will, deontological).
narrative_ontology:cs_axiom('ce2704a1-ddc4-495e-bf35-28cc99034f7c', foundational, legislature_is_subordinate_to_executive_mandate).
narrative_ontology:cs_axiom_status(legislature_is_subordinate_to_executive_mandate, holdable).
narrative_ontology:cs_axiom_grounding('ce2704a1-ddc4-495e-bf35-28cc99034f7c', legislature_is_subordinate_to_executive_mandate, deontological).
narrative_ontology:cs_reference_frame('ce2704a1-ddc4-495e-bf35-28cc99034f7c', de_gaulle_presidency_authority).
narrative_ontology:cs_drift_state('ce2704a1-ddc4-495e-bf35-28cc99034f7c', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ce2704a1-ddc4-495e-bf35-28cc99034f7c', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, legislative_minority).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, constitutional_court).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, electorate_under_presidential_appeal).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, prime_minister).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, prime_minister).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, general_will_embodied_in_executive).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, presidential_mandate_legitimacy).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__hyper_presidential_reading, state_continuity_through_presidency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The office of the president, across all incumbents, sets the constitutional interpretation through its exercise of Article 4 arbitration, Article 8 government control, Article 49.3 confidence vote authority, Article 12 dissolution power, and Article 16 emergency invocation. The institution claims to embody the national will and defends its interpretation through invoking de Gaulle's founding vision, electoral mandates, and state capacity narratives. Persists by controlling the terms on which the Constitution is understood: Article 4 is read to mean 'the president arbitrates as the embodiment of the general will,' not 'the president arbitrates between equal branches.'
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution, agenda_setter,
    institutional, generational, analytical, national).

% The sitting president, elected directly, exercises the full scope of the hyper-presidential reading: can initiate confidence votes (Article 49.1) that force the legislature to accept government bills or face dissolution (Article 49.2); can call referendums on constitutional or policy questions (Article 11), framing them as direct appeals over legislative obstruction; can invoke Article 16 emergency powers when claiming the state faces peril; appoints the constitutional court (Article 56, 9-year terms, renewable one-third per 3-year cycle); controls the government apparatus through the prime minister (Article 8). The incumbent collects gains directly: authority to govern without legislative authorization, ability to bypass legislative deliberation, power to reshape institutional relationships through referendum and court appointment.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter).

% Nominally the legislative body but structurally subordinate under the hyper-presidential reading. Cannot initiate confidence votes against a government (Article 49.1 is government prerogative); faces dissolution if it votes no confidence (Article 12 dissolution triggered by lost confidence); loses effective legislative authority when the president invokes Article 49.3 (government bills pass without legislative vote); loses budgetary authority under Article 16 emergency decrees. Members pay through: loss of legislative function (bills pass or fail at executive discretion), institutional subordination (treated as obstacles rather than deliberative partners), and electoral jeopardy (dissolution at presidential discretion). The assembly must defer to executive priorities to avoid dissolution, which means it refrains from asserting legislative authority even when it possesses numerical capacity to block bills.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    organized, biographical, constrained, national).

% Lacks the numbers to prevent executive dominance. Cannot form a confidence-voting coalition (Article 49 requires the government to survive a confidence vote, not the minority to initiate one). Faces dissolution if it joins a majority blocking executive bills (because the president can dissolve the assembly and call new elections, framing the minority's obstruction as forcing the dissolution). Exit options are constrained to: accept executive priority, call for public mobilization outside the legislature, or resign from a legislature whose power is subordinated. Pays through: inability to enforce co-equal deliberation, vulnerability to dissolution, and loss of legislative leverage relative to legislative majorities that can negotiate with the executive.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, legislative_minority, payer,
    moderate, biographical, constrained, national).

% Under the hyper-presidential reading, the court's review authority is hollowed. Can rule laws unconstitutional (Article 61), but the president can invoke Article 16 to bypass judicial review in emergencies, can call referendums to overturn court rulings by rewriting the Constitution, and controls the court's membership (Article 56: the president appoints three members, the Assembly president appoints three, the Senate president appoints three, with nine-year staggered terms). The court pays through: institutional independence eroded, rulings overridable by presidential referendum, and membership dependence on presidential appointment. When the court issues a ruling the president opposes, the president can reframe it as an obstacle to the general will and appeal directly to the electorate via referendum.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_court, payer,
    institutional, generational, constrained, national).

% Under the hyper-presidential reading, the electorate is positioned as the ultimate sovereign, appealed to directly by the president through referendums and general election mandates. Citizens benefit from: direct democratic ratification of constitutional changes (Article 11 referendums), the possibility of rejecting an executive they no longer support (presidential elections every 5 years post-2000), and the symbolic claim that the president acts for their will, not for institutional interests. Citizens pay through: subordination of legislative representation (the National Assembly's power is reduced relative to the presidency, so legislative constituencies lose deliberative weight), difficulty in forming legislative coalitions to constrain the president (because the president can dissolve the assembly and reframe the electorate's electoral choice as endorsement of presidential priorities), and loss of institutions that provide deliberative resistance to executive unilateralism.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, electorate_under_presidential_appeal, beneficiary,
    powerful, biographical, mobile, national).

% The political and constitutional reading that the Fifth Republic Constitution creates a co-equal legislature requiring presidential deference to legislative majority will on policy matters. Held by parliamentarians, constitutional scholars (including some founding-era interpreters like Debré who initially imagined prime ministerial authority as more robust), and democratic reformers. Excluded from the institutional apparatus under the hyper-presidential reading because that reading's logic treats legislative constraint as illegitimate obstruction of the general will and reframes legislators asserting co-equal power as fragments that must be overcome.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, parliamentary_opposition_reading, excluded,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_non_agent(fifth_republic_constitution__hyper_presidential_reading, parliamentary_opposition_reading).

% Experts who analyze the Constitution's text and France's constitutional practice. Report that the 1958 Constitution is genuinely ambiguous on the scope of presidential authority; that the hyper-presidential reading is one coherent interpretation (consistent with de Gaulle's intentions and practicing presidents' invocations), but the parliamentary reading and cohabitation reading are also consistent with the text; and that the hyper-presidential reading's dominance is a political fact (periods when presidents had supportive legislatures) rather than a constitutional mandate. Note that cohabitation periods (1986–1988, 1993–1995, 1997–2002) demonstrate the Constitution can operate under alternative readings; the hyper-presidential reading is not the only defensible one.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_scholars_pluralist, observer,
    analytical, generational, analytical, national).

% Under the hyper-presidential reading, the prime minister is the president's agent, not a co-equal executive. Nominally heads the government (Article 8), but serves at presidential pleasure and must implement presidential policy without legislative negotiation authority. Benefits from executive status and the ability to govern without legislative deliberation on trivial matters. Pays through: subordination to presidential direction, vulnerability to removal (Article 8: the president can dismiss the prime minister unilaterally), and responsibility for legislative failures when the president's priorities prove unpopular. In cohabitation periods, this dynamic inverts: the prime minister becomes a constrained agent of the president's opposition coalition. The prime minister's structural position under the hyper-presidential reading is thus one of dependent authority: can exercise executive power only as long as the president consents.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, prime_minister, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__hyper_presidential_reading, prime_minister, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__hyper_presidential_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates policy authority in a single elected institution (the presidency) that is directly accountable to the entire national electorate, solving the coordination problem of legislative fragmentation by enabling rapid policy execution and bypassing slow deliberative cycles. The reading claims this solves the Fourth Republic's immobilism problem.
% TRANSFER_FUNCTION: Transfers legislative deliberative authority from the National Assembly (577 members, coalition-based, slow deliberation) to the presidency (one elected officer, direct mandate, rapid decision-making). Moves power over policy initiation, legislative passage, budget authority, court appointments, and constitutional amendment from parliament to the executive.
% ABSENT_VOICES: Parliamentarians and constitutional scholars who defend legislative co-equality; constitutional courts and independent institutions claiming review authority; regional governments and minority interests requiring legislative negotiation; workers' councils and civil-society organizations that would prefer distributed deliberation to centralized presidential decree. These voices are excluded from the institutional apparatus by the reading's own logic: it treats them as obstacles to the general will rather than as legitimate participants in a co-equal deliberative system.
% DISAPPEARANCE_RATIONALE: If the hyper-presidential reading and its enforcement mechanisms (Article 49.3 confidence-vote dominance, Article 16 emergency decree authority, Article 12 dissolution power, Article 11 referendum control) were to collapse or be reinterpreted, the legislature would reclaim deliberative authority, policy would require legislative majorities and negotiation, emergency powers would face judicial review, referendums would require parliamentary authorization, and the executive would operate as the prime minister's government subject to legislative consent. The state apparatus would reorganize around cabinet government and legislative co-equality rather than presidential unilateralism.
% FOUNDING_PROBLEM: The Fourth Republic (1947–1958) suffered from acute governmental immobilism: a fragmented legislature with multiple small parties, unstable coalitions, weak executives dependent on shifting parliamentary majorities, inability to make decisive policy on colonial crises (Indochina, Algeria), and repeated government collapse (26 governments in 12 years). The founding problem was loss of state capacity and effective governance.
% FOUNDING_PROBLEM_CORROBORATION: The presidency attests the founding problem remains live: legislative obstruction is an ongoing threat, and hyper-presidential authority is necessary to maintain state capacity. Constitutional scholars, parliamentarians, and international observers attest the founding problem is solved: the Fifth Republic has not experienced a government collapse due to legislative paralysis in 66 years; the state maintained capacity through the Cold War, European integration, decolonization, and economic transitions. The founding problem (immobilism, governmental instability) was real in 1958 and is substantially addressed. However, the hyper-presidential reading persists and extracts MORE authority than the founding problem required, suggesting mandatrophy: the reading was born from crisis (real coordination function) but persists beyond the crisis, using the founding problem as justification for extracting authority that now serves president-centered interests rather than state-capacity needs. Cohabitation periods (when the reading was not invoked despite legislative opposition) demonstrate the Constitution can operate without hyper-presidential authority, yet the reading re-emerges when legislators are favorable to the president, suggesting the reading's persistence is contingent on political alignment, not constitutional necessity.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__hyper_presidential_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.78) because the reading gives the presidency authority over policy, emergency powers, referendum control, and legislature dissolution without legislative authorization thresholds — the breadth of extractive capacity is structural. Suppression is substantial (0.71) because the reading requires active enforcement: legislators must be deterred from asserting co-equal authority (Article 49.3 votes function as confidence votes the president initiates, not parliament), referendums must be framed as direct democracy overriding legislative obstruction, and the constitutional court must be prevented from asserting independent review. Theater is moderate (0.42): the reading genuinely coordinates rapid policy execution (a real coordination function), but an increasing share of enforcement activity defends presidential unilateralism against legislative reclamation rather than solving the original immobilism problem. The measurement trajectory shows extractiveness rising from 0.35 (1958, when the reading competed with others and faced institutional resistance) to 0.78 (2024, when the reading's institutional dominance is consolidated). Theater rising from 0.15 to 0.42 suggests growing performative maintenance: the founding problem (immobilism) recedes as a live threat; the reading now performs state capacity rather than solving a crisis.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces stark per-seat divergence. From the presidential seat, the extraction is invisible: the seat experiences only the coordination gain (state capacity recovered, policy execution accelerated). From the legislative seat, the extraction is stark: legislators experience loss of authority, dissolution threat, and institutional subordination. The engine computes this divergence; it is not tuned or predicted, it is measured from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The presidency embodies the beneficiary position: it collects authority over policy, emergency powers, referendum control, and legislature dissolution. Exit options are mobile (the presidency can be vacated by incumbent at end of term, but the institution persists across presidents). The legislature embodies the payer position: it must defer to executive priorities, faces dissolution if it opposes, and loses budgetary authority under Articles 49.3 and 16. Exit options are constrained (legislators must function within a hollowed legislative chamber or resign). The constitutional court pays through eroded review authority; its exit options are constrained by its appointment dependence. The electorate under presidential appeal benefits from direct democracy possibilities but pays through legislative subordination. Directionality derivation yields: presidency d ≈ 0.15 (beneficiary); legislature d ≈ 0.85 (target); court d ≈ 0.72 (target); electorate d ≈ 0.50 (symmetric: coordination gain + subordination cost).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Fourth Republic immobilism) has receded from a live threat (1958–1970s) to a historical memory (2000–2024). Yet the institutional arrangement that addressed it persists with increasing extractiveness (0.35 → 0.78) and theater ratio (0.15 → 0.42). This pattern is exactly mandatrophy: the founding mandate (restore state capacity) has outlived its function (the state is stable, legislatures do not collapse), but the constraint persists and extracts more, not less. The measurement trajectory shows extractiveness accumulating while the coordinating function stagnates — new uses for Article 49.3 and Article 16 emerge (constitutional amendment bypass, referendum manipulation) that are detached from the original immobilism problem. The hyper-presidential reading thus exhibits mandatrophy: it was born from a real crisis and provided real coordination, but the coordination mandate has died while the extraction machinery remains and grows. The Tangled Rope claim captures this: genuine founding coordination + accumulated extraction + active enforcement to maintain presidency's structural advantage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    general_will_authority_source,
    'Does the president''s constitutional authority derive from direct election by the people (general will) or from delegation by the Constitution (a distributed institution)? Are these sources compatible?',
    'Constitutional court ruling on the status of Article 4 relative to Articles 3 (national sovereignty) and 24 (legislative power); or comparative analysis of how peer democracies (Germany, Spain, Italy) distribute the ''will of the people'' between elected branches.',
    'If authority derives from constitutional distribution, the legislature is a co-equal receiver of the same ''general will''; if from direct election, the legislature must defer. This resolution determines whether the hyper-presidential reading or the parliamentary reading is structurally justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(general_will_authority_source, conceptual, 'The contested basis of executive authority in the Fifth Republic Constitution.').

omega_variable(
    article_49_3_legitimacy,
    'Is Article 49.3 (government confidence vote override) a mechanism for executive-legislative coordination or a tool for suppressing legislative deliberation?',
    'Empirical analysis of Article 49.3 use: frequency, outcomes, legislative response patterns, and correlation with legislative cohesion and policy efficacy. Comparative analysis with peer democracies'' confidence-vote mechanisms.',
    'If used frequently to force passage over legislative opposition (not merely to formalize consensus), it is an extraction tool; if used rarely as a last resort, it remains coordinating. The hyper-presidential reading predicts high use for enforcement; the parliamentary reading predicts low use for consensus-building.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_49_3_legitimacy, empirical, 'Whether Article 49.3 functions as coordination or coercion.').

omega_variable(
    cohabitation_reading_persistence,
    'Why does cohabitation (shared executive authority between a president and prime minister from opposing coalitions) recur as a lived practice (1986–1988, 1993–1995, 1997–2002) despite the hyper-presidential reading''s claims of constitutional incompatibility?',
    'Historical analysis of cohabitation periods: what forced the accommodation, how was authority actually allocated, and what does the fact of cohabitation''s recurrence (not merely tolerance but functional necessity) imply about whether the hyper-presidential reading accurately describes the Constitution?',
    'If cohabitation is a structural possibility even when a president opposes it (as occurred 1986–1988), the hyper-presidential reading''s claim to embody unilateral authority is empirically false — the Constitution permits co-equal executive authority. This would downgrade the hyper-presidential reading from a coherent interpretation to a contingent political outcome dependent on having a supportive legislature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_reading_persistence, empirical, 'Whether cohabitation proves the hyper-presidential reading is not constitutionally mandated.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Does the legislature''s subordination to the hyper-presidential reading rest on structural barriers (Article 49.3, dissolution threat, appointment control) or on internalized legislative deference (belief that the president truly embodies the general will)?',
    'Post-cohabitation behavior: if a legislature from an opposition coalition asserts co-equal authority despite the hyper-presidential reading, and the president accommodates rather than invokes Article 49.3, the suppression is structural. If the legislature refrains from asserting authority even when empowered to do so, the suppression includes internalized deference.',
    'Structural suppression is more fragile (depends on enforcement machinery and electoral outcomes); internalized suppression is more durable (persists after barriers are removed) but indicates the constraint''s narrative (general will embodied in presidency) has captured legislative actors'' self-concept. Either way, the constraint meets the Tangled Rope definition (real coordination + asymmetric extraction + active enforcement), but the mechanism determines policy vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'The mechanism sustaining legislative subordination to the hyper-presidential reading.').

omega_variable(
    reading_as_political_artifact,
    'Is the hyper-presidential reading a defensible interpretation of the 1958 Constitution, or is it a political artifact created by de Gaulle''s dominance (1958–1969) and sustained by contingent legislative coalitions?',
    'Constitutional text analysis comparing the hyper-presidential, parliamentary, and cohabitation readings against the Constitution''s explicit articles and implicit structure; expert consensus among constitutional scholars outside the French executive; and comparison to how peer democracies interpret similar constitutional provisions.',
    'If the reading is a defensible interpretation, multiple readings can coexist and the classification of which reading applies is politically contingent. If the reading is a political artifact with thin constitutional basis, it should be classified as extraction dressed in constitutional language — a false summit: appears as natural constitutional law but is a constructed constraint benefiting the presidency. The corpus''s measurement of this constraint will contribute data to the false-summit detection system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_as_political_artifact, conceptual, 'Whether the hyper-presidential reading is constitutional or contingent political practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1958, 0.15).
narrative_ontology:measurement_basis(fift_tr_t1958, observed).
narrative_ontology:measurement(fift_tr_t1974, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1974, 0.22).
narrative_ontology:measurement_basis(fift_tr_t1974, observed).
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 1986, 0.28).
narrative_ontology:measurement_basis(fift_tr_t1986, observed).
narrative_ontology:measurement(fift_tr_t2000, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement_basis(fift_tr_t2000, observed).
narrative_ontology:measurement(fift_tr_t2012, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2012, 0.4).
narrative_ontology:measurement_basis(fift_tr_t2012, observed).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(fift_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1958, 0.35).
narrative_ontology:measurement_basis(fift_be_t1958, observed).
narrative_ontology:measurement(fift_be_t1974, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1974, 0.52).
narrative_ontology:measurement_basis(fift_be_t1974, observed).
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 1986, 0.61).
narrative_ontology:measurement_basis(fift_be_t1986, observed).
narrative_ontology:measurement(fift_be_t2000, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement_basis(fift_be_t2000, observed).
narrative_ontology:measurement(fift_be_t2012, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2012, 0.76).
narrative_ontology:measurement_basis(fift_be_t2012, observed).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 2024, 0.78).
narrative_ontology:measurement_basis(fift_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1958, 0.41).
narrative_ontology:measurement_basis(fift_su_t1958, observed).
narrative_ontology:measurement(fift_su_t1974, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1974, 0.54).
narrative_ontology:measurement_basis(fift_su_t1974, observed).
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 1986, 0.62).
narrative_ontology:measurement_basis(fift_su_t1986, observed).
narrative_ontology:measurement(fift_su_t2000, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement_basis(fift_su_t2000, observed).
narrative_ontology:measurement(fift_su_t2012, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2012, 0.7).
narrative_ontology:measurement_basis(fift_su_t2012, observed).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(fift_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__hyper_presidential_reading, 0.22).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This story instantiates one reading of the Fifth Republic Constitution kernel. The sibling constraints (parliamentary_constraint_reading, cohabitation_equilibrium_reading) represent alternative coherent interpretations of the same constitutional text. The readings differ on whether Article 4 and Article 8 create a hierarchical executive (hyper-presidential) or co-equal branches. The hyper-presidential reading (this story) claims the president's authority derives from direct election and embodies the national will; the legislature enters the victim set when it obstructs this mandate. The parliamentary reading claims the president requires legislative authorization for policy; the legislature is a co-equal partner. The cohabitation reading claims the Constitution permits shared executive authority between president and prime minister, forcing negotiated allocation. All three readings are linguistically defensible from the 1958 text; the readings' relative dominance is politically contingent, not constitutionally determined. This story models the hyper-presidential reading's claimed structural advantages (direct mandate, unilateral authority) and their cost (legislative subordination, institutional extraction). Siblings model the alternative framings' structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__hyper_presidential_reading, organized, 0.81).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
