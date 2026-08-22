% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Legislative Sovereignty over Constitutional Text
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the legislative sovereignty reading of
 *   contested constitutional authority: the constitutional text is
 *   interpreted to grant parliament ultimate authority to determine
 *   constitutional meaning, with courts offering advisory interpretation that
 *   the legislature can override through notwithstanding clauses, statutory
 *   amendment, or simple re-passage. This reading privileges democratic
 *   accountability over judicial independence and individual rights
 *   protection. It is one of three structurally distinct readings of the same
 *   kernel (constitutional text); the sibling readings — judicial supremacy
 *   and popular sovereignty — would produce different victim/beneficiary
 *   structures and different classifications. This story instantiates only
 *   the legislative sovereignty reading's constraint.
 *
 * KEY AGENTS:
 *   - Majoritarian legislature: ultimate authority holder; sets constitutional meaning
 *   - Judiciary: advisory interpreter; identity-locked subordination
 *   - Minority rights claimants: trapped targets of majoritarian override
 *   - Majoritarian voters: electoral beneficiaries of supremacy structure
 *   - Courts elsewhere: analytical observers representing alternative readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.68).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.52).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Legislative Sovereignty over Constitutional Text").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional/political").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, '44dba27c-8965-4737-abef-f787fc817fe6').
narrative_ontology:cs_kernel_codification('44dba27c-8965-4737-abef-f787fc817fe6', fixed_text).
narrative_ontology:cs_authority_grounding('44dba27c-8965-4737-abef-f787fc817fe6', extraction).
narrative_ontology:cs_interpretation_layer_present('44dba27c-8965-4737-abef-f787fc817fe6').
narrative_ontology:cs_reading_relation('44dba27c-8965-4737-abef-f787fc817fe6', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('44dba27c-8965-4737-abef-f787fc817fe6', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('44dba27c-8965-4737-abef-f787fc817fe6', foundational, parliament_final_constitutional_arbiter).
narrative_ontology:cs_axiom_status(parliament_final_constitutional_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('44dba27c-8965-4737-abef-f787fc817fe6', parliament_final_constitutional_arbiter, conventional).
narrative_ontology:cs_axiom('44dba27c-8965-4737-abef-f787fc817fe6', foundational, electoral_accountability_legitimates_supremacy).
narrative_ontology:cs_axiom_status(electoral_accountability_legitimates_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('44dba27c-8965-4737-abef-f787fc817fe6', electoral_accountability_legitimates_supremacy, deontological).
narrative_ontology:cs_reference_frame('44dba27c-8965-4737-abef-f787fc817fe6', parliamentary_sovereignty_doctrine).
narrative_ontology:cs_drift_state('44dba27c-8965-4737-abef-f787fc817fe6', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('44dba27c-8965-4737-abef-f787fc817fe6', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, majoritarian_legislature).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_rights_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, elected_majoritarian_voters).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, non_majoritarian_voters).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, parliamentary_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate interpretive authority over constitutional meaning through statutory override, notwithstanding clauses, or simple legislative re-passage. Sets the political agenda for what the constitution permits. Defends this authority as grounding democratic legitimacy in the elected body accountable to voters. Collects the rents of sovereignty: the ability to re-interpret the constitutional text without judicial consent.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, majoritarian_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Offers formal constitutional review — it interprets the text, develops doctrine, and issues advisory opinions. Its interpretations carry persuasive weight but no final authority; the legislature can reverse them. Benefits from exercising interpretive authority and articulating constitutional principles. Pays by accepting subordination: the institution's pronouncements are advisory unless the legislature chooses to defer, and this subordination is built into the reading's foundation.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, judiciary, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, judiciary, payer).

% Depend on courts to protect their rights against majoritarian legislation. Under this reading, if the legislature disagrees with a court's interpretation protecting a minority right, it can override that interpretation through notwithstanding clause or statutory re-passage. They are trapped: unable to exit the jurisdiction, lacking electoral power to reverse majoritarian control, and dependent on an institution (the judiciary) whose authority is subordinate to the group that is harming them.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, minority_rights_claimants, payer,
    powerless, biographical, trapped, national).

% As voters, they elect the legislature that holds ultimate constitutional authority. This reading vindicates their electoral power: the constituency that wins elections gets to determine what the constitution means. They also carry subordinate cost: they are bound by the constitutional text's constraints (they cannot vote to abolish rights that are clearly textually grounded), but they experience this as legitimacy rather than extraction because electoral accountability flows through their representatives.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, elected_majoritarian_voters, beneficiary,
    organized, biographical, constrained, national).

% Voters not in the electoral majority. They depend on courts to block majoritarian legislation from harming them. But under this reading, courts are subordinate and the majority can override judicial protection through legislative action. They pay through reduced exit options and reliance on an institution with no final authority.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, non_majoritarian_voters, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, non_majoritarian_voters, excluded).

% Observe and sometimes influence the debate through comparative analysis. Represent alternative readings (judicial supremacy, popular sovereignty). They demonstrate that the legislative sovereignty reading is contestable — other institutional frameworks exist.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, courts_in_other_jurisdictions, observer,
    institutional, generational, analytical, continental).

% Study and critique the reading's coherence, historical grounding, and implications. Their analysis can shift the frame but does not alter the reading's operation in any single jurisdiction that has adopted it.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__legislative_sovereignty_reading, majoritarian_legislature).
narrative_ontology:fixing_cost_class(constitutional_text__legislative_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the question of final constitutional authority by designating a single institution (the elected legislature) as the ultimate arbiter. This enables democratic legitimacy to flow through electoral accountability rather than through an unelected judiciary. It avoids deadlock between coordinate branches by giving the legislature a clear trump card.
% TRANSFER_FUNCTION: Moves interpretive authority from courts to the elected legislature. Courts retain advisory power and prestige but lose the ability to bind political outcomes; the legislature retains the ability to override any court decision through statutory amendment or notwithstanding clause. Non-majoritarian voters lose the protection that judicial independence would provide; majoritarian voters gain the assurance that their elected representatives hold ultimate constitutional say.
% ABSENT_VOICES: Minority groups who cannot form a legislative majority, judicial officers who would prefer coordinate authority, and proponents of popular sovereignty (who would argue neither courts nor legislature should be supreme) are excluded from the core reading's framework. Their objection is that the reading entrenches majoritarian power without structural safeguards for rights.
% DISAPPEARANCE_RATIONALE: If this reading vanished and courts held supreme interpretive authority instead, the constitutional landscape would reorganize: minority protections currently reversible by the legislature would become judicially entrenched; legislators would face a binding check on their power. If instead popular sovereignty reading took hold, the constitutional authority structure would shift to emphasize amendment and constituent processes. The constraint's disappearance would alter the distribution of constitutional power.
% FOUNDING_PROBLEM: How can a constitutional text be interpreted and applied without creating either (a) tyranny of an unelected judiciary, or (b) tyranny of a majoritarian legislature unchecked by any guardian of rights? The legislative sovereignty reading solves this by preferring democratic legitimacy over rights protection — the problem it solves is the problem of ultimate authority when two coordinate branches disagree.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of parliamentary supremacy and some democracies (Canada with notwithstanding clauses, some Westminster systems) affirm this reading solves the authority problem by privileging electoral accountability. Proponents of judicial supremacy and international human rights bodies argue the founding problem requires courts to maintain authority over rights; they attest the reading shifts the problem rather than solving it — it trades tyranny of courts for tyranny of majorities. No uncontested external corroboration exists; the corroboration divides along the same reading boundary.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.68 reflects the reading's core structure: the majority extracts the benefit of ultimate constitutional authority; minorities lose protection against majoritarian reinterpretation. The measurement series shows modest growth early (0.55 to 0.65 in the first 15 time units) as the reading becomes more operationally entrenched in practice, then plateaus (0.68) once the supremacy structure is fully normalized. Theater rises from 0.25 to 0.41 and stabilizes, indicating an increasing share of legislative activity is performative constitutional deference (courts are invited to opine; the legislature performs consideration; then it overrides) rather than genuine interinstitutional dialogue. Suppression grows from 0.38 to 0.52: the constraint requires ongoing suppression of judicial claims to coordinate authority and of minoritarian constitutional claims, but this suppression is moderate because the legislature's electoral accountability provides a legitimacy cover that reduces naked coercion. The three metrics on one shared time grid: all are authored at every time point so the engine can detect coordinated drift or misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the majoritarian legislature's seat, this constraint is genuine democratic coordination — the electorate spoke, the representatives deliver, judicial advice is solicited and considered but ultimate authority stays with the people's representatives. From the minority seat, the same constraint is pure extraction — the majority has rigged the constitutional interpretation process to entrench its power. From the judiciary's seat, it is identity-locked subordination: the court accepts advisory status as legitimate because that is what its reading of the text demands, but this acceptance is exactly what the majority relies on. The engine computes these divergences from the stakeholder structure; the claim-type (tangled_rope) and metrics (high extractiveness, active enforcement) describe the constraint as observed across all seats, not as any one seat experiences it.
 *
 * DIRECTIONALITY LOGIC:
 *   The majoritarian legislature sits at d near 1.0 (full beneficiary, structurally vindicated, low exit cost — it is the seat the constraint is built to protect). Minority rights claimants sit at d near 1.0 (full target, trapped, powerless, their protection mechanism is subordinate). Non-majoritarian voters sit at intermediate d (constrained exit, moderate power, they can form coalitions but face a structural disadvantage). Judiciary is near d=0.5 (symmetric): it gains prestige and the exercise of interpretive authority but loses coordinate power; it is identity-locked in subordination but accepts that lock as the reading's foundation. The directionality reflects the core asymmetry: this reading is built to serve electoral majorities and to constrain courts; it is therefore extractive toward anyone not in the majority.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy classification because the founding problem (ultimate constitutional authority) remains live and contested. The legislature continues to use its supremacy actively: parliaments override court decisions, amend constitutions to entrench their authority, and invoke notwithstanding clauses. However, the measurement series shows theater rising toward the asymptote (0.41 at endpoint), suggesting growing performative elements: legislatures hold formal consultation processes with courts, appear to defer and reconsider, then pass the same measures. This is not yet full mandatrophy (which would show theater>0.6 and extractiveness declining as the supremacy structure becomes theater), but it signals incipient ritualization — the constraint is hardening into a performance of consultation without genuine influence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_vs_reading_ambiguity,
    'Does the constitutional text actually establish legislative supremacy, or does the legislative sovereignty reading impose a particular interpretive frame onto ambiguous text?',
    'Comparative textual analysis across constitutions with explicit parliamentary supremacy clauses (e.g., section 33 of the Canadian Charter, which explicitly permits legislative override) versus those without such clauses. If non-supremacy texts are read by proponents to support supremacy anyway, the reading is a framing choice, not a textual discovery.',
    'If the text is genuinely clear about legislative supremacy, the reading is a faithful instantiation of the kernel. If the text is ambiguous and proponents choose the supremacy reading, then the reading is constructed and the constraint is falsely naturalized — it would shift from mountain-adjacent (if text is clear) to tangled_rope-or-snare (if reading is chosen).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(text_vs_reading_ambiguity, empirical, 'Whether legislative supremacy is established by the text or by interpretive choice.').

omega_variable(
    minority_suppression_mechanism,
    'Is the measured suppression (0.52) structural (minorities face legal barriers to override legislative constitutional re-interpretation) or internalized (minorities have accepted the reading''s legitimacy and treat legislative authority as binding even when they disagree)?',
    'Post-override behavior: if minorities mount sustained constitutional challenges after override (litigation, amendment campaigns, civil disobedience), the suppression is structural and not fully accepted. If they acquiesce and internalize deference to legislative authority, the suppression is partly internalized.',
    'Structural suppression suggests the constraint''s persistence depends on active force. Internalized suppression suggests the constraint has colonized the beliefs of the targets — they carry the suppression after any hypothetical exit from subordination. If internalized, the effective extraction is higher than the measured 0.68 because the target''s options shrink not just externally but cognitively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_suppression_mechanism, empirical, 'Whether minority suppression is structural or internalized.').

omega_variable(
    competing_reading_foreclosure,
    'Does the legislative sovereignty reading logically foreclose the judicial supremacy reading within a single constitutional framework, or can both readings coexist as live positions held by different institutional seats?',
    'Institutional practice: in jurisdictions claiming both readings (legislature asserts supremacy; courts assert coordinate authority), observe whether the contradiction is resolved (one reading wins) or perpetuated (both persist). If both persist indefinitely, they coexist rather than foreclose.',
    'If legislative sovereignty forecloses judicial supremacy, the two readings cannot coexist in coherent practice and one must eventually displace the other. If they coexist, the constraint is inherently contested and the measurement series should show oscillation (legislature asserts supremacy, courts reassert authority, cycle repeats) rather than monotonic growth toward stabilization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_reading_foreclosure, conceptual, 'Whether this reading logically forecloses the sibling judicial supremacy reading.').

omega_variable(
    electoral_cycle_effects,
    'Does the measured extractiveness (0.68) vary with electoral cycles — rising when the majority is secure, falling when it faces challenge — or is it constant across electoral conditions?',
    'Time-series analysis during known electoral cycles: does extractiveness spike or dip around elections, votes of no-confidence, or coalition changes?',
    'If extractiveness varies with electoral security, the constraint is conditional on majoritarian power — it persists because the majority can enforce it, not because of inherent textual authority. If extractiveness is constant, the reading has been normalized and persists independent of majoritarian strength.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(electoral_cycle_effects, empirical, 'Electoral cycle effects on measured constraint strength.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cons_tr_t5, constitutional_text__legislative_sovereignty_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__legislative_sovereignty_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(cons_tr_t15, constitutional_text__legislative_sovereignty_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__legislative_sovereignty_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cons_tr_t25, constitutional_text__legislative_sovereignty_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__legislative_sovereignty_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(cons_tr_t35, constitutional_text__legislative_sovereignty_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cons_be_t5, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(cons_be_t10, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(cons_be_t15, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(cons_be_t20, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(cons_be_t25, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(cons_be_t30, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cons_be_t35, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cons_su_t5, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(cons_su_t10, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(cons_su_t15, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(cons_su_t20, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(cons_su_t25, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(cons_su_t30, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(cons_su_t35, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 35, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__legislative_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'constitutional_text'. The sibling readings (judicial_supremacy_reading, popular_sovereignty_reading) are separate constraint stories with different ε values, beneficiary/victim structures, and classifications. All three readings share the same text (the kernel) but instantiate different constraints because they disagree on what the text establishes about ultimate authority. The network edges record that these constraints are structurally coupled: changes in the legitimacy of one reading (e.g., legislative override becomes seen as illegitimate, or courts formally accept subordination) would ripple into the others. Do not merge the readings into one story — the ε-invariance principle requires separate stories because each reading produces a different structural analysis of extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
