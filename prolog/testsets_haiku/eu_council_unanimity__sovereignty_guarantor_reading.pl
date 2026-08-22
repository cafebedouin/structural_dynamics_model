% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__sovereignty_guarantor_reading, []).

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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity as Sovereignty Protection
 *   domain: institutional_design/international_relations
 *
 * SUMMARY:
 *   The EU Council's unanimity requirement for decisions affecting core state
 *   sovereignty (taxation, justice, foreign affairs, border control, cultural
 *   matters) is institutionalized as a guarantee that no member state can be
 *   overruled on questions that implicate its fundamental independence. This
 *   reading frames unanimity as a protection mechanism: each state retains
 *   veto power to defend its vital interests against majoritarian coalitions.
 *   Small and medium states are the primary beneficiaries; large states pay
 *   the coordination cost of building consensus. The constraint is
 *   fundamentally about rights-protection (negative liberty — the right to
 *   say no) rather than extraction. The measured extractiveness is moderate
 *   because the constraint genuinely coordinates the collective action
 *   problem (how to govern together without hegemonism) while also imposing
 *   real friction costs on policy formation. The constraint is CLAIMED as
 *   rope (coordination with beneficiary protection) and the metrics describe
 *   a coordination-heavy structure with limited systematic extraction — a
 *   deliberate absence of divergence between claim and metrics in this
 *   reading, reflecting the sovereignty-guarantor framing.
 *
 * KEY AGENTS:
 *   - Small states (Poland, Hungary, Cyprus, Malta, etc.): beneficiary — veto power as guarantee against majoritarian overruling; moderate power, constrained exit (union membership itself is valuable enough to accept coordination costs)
 *   - Medium states (Spain, Belgium, Netherlands, etc.): dual beneficiary/payer — benefit from veto protection on their own sovereignty, pay coordination cost when blocking others' priorities
 *   - Large states (Germany, France, Italy): payer — cannot unilaterally set EU agenda; must build consensus with smaller states they could economically dominate outside the union
 *   - Council secretariat: agenda-setter, facilitator — administers unanimity procedure without extracting
 *   - EU Parliament: excluded — represents citizens but has no voice in unanimity decisions
 *   - EU citizens: excluded, identity-locked — bound by union but cannot veto national government veto; interests mediated through member states
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.38).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.12).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity as Sovereignty Protection").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional_design/international_relations").

domain_priors:requires_active_enforcement(eu_council_unanimity__sovereignty_guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '27afdec4-cdd7-4815-a710-63f45eaf5d4c').
narrative_ontology:cs_kernel_codification('27afdec4-cdd7-4815-a710-63f45eaf5d4c', formalized).
narrative_ontology:cs_authority_grounding('27afdec4-cdd7-4815-a710-63f45eaf5d4c', lineage).
narrative_ontology:cs_interpretation_layer_present('27afdec4-cdd7-4815-a710-63f45eaf5d4c').
narrative_ontology:cs_reading_relation('27afdec4-cdd7-4815-a710-63f45eaf5d4c', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('27afdec4-cdd7-4815-a710-63f45eaf5d4c', eu_council_unanimity__diplomatic_capital_reading, influences).
narrative_ontology:cs_axiom('27afdec4-cdd7-4815-a710-63f45eaf5d4c', foundational, veto_as_sovereign_right).
narrative_ontology:cs_axiom_status(veto_as_sovereign_right, holdable).
narrative_ontology:cs_axiom_grounding('27afdec4-cdd7-4815-a710-63f45eaf5d4c', veto_as_sovereign_right, deontological).
narrative_ontology:cs_axiom('27afdec4-cdd7-4815-a710-63f45eaf5d4c', foundational, small_state_protection_necessity).
narrative_ontology:cs_axiom_status(small_state_protection_necessity, holdable).
narrative_ontology:cs_axiom_grounding('27afdec4-cdd7-4815-a710-63f45eaf5d4c', small_state_protection_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('27afdec4-cdd7-4815-a710-63f45eaf5d4c', treaty_based_sovereign_veto_guarantee).
narrative_ontology:cs_drift_state('27afdec4-cdd7-4815-a710-63f45eaf5d4c', contemporary_rule_of_law_disputes, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('27afdec4-cdd7-4815-a710-63f45eaf5d4c', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, medium_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, medium_states).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, large_states).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens).
narrative_ontology:constraint_vindicates(eu_council_unanimity__sovereignty_guarantor_reading, sovereignty_equality_doctrine).
narrative_ontology:constraint_vindicates(eu_council_unanimity__sovereignty_guarantor_reading, consent_based_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess formal veto power over EU-wide policy despite limited economic or military capacity. Can block initiatives they assess as infringing core sovereignty — border control, taxation, religious or cultural matters. Without unanimity, they would be structurally overruled by larger-state coalitions. The veto is their only structural guarantee that their interests cannot be trampled.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_states, beneficiary,
    moderate, generational, constrained, continental).

% Benefit from the guarantee that their own core sovereignty cannot be overruled by a European majority. Also bear the cost of negotiating consensus when their preferences diverge from smaller states, or when one state's veto halts policies the medium states support. Their position oscillates between veto-wielder and veto-target depending on the issue.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, medium_states, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, medium_states, payer).

% Possess the greatest economic and military resources but are structurally constrained by the unanimity requirement when smaller states block initiatives the large states deem necessary. They bear the coordination cost of building consensus and the loss of unilateral agenda-setting power they would hold in a majority-rule system. They cannot impose policy on the smaller states even when the distribution of raw power would favor them.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_states, payer,
    institutional, generational, constrained, continental).

% Administers the formal unanimity rule and facilitates negotiation to reach consensus. Does not extract from the constraint but shapes its operation through procedural design, timing of votes, and interpretation of abstention rules. Can accelerate or decelerate consensus-building but cannot override the substantive requirement.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, council_secretariat, agenda_setter,
    organized, generational, analytical, continental).

% Represents EU citizens directly (by principle) but is excluded from the unanimity requirement for most core sovereignty decisions. Cannot veto even where it might represent genuine popular opposition within a member state. Has formal legislative power over some domains (codecision) but not over justice, taxation, or foreign affairs in Council unanimity procedures.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_parliament, excluded,
    organized, generational, constrained, continental).

% Subject to EU law and policy but have no direct voice in unanimity decisions. A single national government's veto can block EU-wide action (e.g., climate policy, pandemic response) that the citizen's own country's electorate might support. Their interests are mediated entirely through member-state governments, which may or may not align with supermajority European preferences.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens, excluded,
    powerless, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens, payer).

% Operate across EU borders and are affected by policy harmonization decisions that unanimity rules can block. They lobby member states to align on harmonization (to reduce compliance complexity) or to block harmonization (when diversity creates profit opportunities). They are not organizational parties to the unanimity rule but operate in its shadow and bear the consequences of veto-induced policy paralysis or fragmentation.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, business_coalitions, observer,
    powerful, biographical, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__sovereignty_guarantor_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__sovereignty_guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects each member state's right to defend core sovereignty interests without being overruled by a European majority. Solves the coordination problem: how can a union of unequal-sized states protect themselves from majoritarian expropriation while still enabling collective action in areas where genuine consensus exists.
% TRANSFER_FUNCTION: Transfers procedural power: each state gains a formal veto right; in exchange, each state accepts that consensus-building is slower and that initiatives cannot proceed without its explicit agreement. The transfer is of decision-making tempo and unilateral agenda-setting capacity (from larger to smaller states, in a directional sense, though the larger states also benefit from the same protection when roles reverse).
% ABSENT_VOICES: EU citizens themselves are excluded: they cannot participate in unanimity decisions or override a national veto even when supermajority sentiment exists across the EU. The European Parliament (representing citizens directly) is also excluded from most unanimity-governed domains. Within member states, domestic opposition to a national government's veto has no channel into EU negotiations.
% DISAPPEARANCE_RATIONALE: If unanimity vanished and majority voting replaced it across all domains, the political equilibrium would shift immediately: larger states would pursue initiatives they are currently blocked from implementing; smaller states would lose the structural guarantee of veto protection and would face systematic overruling on issues where coalitions of large states align against them. The EU's institutional architecture and its policy output would reorganize substantially.
% FOUNDING_PROBLEM: After World War II, smaller European states feared absorption into a hegemonic union where their interests would be systematically overruled. The founding problem was: how can we form a union that constrains hegemonic behavior and ensures that no coalition of larger states can impose policy on us without our consent?
% FOUNDING_PROBLEM_CORROBORATION: Smaller member states (Poland, Hungary, Cyprus, Malta, others) invoke the founding problem continuously when defending veto use — they cite the asymmetry of power and the risk of majoritarian overruling. Historians and integration scholars outside the benefiting parties (e.g., Pierson, Schimmelfennig, Keohane) document that the original unanimity requirement was precisely constructed to protect smaller states from hegemonic dynamics. Even larger states acknowledge the legitimacy of the founding problem while disagreeing about whether unanimity remains the right instrument.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).
:- end_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the constraint solves a genuine coordination problem (protecting small-state interests) but also imposes real friction on policy formation. The founding problem (protection against hegemonism) is live — smaller states continue to defend unanimity explicitly as sovereignty protection. Suppression is very low (0.12) because this reading's coherence depends on the veto being a legitimate rights-exercise, not a coerced mechanism. If suppression were high, the constraint would be a snare, not a rope protecting rights. Theater ratio has oscillated moderately (0.08 to 0.24) with peaks around 2016–2022 when Hungary and Poland used veto extensively on rule-of-law grounds, generating performative debate about whether they were defending sovereignty or weaponizing the veto. The recent decline (to 0.18 by 2026) reflects normalization after the acute rule-of-law disputes. The measurement grid shares time points across all three metrics, allowing temporal analysis of how the constraint evolved. Notably, extractiveness did NOT rise monotonically — it peaked around 2016 and slightly declined as the EU developed stronger filtering mechanisms (qualified-majority-vote carve-outs for procedural decisions) that reduced veto frequency on low-stakes issues.
 *
 * PERSPECTIVAL GAP:
 *   Small states experience this constraint as legitimate sovereignty protection: they see their veto use as rights-exercise. Large states experience it as coordination friction and loss of unilateral power: they see small-state veto use as obstruction. The engine's per-seat computation should capture this divergence: from a small-state beneficiary seat, the directionality (d) is low (veto-wielder benefits), while from a large-state constrained seat, d is high (coordination burden imposed). The shared understanding of the constraint's legitimacy (foundational, not extractive) unites the reading despite the perspectival gap — both large and small states acknowledge that small-state veto protection is a legitimate feature of EU design, even when they dispute specific veto uses.
 *
 * DIRECTIONALITY LOGIC:
 *   Small and medium states have low directionality (d near beneficiary end, ~0.25–0.35) because they benefit from veto power as an absolute guarantee, with exit only at the cost of union membership itself (identity-locked, but the exit is existentially costly). Large states have higher directionality (d near 0.55–0.65) because the constraint imposes coordination requirements and denies unilateral agenda-setting, but they retain veto power too (symmetric in form, though less valuable in practice given their relative strength). The council secretariat is analytical (d = 0.5, no extraction, purely administrative). EU citizens are analytically excluded (no seat in the unanimity decision; power to raise directionality questions does not translate to voting power). The beneficiary/victim split is clean: beneficiaries are the small and medium states (named agents); victims (in the sense of bearing coordination cost) are the large states and the EU population at large. There is no single villain extracting rents — the 'victimhood' is the price of the coordinating mechanism itself, not systematic extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT mandatrophic. Its founding problem (protection against hegemonism) remains live and is explicitly defended by beneficiary states. The constraint's persistence tracks the persistence of its justifying function — smaller states continue to assert that unanimity is essential to prevent overruling on sovereignty matters. The recent oscillations in veto use (peaks in 2016–2022, decline by 2026) reflect tactical variation, not mandate-death. If the founding problem became dead (smaller states stopped worrying about overruling, or the EU evolved other mechanisms that made unanimity redundant), then we would see mandatrophy signals — performance-maintenance without function. That is not yet the case for this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_legitimacy_vs_obstruction,
    'Is the boundary between legitimate sovereignty defense (veto use) and obstruction/rentierism empirically stable, or does it depend on framing by the vetoing state?',
    'Analyze veto use patterns: do vetoing states cite sovereignty/core-interest grounds consistently, or do they shift justifications strategically? Compare outcomes when veto uses are motivated by sovereignty concerns (e.g., border control, taxation, religious liberty) versus instrumental concerns (e.g., negotiating leverage on unrelated issues). Look for third-party assessments from within EU institutions that distinguish legitimate from strategic veto use.',
    'If the boundary is stable and observable (certain veto uses are widely recognized as legitimate, others as strategic), the sovereignty_guarantor reading remains coherent and ε stays moderate. If the boundary is chronically ambiguous and subject to actor reinterpretation, the reading''s coherence weakens and ε should drift toward higher values (the constraint becomes more extractive as legitimacy becomes contested).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_legitimacy_vs_obstruction, empirical, 'Whether veto use can be distinguished from rights-exercise to obstruction on stable grounds').

omega_variable(
    small_state_dependency_lock,
    'Do small states defend unanimity genuinely for sovereignty protection, or is unanimity defended because they have become dependent on EU membership and cannot afford real exit (de facto identity-locked)?',
    'Assess whether small states would spontaneously demand unanimity if they were newly negotiating EU terms, or whether their attachment to unanimity is defensive (preserving what they have, not because it is optimal). Examine discourse from small-state governments and legislatures: do they frame unanimity as essential protection or as a lesser evil compared to majority rule? Test counterfactually: if EU offered small states alternative mechanisms (e.g., supermajority-minus-small-states, dual supermajorities, reserved domains), would they prefer them to unanimity?',
    'If small-state defense of unanimity is driven by genuine sovereignty concerns, the reading is robust — veto is understood as protecting real interests. If defense is driven by dependency (they cannot exit the union, so they defend the best protection the union offers them), then the constraint may be partially extractive — small states are paying the coordination cost because they cannot refuse membership. This would raise ε and complicate the beneficiary/victim split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_state_dependency_lock, empirical, 'Whether small-state support for unanimity is authentic preference or defensive adaptation to constrained choices').

omega_variable(
    large_state_unanimity_alternatives,
    'Are large states genuinely constrained by unanimity, or do they possess alternative leverage mechanisms (economic pressure, institutional capture, norm-setting) that allow them to achieve outcomes despite formal veto?',
    'Examine outcomes in areas where unanimity is required but large-state preferences diverged from small-state interests (e.g., fiscal coordination, environmental standards, defense spending). Did small states actually block large-state initiatives, or did informal mechanisms (budgetary leverage, treaty reform threats, selective implementation) allow large states to achieve their goals despite formal vetoes? Quantify the rate at which unanimity blocks are actually enforced versus negotiated away through informal side-payments.',
    'If large states can systematically overcome unanimity through informal mechanisms, then the formal constraint is partly theatrical — ε should increase because extraction is occurring through mechanisms not visible in the formal rule. If unanimity actually blocks large-state initiatives (as the sovereignty_guarantor reading claims), then the constraint is genuinely coordinating and ε remains moderate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(large_state_unanimity_alternatives, empirical, 'Whether unanimity is a binding constraint on large-state action or a formal requirement with informal override mechanisms').

omega_variable(
    kernel_reading_distinctness,
    'Do the sovereignty_guarantor, veto_trap, and diplomatic_capital readings describe genuinely distinct constraint mechanisms, or are they alternative interpretations of a single underlying constraint that should be modeled as one story with multiple readings?',
    'Compare the three readings on the five structural dimensions: (1) What coordination problem does each reading claim the constraint solves? (2) Who is named as beneficiary in each? (3) What is the baseline ε for each (the measured extractiveness of the standing arrangement)? (4) What would falsify each reading''s core claim? (5) Do they have different stakeholder maps or the same stakeholders with different roles?',
    'If the readings differ substantially on coordination function, beneficiary set, or ε, they are genuinely distinct constraints (per the ε-invariance principle) and should remain as three separate stories. If they interpret the same constraint with the same beneficiary/victim structure and same ε from different normative angles (good sovereignty protection vs. bad obstruction vs. neutral consensus-forcing), they should be consolidated into one story with omega variables documenting the normative interpretation gap. Current authoring treats them as three distinct constraints (different ε, different beneficiary structures); this omega tests whether that decomposition is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinctness, conceptual, 'Whether the three readings of unanimity are distinct constraints or alternative normative framings of a single constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 1992, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t1992, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1992, 0.08).
narrative_ontology:measurement_basis(eu_c_tr_t1992, observed).
narrative_ontology:measurement(eu_c_tr_t2000, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement_basis(eu_c_tr_t2000, observed).
narrative_ontology:measurement(eu_c_tr_t2008, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement_basis(eu_c_tr_t2008, observed).
narrative_ontology:measurement(eu_c_tr_t2016, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2016, 0.22).
narrative_ontology:measurement_basis(eu_c_tr_t2016, observed).
narrative_ontology:measurement(eu_c_tr_t2022, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2022, 0.24).
narrative_ontology:measurement_basis(eu_c_tr_t2022, observed).
narrative_ontology:measurement(eu_c_tr_t2026, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2026, 0.18).
narrative_ontology:measurement_basis(eu_c_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1992, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1992, 0.32).
narrative_ontology:measurement_basis(eu_c_be_t1992, observed).
narrative_ontology:measurement(eu_c_be_t2000, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement_basis(eu_c_be_t2000, observed).
narrative_ontology:measurement(eu_c_be_t2008, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2008, 0.38).
narrative_ontology:measurement_basis(eu_c_be_t2008, observed).
narrative_ontology:measurement(eu_c_be_t2016, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2016, 0.41).
narrative_ontology:measurement_basis(eu_c_be_t2016, observed).
narrative_ontology:measurement(eu_c_be_t2022, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2022, 0.39).
narrative_ontology:measurement_basis(eu_c_be_t2022, observed).
narrative_ontology:measurement(eu_c_be_t2026, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(eu_c_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t1992, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 1992, 0.08).
narrative_ontology:measurement_basis(eu_c_su_t1992, observed).
narrative_ontology:measurement(eu_c_su_t2000, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement_basis(eu_c_su_t2000, observed).
narrative_ontology:measurement(eu_c_su_t2008, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2008, 0.11).
narrative_ontology:measurement_basis(eu_c_su_t2008, observed).
narrative_ontology:measurement(eu_c_su_t2016, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2016, 0.14).
narrative_ontology:measurement_basis(eu_c_su_t2016, observed).
narrative_ontology:measurement(eu_c_su_t2022, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2022, 0.13).
narrative_ontology:measurement_basis(eu_c_su_t2022, observed).
narrative_ontology:measurement(eu_c_su_t2026, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2026, 0.12).
narrative_ontology:measurement_basis(eu_c_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__sovereignty_guarantor_reading, 0.18).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% This story is part of the EU Council unanimity constraint family (kernel: eu_council_unanimity). The kernel is a formalized rule requiring unanimous consent for decisions affecting core state sovereignty. Three structurally distinct readings instantiate three constraint stories: (1) sovereignty_guarantor_reading — unanimity as protection against majoritarian overruling; small states are beneficiaries; ε moderate, claimed type rope; (2) veto_trap_reading — unanimity enables minoritarian extraction through credible blocking; small states become extractive agents; ε high, claimed type snare; (3) diplomatic_capital_reading — unanimity forces consensus-building and strengthens policy legitimacy; beneficiary set shifts to the mechanism itself (legitimacy as the good); ε lower, claimed type rope. These readings diverge on the constraint's primary function, beneficiary structure, and extractiveness. Each decomposition follows the ε-invariance principle: changing the reading changes what property (extractiveness) is being measured, so they are genuinely distinct constraints, not alternative perspectives on a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_council_unanimity__sovereignty_guarantor_reading, powerful, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
