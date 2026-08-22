% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__textualist_paradox_reading, []).

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
 *   constraint_id: all_men_created_equal__textualist_paradox_reading
 *   human_readable: Textualist Paradox Reading of 'All Men Created Equal'
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This reading treats the Declaration's 'all men are created equal' as a
 *   textual commitment whose universal semantic scope ('all men') cannot be
 *   coherently restricted by the 18th-century social taxonomy of its authors
 *   without generating a performative contradiction: the text says 'all,' the
 *   authors meant 'some,' and the constraint is the interpretive pressure
 *   that contradiction exerts on any framework claiming fidelity to the text.
 *   The reading does not resolve the contradiction by expanding 'all men'
 *   (universalist reading) nor by binding meaning to founder intent
 *   (originalist reading); it holds the contradiction as an active structural
 *   feature that delegitimizes originalist authority claims by showing the
 *   text itself exceeds and resists the interpretive frame that would contain
 *   it. The victim is the originalist interpretive framework — its claim to
 *   be the uniquely faithful reading of the founding text is structurally
 *   undermined by the text's own universal language. The coordination
 *   function is providing a stable interpretive anchor that prevents meaning
 *   from collapsing into pure intent or pure will; the extraction is the
 *   delegitimization of the originalist seat that previously claimed
 *   exclusive interpretive authority.
 *
 * KEY AGENTS:
 *   - constitutional_textualists: Primary beneficiary (analytical/powerful) — uses the paradox to anchor textual supremacy over intent
 *   - originalist_interpretive_framework: Primary victim (institutional/organized) — its authority claim is delegitimized by the text's own semantics
 *   - living_constitutionalists: Secondary beneficiary (organized/powerful) — gains interpretive space from the paradox without adopting textualist method
 *   - equal_protection_litigants: Tertiary beneficiary (moderate/powerless) — gains doctrinal resources from the reading's destabilization of static originalism
 *   - founder_intent_jurisprudence: Secondary victim (institutional/organized) — loses exclusive claim to founding-era authority
 *   - static_originalism_scholars: Tertiary victim (organized/moderate) — professional identity and institutional position built on the framework the paradox undermines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.38).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.22).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "Textualist Paradox Reading of 'All Men Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, '1327c05f-aaea-477e-991e-d0d7fc93e826').
narrative_ontology:cs_kernel_codification('1327c05f-aaea-477e-991e-d0d7fc93e826', fixed_text).
narrative_ontology:cs_authority_grounding('1327c05f-aaea-477e-991e-d0d7fc93e826', lineage).
narrative_ontology:cs_interpretation_layer_present('1327c05f-aaea-477e-991e-d0d7fc93e826').
narrative_ontology:cs_reading_relation('1327c05f-aaea-477e-991e-d0d7fc93e826', all_men_created_equal__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('1327c05f-aaea-477e-991e-d0d7fc93e826', all_men_created_equal__universalist_reading, influences).
narrative_ontology:cs_axiom('1327c05f-aaea-477e-991e-d0d7fc93e826', foundational, textual_semantics_exceed_founder_intent).
narrative_ontology:cs_axiom_status(textual_semantics_exceed_founder_intent, holdable).
narrative_ontology:cs_axiom_grounding('1327c05f-aaea-477e-991e-d0d7fc93e826', textual_semantics_exceed_founder_intent, empirically_contingent).
narrative_ontology:cs_axiom('1327c05f-aaea-477e-991e-d0d7fc93e826', foundational, performative_contradiction_delegitimizes_exclusive_fidelity_claim).
narrative_ontology:cs_axiom_status(performative_contradiction_delegitimizes_exclusive_fidelity_claim, holdable).
narrative_ontology:cs_axiom_grounding('1327c05f-aaea-477e-991e-d0d7fc93e826', performative_contradiction_delegitimizes_exclusive_fidelity_claim, deontological).
narrative_ontology:cs_reference_frame('1327c05f-aaea-477e-991e-d0d7fc93e826', founding_generation_understanding).
narrative_ontology:cs_drift_state('1327c05f-aaea-477e-991e-d0d7fc93e826', contemporary_interpretive_debate, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1327c05f-aaea-477e-991e-d0d7fc93e826', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, constitutional_textualists).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, living_constitutionalists).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, equal_protection_litigants).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, founder_intent_jurisprudence).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, static_originalism_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars and jurists who prioritize text over intent. The paradox gives them a textual anchor to resist originalist intent-based readings without committing to living constitutionalism's expansive method. They can cite the text's own words against originalist authority claims. Exit is arbitrage-grade: they can shift between textualist, originalist, and pragmatist frameworks as methodological commitments without professional penalty.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, constitutional_textualists, beneficiary,
    powerful, generational, arbitrage, national).

% The interpretive structure claiming that constitutional meaning is fixed by the founding generation's understanding. Its authority rests on being the uniquely faithful reading. The paradox shows the founding text's own language exceeds that understanding. The framework cannot exit without abandoning its core premise (fidelity to original meaning); its institutional bearers (Federalist Society, originalist judges, law school centers) have fused professional identity with the framework. Exit means professional and institutional reconstitution.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework, payer,
    institutional, generational, identity_locked, national).

% Scholars and jurists who read the Constitution as evolving. The paradox weakens their main interpretive rival (originalism) without requiring them to adopt textualist method. They benefit from the delegitimization pressure on originalism. Exit is mobile: they can engage or ignore the paradox without methodological cost.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, living_constitutionalists, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__textualist_paradox_reading, living_constitutionalists, observer).

% Litigants and advocacy groups pressing equal protection claims. The paradox provides doctrinal resources to challenge originalist narrowings of 'equal protection' scope. They use the reading instrumentally. Exit is constrained: they need viable legal arguments but are not committed to any interpretive theory.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, equal_protection_litigants, beneficiary,
    moderate, biographical, constrained, national).

% The body of doctrine and precedent that treats founder intent as the touchstone of constitutional meaning. The paradox demonstrates that the most famous founding text's intent contradicts its text. This jurisprudence cannot abandon intent without collapsing its distinguishing claim. Institutional bearers (originalist judges, scholarship networks) are identity_locked.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, founder_intent_jurisprudence, payer,
    institutional, generational, identity_locked, national).

% Academics whose professional identity, publication record, and institutional positions are built on defending originalism as the uniquely legitimate interpretive method. The paradox directly targets the coherence of their life's work. Exit requires abandoning the scholarly identity they have constructed — professionally and psychologically prohibitive.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, static_originalism_scholars, payer,
    organized, biographical, identity_locked, national).

% Scholars who study constitutional interpretation across systems. They observe the paradox as a case study in how founding texts generate interpretive pressures that exceed their authors' intent. They have no stake in the U.S. interpretive debate and full analytical exit.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable interpretive anchor: the text's own universal language constrains interpretation without requiring either founder intent (originalism) or judicial will (living constitutionalism) as the sole authority. It coordinates interpretive practice around a textual fixed point that exceeds its authors.
% TRANSFER_FUNCTION: Moves interpretive authority from the originalist framework (which claims exclusive fidelity to the founding) to a textualist reading that uses the text's own semantics against that claim. The transfer is authority and legitimacy, not material resources.
% ABSENT_VOICES: The founding generation itself — they cannot speak to whether they intended the universal language as aspiration or commitment. Enslaved persons and women of the founding era — their exclusion from 'all men' is the historical fact the paradox illuminates, but they were structurally excluded from the constitutional conversation. Contemporary originalist judges who decline to engage the paradox substantively — their silence is a form of absence from the scholarly debate.
% DISAPPEARANCE_RATIONALE: If the paradox reading vanished, originalism would lose its most potent textualist challenger — the reading that uses the founding text's own words against originalist authority. The interpretive landscape would shift toward a binary originalism vs. living constitutionalism contest, with textualism absorbed into one side or the other. The constraint's pressure on originalist coherence would disappear.
% FOUNDING_PROBLEM: The founding problem was stabilizing constitutional meaning against judicial discretion by anchoring interpretation in a fixed, authoritative source. Originalism claimed that source was founder intent. This reading claims the source is the text itself, and that the text's universal semantics generate a contradiction that originalism cannot resolve without abandoning its claim to exclusive fidelity.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (Scalia, Bork, Barnett, McGinnis) attest the founding problem is live — judicial discretion remains the threat. Textualist scholars (Eskridge, Manning, Nourse) and living constitutionalists (Balkin, Strauss) attest the problem is mischaracterized: the text itself, not intent, was the founding fix, and the paradox shows the text exceeds originalist containment. No neutral arbiter exists; the status is genuinely contested across interpretive communities.
narrative_ontology:disappearance_verdict(all_men_created_equal__textualist_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__textualist_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(all_men_created_equal__textualist_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__textualist_paradox_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__textualist_paradox_reading_tests).
:- end_tests(all_men_created_equal__textualist_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the reading extracts interpretive authority from the originalist framework without replacing it with a new authoritative structure — it creates pressure, not a replacement regime. Suppression is low (0.22) because the constraint operates through argumentative force and scholarly pressure, not coercion; originalist scholars remain free to maintain their position, but the interpretive cost of doing so rises. Theater ratio (0.31) reflects that some performances of 'fidelity to text' in originalist discourse are increasingly performative — the contradiction is managed rather than engaged. Accessibility collapse (0.42) is moderate: alternatives (originalism, universalism) remain live but are forced to acknowledge the paradox. Resistance (0.55) is significant: originalist institutions (Federalist Society, originalist jurisprudence, judicial appointments) actively resist the reading's implications.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualists and living constitutionalists are beneficiaries (d low) — the paradox gives them interpretive leverage without requiring them to defend a positive alternative. Originalist framework and its institutional bearers are victims (d high) — their authority claim is the target of extraction. The originalist framework is an interpretive structure, not a person; its 'exit options' are theoretical (abandon the framework, modify it, or absorb the contradiction as managed tension), making it identity_locked at the institutional level. Equal protection litigants are mobile beneficiaries — they can use the reading instrumentally without committing to its method.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling the originalist claim to exclusive fidelity as pure coordination (it is coordination + extraction — the framework coordinates interpretive practice while extracting authority from the text's actual semantics). The paradox reading exposes the extraction component. The founding problem (stabilizing constitutional meaning against judicial will) is contested: originalists say it's live; this reading says the text itself solved it differently than originalism claims. The constraint is not a piton — it has active scholarly maintenance and real interpretive stakes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performative_contradiction_operativity,
    'Does the performative contradiction in ''all men are created equal'' operate as an active interpretive constraint on legal reasoning, or is it a philosophical observation that courts and scholars can acknowledge without changing their interpretive practice?',
    'Trace citation networks: does the paradox appear in majority opinions, dissents, and briefs as a premise that moves outcomes, or only in law review commentary? Measure the frequency with which originalist opinions engage the paradox substantively versus managing it through distinction or dismissal.',
    'If operative, the reading has structural force in the legal system (moderate extraction from originalist authority). If merely philosophical, extractiveness is lower and the constraint is closer to rope (coordination of interpretive discourse) than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_contradiction_operativity, empirical, 'Whether the textual paradox functions as a working constraint in legal practice').

omega_variable(
    originalist_framework_resilience,
    'Can the originalist interpretive framework absorb the paradox as a managed tension (e.g., ''the text''s universal language was aspirational, its legal force was bounded'') without losing its claim to exclusive fidelity?',
    'Analyze post-2008 originalist scholarship (New Originalism, original methods originalism, original law originalism) for explicit engagement with the paradox. Determine whether a coherent originalist response exists that preserves the framework''s authority claim.',
    'If absorbable, the victim (originalist framework) is not genuinely threatened — extraction is lower, constraint may be rope. If not absorbable, the framework faces genuine delegitimization pressure — extraction is higher, tangled_rope confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalist_framework_resilience, conceptual, 'Whether originalism can internally resolve the paradox without conceding the reading''s core claim').

omega_variable(
    kernel_identity_conditions,
    'Is ''all_men_created_equal'' a single kernel with three readings, or are these three distinct constraints that share only a linguistic label?',
    'Apply the ε-invariance test: do the three readings have stably different extractiveness profiles across the interval when assessed by their own lights? If ε differs structurally (not just in emphasis), they are distinct constraints linked by network.affects_constraints, not readings of one kernel.',
    'If distinct constraints, this story''s kernel_context is mis-specified and should be re-authored as a standalone constraint with network links. If one kernel, the committer frame is correct and the reading_relations/axioms capture the structural relationships.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_conditions, conceptual, 'Whether the kernel/reading frame or the constraint-family frame is structurally correct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 1776, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1776, 0.05).
narrative_ontology:measurement(all__tr_t1857, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1857, 0.12).
narrative_ontology:measurement(all__tr_t1868, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1868, 0.18).
narrative_ontology:measurement(all__tr_t1896, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1896, 0.24).
narrative_ontology:measurement(all__tr_t1954, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1954, 0.28).
narrative_ontology:measurement(all__tr_t2025, all_men_created_equal__textualist_paradox_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1776, 0.15).
narrative_ontology:measurement(all__be_t1857, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1857, 0.22).
narrative_ontology:measurement(all__be_t1868, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1868, 0.28).
narrative_ontology:measurement(all__be_t1896, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1896, 0.31).
narrative_ontology:measurement(all__be_t1954, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1954, 0.35).
narrative_ontology:measurement(all__be_t2025, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1776, 0.08).
narrative_ontology:measurement(all__su_t1857, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1857, 0.14).
narrative_ontology:measurement(all__su_t1868, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1868, 0.18).
narrative_ontology:measurement(all__su_t1896, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1896, 0.21).
narrative_ontology:measurement(all__su_t1954, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1954, 0.22).
narrative_ontology:measurement(all__su_t2025, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 2025, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__textualist_paradox_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__textualist_paradox_reading, 0.08).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__universalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading in the all_men_created_equal kernel family. The originalist_reading claims exclusive fidelity to founder intent; the universalist_reading claims the principle demands iterative expansion. This reading claims the text's universal semantics generate a performative contradiction that delegitimizes the originalist authority claim without adopting the universalist expansion. All three have distinct ε profiles and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__textualist_paradox_reading, institutional, 0.78).
constraint_indexing:directionality_override(all_men_created_equal__textualist_paradox_reading, organized, 0.65).
constraint_indexing:directionality_override(all_men_created_equal__textualist_paradox_reading, analytical, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
