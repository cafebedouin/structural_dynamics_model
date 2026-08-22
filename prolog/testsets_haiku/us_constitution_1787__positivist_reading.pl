% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: U.S. Constitution (1787): Positivist Reading — Text-Plus-Amendment Framework
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The positivist reading of the U.S. Constitution frames constitutional
 *   meaning as the union of two components: (1) what the constitutional text,
 *   as written, says, and (2) legitimate amendments approved through the
 *   Article V process. Under this reading, judicial interpretation is
 *   constrained to the text as currently written and amended. Courts cannot
 *   evolve meaning beyond the text by invoking contemporary values (living
 *   reading) or by inferring framers' intent not expressed in the words
 *   (originalism's historical reconstruction). Constitutional change must
 *   come through amendment—a supermajority-gated democratic process. This
 *   reading positions the judiciary as interpreter-of-settled-text rather
 *   than as co-author of constitutional meaning. The constraint is CLAIMED as
 *   rope (coordination problem solved: stable meaning + democratic amendment
 *   authority). The authored metrics describe moderate extractiveness (the
 *   high amendment bar imposes cost on reformers) and low suppression (the
 *   constraint is legitimate, not coercive), which align with the rope claim.
 *
 * KEY AGENTS:
 *   - Amendment Constituency: The democratic electorate, organized through state legislatures and constitutional conventions, empowered to amend. Benefits from the positivist reading by concentrating constitutional authority in democratic hands.
 *   - Text-Bound Judiciary: Courts operating under text-as-written guidance. Benefits by having clear interpretive boundaries and reduced legitimacy pressure to innovate.
 *   - Supermajority-Gated Reformers: Civil rights movements, regulatory reformers, and others seeking constitutional change. Bear the cost of the high amendment bar; must build broad consensus rather than winning judicial reinterpretation.
 *   - Originalist and Living Constitutional Jurists: Excluded from this framework but would contest its core premises.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.42).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.28).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.19).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.19).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "U.S. Constitution (1787): Positivist Reading — Text-Plus-Amendment Framework").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, '09c4495d-c31a-4800-867f-5a5afddb6a09').
narrative_ontology:cs_kernel_codification('09c4495d-c31a-4800-867f-5a5afddb6a09', fixed_text).
narrative_ontology:cs_authority_grounding('09c4495d-c31a-4800-867f-5a5afddb6a09', lineage).
narrative_ontology:cs_interpretation_layer_present('09c4495d-c31a-4800-867f-5a5afddb6a09').
narrative_ontology:cs_reading_relation('09c4495d-c31a-4800-867f-5a5afddb6a09', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('09c4495d-c31a-4800-867f-5a5afddb6a09', us_constitution_1787__living_reading, coexists_with).
narrative_ontology:cs_axiom('09c4495d-c31a-4800-867f-5a5afddb6a09', foundational, text_is_interpretive_boundary).
narrative_ontology:cs_axiom_status(text_is_interpretive_boundary, holdable).
narrative_ontology:cs_axiom_grounding('09c4495d-c31a-4800-867f-5a5afddb6a09', text_is_interpretive_boundary, conventional).
narrative_ontology:cs_axiom('09c4495d-c31a-4800-867f-5a5afddb6a09', foundational, amendment_is_exclusive_change_mechanism).
narrative_ontology:cs_axiom_status(amendment_is_exclusive_change_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('09c4495d-c31a-4800-867f-5a5afddb6a09', amendment_is_exclusive_change_mechanism, deontological).
narrative_ontology:cs_reference_frame('09c4495d-c31a-4800-867f-5a5afddb6a09', text_plus_amendment_framework).
narrative_ontology:cs_drift_state('09c4495d-c31a-4800-867f-5a5afddb6a09', contemporary_amendment_drought, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('09c4495d-c31a-4800-867f-5a5afddb6a09', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, amendment_constituency).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, text_bound_judiciary).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, supermajority_gated_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The democratic polity (states, electoral constituencies) that ratifies constitutional amendments. Under the positivist reading, they hold primary authority over constitutional meaning. They benefit from the constraint's recognition of amendment as the legitimate change mechanism. Their exit option is to organize outside the Constitution (revolution, constitutional convention) or to change the amendment rule itself (a meta-amendment). They are mobile in that they can attempt amendments repeatedly; failure does not lock them out permanently.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, amendment_constituency, beneficiary,
    organized, generational, mobile, national).

% Federal courts, state courts, and the Supreme Court interpreting the Constitution under a text-bound methodology. They benefit from the constraint by gaining clear boundaries (interpret the text, not framers' intent or contemporary values) and reduced legitimacy pressure to rewrite meaning via doctrine. Their constraint: they cannot evolve meaning through interpretation—amendment is required. Their exit option is to drift toward judicial innovation (living constitutionalism) or originalist reconstruction (adding historical material), both of which violate the positivist constraint.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, text_bound_judiciary, beneficiary,
    institutional, generational, constrained, national).

% Groups seeking constitutional change: civil rights movements, labor organizers, regulatory reformers, progressive activists. They pay the cost of the supermajority requirement—they cannot win faster change through judicial reinterpretation or executive action. They must build consensus across state legislatures or hold a constitutional convention. Their exit options: work for amendment (high friction), use sub-constitutional statutory change (limited scope), or organize pressure for a constitutional convention (extremely high friction, rare).
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, supermajority_gated_reformers, payer,
    organized, generational, constrained, national).

% Judges and scholars committed to originalism—interpreting the Constitution according to its original public meaning at ratification. They are excluded from this reading's framework. They would argue that positivism is unstable because 'the text' itself requires historical context to be understood, and that amendment cannot be the ONLY source of evolution (the Constitution must evolve through proper interpretation). They resist the positivist reading's core claim that text-boundedness is possible without historical reconstruction.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, originalist_jurists, excluded,
    institutional, generational, constrained, national).

% Judges and scholars advocating evolutionary constitutional meaning—interpreting the Constitution as a living document that evolves with society. They are excluded from this reading's framework. They would argue that the positivist constraint is undemocratic because it locks meaning in until super-majorities form (slow, often impossible) and that the Constitution's text is deliberately aspirational, meant to evolve. They see the positivist reading as an elite-favoring constraint that prevents democratic responsiveness.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, living_constitutionalists, excluded,
    institutional, generational, constrained, national).

% The judiciary (Supreme Court, federal courts, state courts) as the institutional actor tasked with interpreting and applying the Constitution. Under the positivist reading, they are the designated authority for interpreting the settled text. They do not have the authority to amend (that belongs to the democratic amendment process) or to evolve meaning beyond the text. They set the agenda for constitutional litigation and interpretation, but operate under the constraint that they must respect textual limits.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, interpreting_authority, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__positivist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_1787__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, text-based frame for constitutional law that separates interpretation (judicial) from amendment (democratic). Solves the coordination problem of 'how do we change constitutional meaning legitimately without making law-without-amendment by stealth'—by anchoring meaning to explicit text and requiring supermajority democratic action for formal change.
% TRANSFER_FUNCTION: Transfers the authority to alter constitutional meaning from the judiciary (which retains interpretive power over existing text) to the amendment process (which requires supermajority consensus). Also transfers the burden of constitutional adaptation from courts to the democratic electorate, making change slower and requiring broader consensus.
% ABSENT_VOICES: Originalist jurists and living constitutionalists are structurally excluded—they dispute whether this reading's core claims (text-boundedness, amendment as primary mechanism) are correct. Non-amendment reformers (civil rights activists, labor movements) who believe judicial evolution is faster and more responsive would argue for recognition but are not seated. Their absence is partly structural (they are not the interpreting authority) and partly temporal (the amendment gating renders them less powerful in constitutional change).
% DISAPPEARANCE_RATIONALE: If the positivist constraint (text-bound judiciary + amendment-as-primary-change-mechanism) vanished, judicial interpretation would become the de facto constitutional amendment process. Courts would evolve meaning rapidly; the amendment mechanism would atrophy in use; constitutional change would become captured by whoever controls judicial appointment and doctrine. The entire distribution of authority over constitutional meaning would shift from the supermajority-gated democratic process to the judiciary.
% FOUNDING_PROBLEM: Constitutional government requires stability and legitimacy. In the early republic, the founding problem was: how do we prevent courts from rewriting the Constitution under cover of interpretation, while also allowing the Constitution to evolve when the people democratically decide it should? The positivist answer: text is stable; amendment is the legitimate update mechanism.
% FOUNDING_PROBLEM_CORROBORATION: The positivist reading itself attests the problem is live—textual instability threatens rule of law, and judicial discretion threatens democratic control. Originalists contest whether the problem is framed correctly (they see the real problem as fidelity to framers' intent). Living constitutionalists contest whether the solution works (they argue the amendment bar is so high that constitutional meaning becomes ossified). Independent constitutional scholars from outside the judiciary attest the tension is real: judicial activism does expand meaning beyond text, and amendment is indeed difficult. The founding problem is corroborated by the persistent doctrinal dispute itself.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).
:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the supermajority amendment requirement does impose real friction on reformers—they cannot use the faster judicial route. But it is not high extraction because (a) the constraint solves a genuine coordination problem (stable meaning), (b) the route to change is legitimate and open (democratic amendment), and (c) many beneficiaries (the amendment constituency, stable-law beneficiaries) gain from the constraint without being coerced. Suppression is low (0.28) because the constraint operates through rule-of-law clarity, not through force. Reformers know the path forward; they choose not to pursue it (or cannot build consensus) rather than being prevented by coercive machinery. Theater is low (0.19): the constraint's justification (stable meaning, democratic legitimacy) is functionally real, though over time as amendment becomes rarer, courts may drift toward interpretive evolution, raising theater slightly. The measurement series projects modest growth in extractiveness as the amendment bar hardens (fewer successful amendments create a backlog of desired-but-unratified changes) and suppression increases slightly (pressure for judicial innovation builds). Text-boundedness becomes increasingly performative as courts interpret 'text' more generously.
 *
 * PERSPECTIVAL GAP:
 *   From the amendment constituency's seat, this reading is a clear coordination win—it vests democratic authority where it belongs. From the supermajority-gated reformer's seat, it is a high-friction constraint that forces consensus-building and delays change. From the judiciary's seat, it is both clarity (knowing the boundaries) and constraint (unable to evolve meaning). Originalists see it as under-binding (ignoring framers' intent); living constitutionalists see it as over-binding (freezing meaning until amendment). The engine computes per-seat classifications; the authored metrics describe the constraint structurally, independent of which seat is reading it.
 *
 * DIRECTIONALITY LOGIC:
 *   Amendment constituency: high beneficiary (d near 0.0) — they control the constitutional update mechanism. Text-bound judiciary: moderate beneficiary (d ≈ 0.2–0.3) — they gain clarity and reduced legitimacy pressure, but remain bound and cannot innovate freely. Supermajority-gated reformers: target (d near 1.0 if trapped, constrained if they have extra-constitutional organizing paths) — they bear the cost of the high bar. No explicit override needed; the structural data (beneficiary declaration + beneficiary exit options vs. victim power/exit) derives accurate d automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The positivist reading avoids the mandatrophy trap by maintaining that the founding problem (stable meaning + democratic legitimacy) remains live. However, the founding_problem_status is marked contested because originalists argue the real problem is fidelity-to-intent, and living constitutionalists argue the real problem is constitutional ossification. The vanishing verdict (world_rearranges) confirms that if the positivist constraint vanished, the constitutional authority structure would reorganize completely. The potential mandatrophy scenario: if amendment becomes so rare that courts feel obliged to evolve meaning through interpretation (theater_ratio rising), the constraint's functional purpose (text-binding) erodes while its form (the amendment requirement) persists. This is tracked in the temporal measurements as theater_ratio rises slowly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_bar_vs_constitutional_ossification,
    'Is the Article V supermajority requirement the right height for constitutional evolution, or does it lock in outdated meaning faster than democratic values evolve?',
    'Long-term empirical tracking: Are amendment-resistant constitutional provisions preventing societies from adapting to changed circumstances? Do jurisdictions with lower amendment bars (like many state constitutions, or parliamentary systems) experience better democratic outcomes? Does the amendment rarity itself drive judicial creativity (theater_ratio rising)?',
    'If the amendment bar is too high, the positivist reading contributes to mandatrophy (amendment function atrophies, courts innovate to survive, theater_ratio → 1.0). If the bar is roughly correct, the constraint coordinates stable meaning and democratic responsibility as intended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_bar_vs_constitutional_ossification, empirical, 'Whether the amendment supermajority correctly balances stability and responsiveness.').

omega_variable(
    text_boundedness_under_interpretive_pressure,
    'Can judicial interpretation of ''the text'' truly remain stable, or does the meaning of textual language itself evolve with usage, context, and social understanding, making pure text-boundedness impossible?',
    'Jurisprudential analysis: Examine cases where the same constitutional text is interpreted differently across decades with no formal amendment—does the ''text'' meaning shift? Does the constraint require courts to actively deny that linguistic meaning evolves?',
    'If text meaning cannot remain stable without interpretive denial, the positivist reading relies on a performative fiction (theater_ratio increases toward 0.5+). If text can be held stable (via originalism or formalist semantics), the reading is more coherent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(text_boundedness_under_interpretive_pressure, conceptual, 'Whether ''text-boundedness'' is a stable interpretive fact or a sustained interpretive position.').

omega_variable(
    sibling_reading_foreclosure,
    'Do the positivist reading''s core axioms foreclose the originalist and living readings, or are they genuinely coexisting frameworks that could each be instantiated in different institutional contexts?',
    'Structural analysis of axioms: If the positivist axiom ''text-plus-amendment is the ONLY legitimate source of meaning'' is true, then originalism (which adds framers'' intent) and living constitutionalism (which adds evolved values) are foreclosed. If the axiom is ''text-plus-amendment is ONE legitimate source,'' coexistence is maintained.',
    'If axioms foreclose, classify the relationship as forecloses in cs_structure.reading_relations. If coexistent, classify as coexists_with. If positivism creates structural pressure that influences but does not foreclose, classify as influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationships between sibling readings of the constitutional kernel.').

omega_variable(
    democratic_amendment_failure_cascade,
    'If amendment becomes politically impossible (supermajority never forms), does the positivist constraint collapse into pure text-boundedness without democratic authority, making the reading unstable?',
    'Historical tracking: Periods of amendment drought (1865–1913, 1971–present). Do courts during these periods adopt different interpretive strategies, and does the constraint''s legitimacy erode?',
    'If amendment drought breaks the democratic half of the constraint, the reading reverts to pure text-boundedness (high accessibility_collapse, high resistance from reformers, high theater as courts work around the constraint). The constraint might reclassify toward snare (text-boundedness without democratic amendment becomes an elite-favoring rule with no democratic legitimacy channel).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_amendment_failure_cascade, empirical, 'Stability of the positivist reading if amendment becomes politically impossible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__positivist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(us_c_tr_t0, projected).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__positivist_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(us_c_tr_t10, projected).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__positivist_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(us_c_tr_t20, projected).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_1787__positivist_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t30, projected).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__positivist_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement_basis(us_c_tr_t40, projected).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_1787__positivist_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement_basis(us_c_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__positivist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(us_c_be_t0, projected).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__positivist_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(us_c_be_t10, projected).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__positivist_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement_basis(us_c_be_t20, projected).
narrative_ontology:measurement(us_c_be_t30, us_constitution_1787__positivist_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(us_c_be_t30, projected).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__positivist_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement_basis(us_c_be_t40, projected).
narrative_ontology:measurement(us_c_be_t50, us_constitution_1787__positivist_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement_basis(us_c_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__positivist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(us_c_su_t0, projected).
narrative_ontology:measurement(us_c_su_t10, us_constitution_1787__positivist_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement_basis(us_c_su_t10, projected).
narrative_ontology:measurement(us_c_su_t20, us_constitution_1787__positivist_reading, suppression_requirement, 20, 0.23).
narrative_ontology:measurement_basis(us_c_su_t20, projected).
narrative_ontology:measurement(us_c_su_t30, us_constitution_1787__positivist_reading, suppression_requirement, 30, 0.26).
narrative_ontology:measurement_basis(us_c_su_t30, projected).
narrative_ontology:measurement(us_c_su_t40, us_constitution_1787__positivist_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(us_c_su_t40, projected).
narrative_ontology:measurement(us_c_su_t50, us_constitution_1787__positivist_reading, suppression_requirement, 50, 0.28).
narrative_ontology:measurement_basis(us_c_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__positivist_reading, 0.18).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).

% DUAL FORMULATION NOTE:
% The 'us_constitution_1787' kernel generates three separate constraint stories: positivist_reading (this story), originalist_reading (text + framers' intent), and living_reading (text + evolved values). Each story is ε-invariant: the positivist reading's ε (0.42 extractiveness from the amendment supermajority) is structurally distinct from the originalist reading's ε (depends on historical reconstruction precision) and the living reading's ε (depends on judicial discretion degree). The three readings coexist as live positions in American constitutional law and are linked via network.affects_constraints to show family kinship and mutual structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
