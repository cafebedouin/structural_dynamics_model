% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/jurisprudence/political_theory
 *
 * SUMMARY:
 *   Judicial supremacy in constitutional interpretation is one reading of a
 *   deeply contested kernel: who possesses final authority to interpret the
 *   constitution? This reading holds that courts, as guardians of
 *   constitutional and fundamental rights, possess the power to nullify
 *   legislative acts that violate constitutional bounds. The constraint
 *   exhibits classical Tangled Rope structure: genuine coordination function
 *   (preventing majoritarian tyranny) coupled with asymmetric extraction
 *   (subordination of elected branches and empowerment of unelected
 *   judiciary). The reading instantiates a specific constitutional ideology
 *   that has dominated American jurisprudence since Marbury v. Madison (1803)
 *   but remains fiercely contested in comparative constitutional law and
 *   political philosophy. The constraint is simultaneously defended as
 *   necessary protection of fundamental rights and criticized as illegitimate
 *   usurpation of democratic authority. Its ε value (0.58) reflects the
 *   mixture: real benefits for rights protection and judicial capacity to
 *   resolve constitutional disputes, coupled with real extraction costs in
 *   the subordination of legislative authority and entrenchment of unelected
 *   power.
 *
 * KEY AGENTS:
 *   - Judicial Branch: Primary beneficiary (institutional/arbitrage) — gains final interpretive authority, legitimated by rights-guardian role; can reinterpret constitution without legislative override
 *   - Legislative Branch: Primary victim (powerless/trapped) — legislative acts subject to nullification; no exit mechanism except constitutional amendment (supermajority + ratification, extraordinarily expensive)
 *   - Democratic Majorities: Secondary victim (moderate/constrained) — majoritarian legislative choices subject to judicial veto; exit requires constitutional amendment
 *   - Rights-Bearing Individuals: Mixed (powerful/mobile) — benefit from fundamental rights protection but depend on courts for vindication and face undemocratic judicial selection
 *   - Constitutional Tradition (Judicial Mythos): Institutional actor (institutional/arbitrage) — maintains performative narrative of fidelity to founding documents while continuously reinterpreting constitutional meaning
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a choice (judicial supremacy) as a necessity (inevitable solution to constitutional interpretation problem)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.62).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/jurisprudence/political_theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, '78ea393a-c3ee-48af-9ba1-a440b6bfa8e6').
narrative_ontology:cs_kernel_codification('78ea393a-c3ee-48af-9ba1-a440b6bfa8e6', formalized).
narrative_ontology:cs_authority_grounding('78ea393a-c3ee-48af-9ba1-a440b6bfa8e6', lineage).
narrative_ontology:cs_interpretation_layer_present('78ea393a-c3ee-48af-9ba1-a440b6bfa8e6').
narrative_ontology:cs_reading_relation('78ea393a-c3ee-48af-9ba1-a440b6bfa8e6', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('78ea393a-c3ee-48af-9ba1-a440b6bfa8e6', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('78ea393a-c3ee-48af-9ba1-a440b6bfa8e6', foundational, courts_as_fundamental_rights_guardians).
narrative_ontology:cs_axiom_status(courts_as_fundamental_rights_guardians, holdable).
narrative_ontology:cs_axiom_grounding('78ea393a-c3ee-48af-9ba1-a440b6bfa8e6', courts_as_fundamental_rights_guardians, deontological).
narrative_ontology:cs_axiom('78ea393a-c3ee-48af-9ba1-a440b6bfa8e6', foundational, majoritarian_tyranny_preventable_by_judicial_review).
narrative_ontology:cs_axiom_status(majoritarian_tyranny_preventable_by_judicial_review, holdable).
narrative_ontology:cs_axiom_grounding('78ea393a-c3ee-48af-9ba1-a440b6bfa8e6', majoritarian_tyranny_preventable_by_judicial_review, empirically_contingent).
narrative_ontology:cs_reference_frame('78ea393a-c3ee-48af-9ba1-a440b6bfa8e6', constitutional_supremacy_via_judicial_guardianship).
narrative_ontology:cs_drift_state('78ea393a-c3ee-48af-9ba1-a440b6bfa8e6', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('78ea393a-c3ee-48af-9ba1-a440b6bfa8e6', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, judicial_branch).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_authority).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, majoritarian_will).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATED LEGISLATURE (SNARE) — Elected representatives face constitutional nullification of legislative acts by an unelected judiciary. No exit mechanism: legislative supremacy is foreclosed by the reading's own axioms. Maximum experienced extraction — legislative authority is structurally subordinated to judicial review without recourse or override capacity. Legislature bears the cost of constraint while judiciary holds final authority.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__judicial_supremacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEMOCRATIC MAJORITY (SNARE) — Elected majorities face structural nullification of their legislative choices by courts. Exit is constrained: formal amendment is extraordinarily difficult (supermajority + ratification), and informal workarounds (constitutional reinterpretation) are blocked by the reading's commitment to judicial finality. The majoritarian will is subordinated to judicial interpretation of fundamental rights. High experienced extraction — majority preferences are overridden without democratic consent.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__judicial_supremacy_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIAL BRANCH (ROPE) — Courts experience this constraint as coordination of the constitutional system: judicial review enables courts to interpret constitutional meaning and prevent legislative overreach. Net beneficiary — the constraint legitimates judicial authority and provides arbitrage: courts can reinterpret constitutional text without legislative override. The reading classifies as Rope from the judiciary's perspective because they benefit from the coordination function (preventing tyranny of majority) while experiencing low extraction cost.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__judicial_supremacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RIGHTS-BEARING INDIVIDUALS (TANGLED ROPE) — Individuals gain protection of fundamental rights through judicial review — a genuine coordination benefit. However, they also experience extraction: judicial selection is undemocratic, courts can impose narrow readings of rights that serve institutional interests, and individual rights claimants depend on courts for vindication with no alternative remedy. Mixed experience — real benefit (rights protection) coupled with extraction (undemocratic authority and dependency).
constraint_indexing:constraint_classification(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZED LEGISLATIVE SUPERMAJORITIES (TANGLED ROPE) — When supermajorities organize (as in constitutional amendment), they retain capacity to override judicial interpretation. This is genuine coordination: the amendment process prevents a simple majority from tyrannizing minorities, and judicial review prevents tyranny of the majority. But the constraint also enforces extraction: the supermajority threshold is extraordinarily high, requiring 34+ states to ratify in the US context. Organized actors have some agency but face extreme costs.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL MYTHOS (PITON) — The reading's authority derives from a continuous narrative of judicial guardianship — an interpretive tradition claiming unbroken lineage from founding documents and judicial precedent. This narrative is substantially performative: each generation rewrites constitutional meaning through reinterpretation while claiming fidelity to original intent. The theater persists because alternative framings (parliamentary supremacy, coordinate construction) challenge the mythos but have not displaced it institutionally. High theater ratio reflects the gap between the narrative of immutable constitutional limits and the reality of continuous judicial remaking of constitutional meaning.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__judicial_supremacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some form of final interpretive authority is structurally necessary: a constitution must be interpreted, and someone must have authority to resolve disputes. This perspective treats judicial supremacy as a natural solution to an inevitable problem — the coordination problem of constitutional meaning-fixing. However, this naturalizes a choice: other solutions exist (parliamentary supremacy, coordinate construction). The engine will compute this as a false summit.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__judicial_supremacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_interpretive_authority__judicial_supremacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_interpretive_authority__judicial_supremacy_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from the legislature and majoritarian will: legislative nullification is a direct subordination of elected authority, and the judiciary captures the power to reinterpret constitutional meaning without legislative consent. However, the extraction is not maximal (snare-level, ≥0.66) because genuine coordination functions exist. Rights protection is real — fundamental rights beneficiaries experience genuine benefits. The increase in ε over the interval (0.32 → 0.58) reflects historical accumulation of judicial power: the scope of nullification authority has expanded over 100+ years, and courts have increasingly used interpretive authority to expand their own institutional domain (civil rights, structural constitutional questions). Suppression (0.62): Moderate-high. The legislature and majorities face structural barriers to override: constitutional amendment is extraordinarily difficult (US: 2/3 of both chambers + 3/4 of state legislatures), and informal workarounds (reinterpretation) are blocked by the reading's commitment to judicial finality. Suppression increases over the interval as the supermajority threshold becomes more entrenched and political polarization makes amendment even more difficult. Theater ratio (0.48): Moderate-low. The reading contains less pure theater than the piton perspective suggests: there is genuine reasoning about constitutional meaning, real disagreement about correct interpretation, and significant institutional operation. However, some theater is present: courts claim fidelity to original intent while continuously reinterpreting that intent; the narrative of 'courts are guardian of rights' masks the reality that courts often expand their own power while protecting rights; constitutional meaning shifts with changing judicial composition, revealing that interpretation is party to political contestation. The theater ratio increases modestly over time as the performative element of the mythos becomes more visible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a striking perspectival gap between institutional actors and democratic agents. The judiciary experiences Rope — coordination of constitutional meaning and prevention of tyranny. Elected legislatures experience Snare — structural subordination with no exit. Democratic majorities experience Snare — override of their will without consent. Rights-claimants experience Tangled Rope — genuine protection coupled with dependence on unelected authority. The piton perspective reveals the theater: the institutional mythos of judicial guardianship masks continuous reinterpretation and power expansion. The analytical observer risks seeing a natural law (some authority must interpret the constitution) while missing the reading's particularity: other constitutional systems (parliamentary, coordinate construction) resolve the interpretation problem differently. The gap reveals that 'judicial supremacy' is not a discovery of constitutional logic but a institutional choice legitimated through specific narrative claims (rights protection, constitutional fidelity) that mask power dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from the agent's structural position relative to this specific constraint. The judicial branch (institutional + arbitrage) derives low d (≈0.15) — it is a net beneficiary with high exit options (can choose not to exercise nullification power). The legislature (powerless + trapped) derives high d (≈0.95) — it is a net victim with no exit. Democratic majorities (moderate + constrained) derive d ≈0.70 — moderate victims with constrained exit (amendment is possible but extraordinarily expensive). Rights-claimants (powerful + mobile) derive d ≈0.55 — they benefit from the coordination function but depend on courts and face undemocratic selection. The piton perspective (institutional + arbitrage) derives low d because it sees the constraint as self-referential — the institutional structure benefits from its own narrative, revealing low genuine extraction when the theater is accounted for. The analytical perspective derives d ≈0.72 (canonical for analytical), reflecting the observer's position outside the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying its structure as Tangled Rope rather than pure extraction (Snare). The mandatrophy question is: 'Does judicial supremacy coordinate the constitutional system (preventing tyranny) or extract authority from democratic branches?' The answer is both. Judicial review genuinely prevents majoritarian tyranny — a coordination function that saves lives and protects minorities. Simultaneously, courts use this function to extract authority: the scope of rights expands, new constitutional doctrines emerge that benefit judicial power, and courts become party to political contestation while claiming neutrality. The Tangled Rope classification captures this mixture without collapsing one aspect into the other. The extraction is not predatory (it serves the coordination goal of rights protection), but it is extraction nonetheless — the subordination of elected branches and empowerment of unelected courts. A snare reading would require showing that the rights-protection function is primarily pretext. A rope reading would require showing no genuine subordination. The tangled rope reading holds both real benefits and real costs in stable tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_capacity_constraint,
    'Does the judiciary possess sufficient epistemic and institutional capacity to interpret complex constitutional questions with legitimacy comparable to democratic legislatures?',
    'Comparative analysis of judicial vs legislative reasoning on constitutional matters; historical record of judicial overreach vs legislative deference; empirical study of judicial competence on technical/policy questions',
    'If judicial capacity is demonstrably lower: the extraction mechanism is partly self-inflicted cognitive limitation, not justified by superior rights-protection. If comparable or higher: legitimacy of judicial authority strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_capacity_constraint, empirical, 'Whether courts possess sufficient institutional competence for constitutional interpretation').

omega_variable(
    fundamental_rights_necessity_test,
    'What constitutional protections (fundamental rights) actually require judicial enforcement beyond legislative self-restraint? Which are mere judicial preferences dressed as rights?',
    'Historical analysis of judicial nullifications: which overturned laws actually violated inalienable rights vs which reflected judicial policy preferences? Comparative study of parliamentary democracies: do legislatures genuinely violate fundamental rights absent judicial review, or do other mechanisms (public scrutiny, minority coalition-building, institutional culture) prevent abuse?',
    'If most nullifications protect genuine fundamental rights: judicial supremacy''s benefit is real and high. If many reflect policy preferences: the reading naturalizes judicial preferences as rights-protection, and extraction increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamental_rights_necessity_test, empirical, 'Necessity of judicial review for protecting fundamental rights').

omega_variable(
    counter_majoritarian_legitimacy,
    'What legitimizes unelected courts to override elected majorities? Is it fidelity to constitutional text, or is it an implicit claim to moral/epistemic superiority?',
    'Textual analysis: does the constitution explicitly authorize judicial nullification, or is it implied/inferred? Comparative jurisprudence: how do different constitutional systems justify judicial override? Historical genealogy: when did judicial supremacy become doctrine, and what occasioned the shift?',
    'If legitimacy derives from explicit constitutional text: the reading is grounded in the kernel itself. If derived from implied authority or judicial arrogation: the reading instantiates extraction of authority not granted by the constitutional text, increasing ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counter_majoritarian_legitimacy, conceptual, 'Legitimacy basis for judicial override of elected branches').

omega_variable(
    sibling_reading_foreclosure,
    'Does judicial supremacy logically foreclose parliamentary supremacy and coordinate construction, or do these readings coexist as live alternatives held by different political actors?',
    'Logical analysis: are the core axioms of judicial supremacy (courts are final arbiter of constitutional meaning) incompatible with parliamentary supremacy (legislatures are final arbiter)? Political history: have both readings been actively maintained in jurisprudence or only sequentially?',
    'If foreclosed: judicial supremacy is unique/necessary reading. If coexist: the constraint operates in a field where three reading are simultaneously live, and judicial supremacy''s supremacy is institutional rather than logical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether judicial supremacy logically forecloses alternative readings').

omega_variable(
    extraction_vs_coordination_decomposition,
    'Can the constraint be decomposed into separate constitutional stories: one capturing genuine coordination function (preventing arbitrary majorities from violating fundamental rights) and one capturing extraction mechanism (judiciary using interpretation to expand its own authority)?',
    'Historical and structural analysis: which judicial nullifications protect genuine fundamental rights vs which expand judicial power relative to legislature? Counterfactual analysis: would the same rights protections be achievable through parliamentary safeguards or supermajority procedures?',
    'If decomposable: the ε value represents a mixture. Pure coordination component might have lower ε (Rope); pure extraction component might have higher ε (Snare). Current ε=0.58 is the mixture. If not decomposable: Tangled Rope classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_decomposition, empirical, 'Whether the constraint can be decomposed into separate coordination and extraction mechanisms').

omega_variable(
    reading_identity_and_enforcement,
    'Is judicial supremacy a reading that requires active enforcement (courts actively nullifying legislation) or a reading that merely requires institutional architecture (courts empowered to nullify if they choose)?',
    'Empirical study: how frequently do courts actually exercise nullification power? In periods of high judicial restraint, is the reading still operative or does it become dormant? Does the reading''s force depend on active enforcement or on the threat of enforcement?',
    'If enforcement-dependent: suppression drops when courts exercise restraint, and the constraint phases between Tangled Rope and Rope. If architecture-dependent: suppression remains stable as long as nullification power formally exists, regardless of use.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_and_enforcement, empirical, 'Whether judicial supremacy depends on active enforcement or institutional capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(judic_interp_tr_t0, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(judic_interp_tr_t50, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 50, 0.43).
narrative_ontology:measurement(judic_interp_tr_t100, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(judic_interp_be_t0, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(judic_interp_be_t50, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(judic_interp_be_t100, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(judic_interp_su_t0, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(judic_interp_su_t50, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(judic_interp_su_t100, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'constitutional_interpretive_authority.' The sibling readings (parliamentary_supremacy_reading, coordinate_construction_reading) are separate constraint stories with different ε values, beneficiary/victim structures, and classifications. All three readings share the same kernel but instantiate different constitutional ideologies with different structural consequences. The network links them as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
