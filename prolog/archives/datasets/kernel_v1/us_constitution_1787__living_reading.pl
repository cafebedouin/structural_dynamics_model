% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: US Constitution (Living Reading): Aspirational Framework with Evolving Meaning
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   The living reading of the US Constitution construes the 1787 text as an
 *   aspirational framework whose meaning evolves with society. This reading
 *   is the dominant interpretive paradigm in contemporary US legal
 *   institutions (law schools, progressive judiciary, major reform
 *   coalitions), though it coexists with the originalist reading held by a
 *   powerful opposition and the legal positivist reading held by a smaller
 *   academic cadre. The living reading permits constitutional meaning to
 *   expand without formal amendment — allowing claims of privacy, dignity,
 *   and equal protection to be read into the text despite the Framers' lack
 *   of explicit commitment to these concepts. As a constraint, the living
 *   reading exhibits tangled rope structure: it genuinely enables
 *   coordination (progressive reform coalitions can mobilize constitutional
 *   authority; marginalized groups can appeal to aspirational values; judges
 *   can adapt the law to social change) while simultaneously enabling
 *   extraction (judicial authority over meaning expands; textual stability is
 *   sacrificed; the amendment process is bypassed; originalists and
 *   federalist constitutionalists are suppressed). The extractiveness has
 *   risen over time (0.28 → 0.52) as interpretive discretion became
 *   institutionalized, while theater ratio has increased (0.30 → 0.64) as the
 *   appearance of textual constraint masks the reality of discretionary
 *   power. Suppression requirement rose and has stabilized (0.15 → 0.52 →
 *   0.48) as originalism became an organized alternative that must be
 *   actively suppressed to maintain institutional dominance of the living
 *   reading.
 *
 * KEY AGENTS:
 *   - Progressive Reform Coalitions: Primary beneficiary (institutional/arbitrage) — can mobilize constitutional authority for social change without amendment; includes NAACP, civil rights organizations, climate advocates, LGBTQ+ rights groups
 *   - Marginalized Identity Groups: Secondary beneficiary (moderate to organized/constrained) — can appeal to Constitution's aspirational values; women, racial minorities, LGBTQ+ persons, disabled persons
 *   - Supreme Court Justices: Institutional interpreters (institutional/constrained) — gain interpretive authority and doctrinal flexibility; constrained by legitimacy requirement to stay within text
 *   - Legal Academia (Law Schools, Law Reviews): Institutional beneficiary/enforcer (institutional/arbitrage) — living reading is orthodoxy in legal theory; originalism is subordinate alternative; shapes professional culture
 *   - Originalist Movement: Constrained opposition (powerful/constrained) — Federalist Society, originalist judges (Scalia, Thomas, Alito), conservative legal organizations; suppressed in elite legal culture but organized
 *   - Constitutional Stability (Abstract Principle): Victim (powerless/trapped) — the principle that text can constrain power is sacrificed; once meaning is fluid, the document loses constraining force
 *   - Federalist Constitutional Limits: Victim (powerless/trapped) — the idea that Constitution limits federal power through enumerated powers is eroded; living reading expands federal reach through expansive interpretation
 *   - Amendment Process: Degraded alternative (institutional/arbitrage) — formal amendment mechanism atrophied in favor of judicial reinterpretation (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.52).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.48).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "US Constitution (Living Reading): Aspirational Framework with Evolving Meaning").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, '8583adf9-aa50-4d3a-a2dd-9de4dce45c4b').
narrative_ontology:cs_kernel_codification('8583adf9-aa50-4d3a-a2dd-9de4dce45c4b', fixed_text).
narrative_ontology:cs_authority_grounding('8583adf9-aa50-4d3a-a2dd-9de4dce45c4b', lineage).
narrative_ontology:cs_interpretation_layer_present('8583adf9-aa50-4d3a-a2dd-9de4dce45c4b').
narrative_ontology:cs_reading_relation('8583adf9-aa50-4d3a-a2dd-9de4dce45c4b', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8583adf9-aa50-4d3a-a2dd-9de4dce45c4b', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('8583adf9-aa50-4d3a-a2dd-9de4dce45c4b', foundational, meaning_follows_society).
narrative_ontology:cs_axiom_status(meaning_follows_society, holdable).
narrative_ontology:cs_axiom_grounding('8583adf9-aa50-4d3a-a2dd-9de4dce45c4b', meaning_follows_society, empirically_contingent).
narrative_ontology:cs_axiom('8583adf9-aa50-4d3a-a2dd-9de4dce45c4b', secondary, aspiration_enables_inclusion).
narrative_ontology:cs_axiom_status(aspiration_enables_inclusion, holdable).
narrative_ontology:cs_axiom_grounding('8583adf9-aa50-4d3a-a2dd-9de4dce45c4b', aspiration_enables_inclusion, deontological).
narrative_ontology:cs_reference_frame('8583adf9-aa50-4d3a-a2dd-9de4dce45c4b', evolutionary_legitimacy_framework).
narrative_ontology:cs_drift_state('8583adf9-aa50-4d3a-a2dd-9de4dce45c4b', contemporary_us_legal_culture, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8583adf9-aa50-4d3a-a2dd-9de4dce45c4b', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, progressive_reform_coalitions).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, marginalized_identity_groups).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, judicial_interpreters).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, textual_stability).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, original_intent_adherents).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, federalist_constitutional_limits).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TEXTUAL STABILITY (SNARE) — The constraint that constitutional meaning evolves traps the principle of stable legal text. Once meaning becomes fluid, the document cannot constrain power — it can only be reinterpreted by whoever controls the interpretive apparatus. Trapped in the sense that stable text has no exit: the moment the living reading dominates, fixed meaning is abandoned. The extractiveness is maximal for stability as an abstract principle — it is fully sacrificed to the evolving-meaning apparatus.
constraint_indexing:constraint_classification(us_constitution_1787__living_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORIGINALIST ADHERENTS (TANGLED ROPE) — Originalists are constrained by the living reading's dominance in elite judicial and legal education circles, yet they also benefit from the constitutional framework itself — they have professional platforms, funding from conservative institutions, and occasional judicial victories that validate their methodology. High suppression of their ability to establish fixed meaning; moderate extraction through institutional pressure; some coordination benefit through legal ecosystem. Exit is costly but possible (alternative jurisprudential communities, international law circles).
constraint_indexing:constraint_classification(us_constitution_1787__living_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROGRESSIVE REFORM COALITIONS (ROPE) — The living reading benefits reformers by allowing constitutional text to accommodate new rights claims (privacy, dignity, equal protection expanded scope) without formal amendment. Beneficiaries experience the constraint as pure coordination: the ability to interpret the Constitution as aspirational enables collective action toward progressive ends. Arbitrage options abound — they can shift between legislative, judicial, and social movements. Net beneficiary position with minimal experienced extraction.
constraint_indexing:constraint_classification(us_constitution_1787__living_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MARGINALIZED IDENTITY GROUPS (ROPE) — Historically excluded groups (racial minorities, women, LGBTQ+ persons) benefit from the living reading: they can appeal to the Constitution's aspirational values rather than waiting for formal amendment. The constraint enables their inclusion claims. Arbitrage available through legislative advocacy, voting, coalition building. Experiences constraint as coordination mechanism enabling voice — low suppression, low extraction, high benefit.
constraint_indexing:constraint_classification(us_constitution_1787__living_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPREME COURT JUSTICES (TANGLED ROPE) — The living reading gives judges enormous interpretive discretion (benefit: power and doctrinal flexibility), but also constrains them through legitimacy requirements — they must justify evolving meanings as flowing from the text rather than imposing policy preferences (suppression: limited by staying-within-text requirement). Constrained exit (removal via impeachment is rare; they must maintain institutional legitimacy). Mixed coordination-extraction: they coordinate constitutional meaning while extracting interpretive authority.
constraint_indexing:constraint_classification(us_constitution_1787__living_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL AMENDMENT PROCESS (PITON) — The formal amendment process (Article V) is the original mechanism for updating constitutional meaning. The living reading has effectively superseded Article V by allowing judicial reinterpretation without supermajority consensus. The amendment process persists as an institutional structure (people still know about it, it occasionally functions) but its primary function has atrophied — it is now a backup mechanism rarely invoked. Theater ratio high: the amendment process is ritually invoked but materially bypassed. Classification: piton (degraded mechanism maintained through inertia).
constraint_indexing:constraint_classification(us_constitution_1787__living_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CIVILIZATIONAL NECESSITY (MOUNTAIN) — From a civilizational scale, the living reading appears to be a natural law: all foundational texts eventually require reinterpretation as societies evolve. No text survives unchanged across centuries without becoming irrelevant. Meaning must evolve or law becomes ossified. This perspective risks naturalizing the living reading as inevitable rather than examining it as a contested institutional choice. The engine's false summit detector should flag this: the reading is benefiting specific coalitions (progressive reformers, marginalized groups, expansive judicial authority) and harming others (constitutional stability, originalist jurisprudence, federalist limits). Presentational claim of inevitability masks distributional effects.
constraint_indexing:constraint_classification(us_constitution_1787__living_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution_1787__living_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution_1787__living_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__living_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution_1787__living_reading, TR),
    TR >= 0.70.

:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The living reading produces genuine coordination benefits (progressive reform coalitions can use constitutional authority; marginalized groups gain interpretive standing) but also extracts substantial benefits for judges and legal elites. The extraction is not maximal (snare level, 0.66+) because the coordination function is real — there is genuine dispute resolution and legitimate collective action enabled. But extractiveness is higher than rope (0.35) because the constraint concentrates interpretive authority with judges and elite legal actors while suppressing alternatives (originalism, positivism, federalism). Temporal progression (0.28 → 0.52) reflects institutionalization: as the living reading became the established paradigm, extraction increased because the discretionary authority it grants became normalized. Suppression (0.48): Moderate-high. Originalist and federalist alternatives must be actively suppressed in law school curricula, legal hiring, and appellate judiciaries to maintain living reading dominance. However, suppression is not at maximum (snare level, 0.60+) because originalism has organized intellectual infrastructure (Federalist Society, originalist scholars, conservative courts) that prevents complete suppression. Suppression requirement rose from 0.15 to 0.52 as originalism became intellectually sophisticated opposition, then declined slightly (0.48) as originalism gained some institutional traction (originalist judges appointed; originalist scholarship gained academic respectability). Theater ratio (0.64): High. Constitutional interpretation under the living reading performs textual fidelity while exercising substantial discretion. Judges must invoke the text and argue their interpretations flow from it, even when the interpretations reflect evolving values or policy preferences. The performative element increased over time (0.30 → 0.64) as the interpretive apparatus matured — early living reading was more explicit about discretion; contemporary living reading is more theatrical, framing discretionary decisions as inevitable evolutions of constitutional meaning. Claimed type (Tangled Rope): The constraint exhibits both genuine coordination (reform coalitions mobilize constitutional authority; marginalized groups gain voice; legal disputes are resolved through constitutional argument rather than purely through politics) and asymmetric extraction (judges gain authority; textual stability is sacrificed; alternatives are suppressed). Tangled rope is the appropriate classification because neither element can be removed without destroying the constraint's function.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is enormous. Progressive reformers see Rope: the living reading is a coordination mechanism that enables them to mobilize constitutional authority for social change without amendment. Marginal groups see Rope or Tangled Rope: they gain interpretive standing and voice, though with extraction via judicial discretion. Supreme Court justices see Tangled Rope: they coordinate constitutional meaning while extracting authority. Originalists see Snare: they are trapped by institutional dominance of the living reading; they have powerful intellectual arguments but constrained ability to make them prevail in elite legal spaces. The amendment process sees itself as Piton: a degraded mechanism. Textual stability and federalist limits see Snare: they are sacrificed with no exit. The analytical observer at civilizational scale risks seeing Mountain: constitutional evolution is a natural law of legal systems. The false summit detector should flag this — the mountain classification naturalizes what is actually a contested institutional arrangement that benefits identifiable coalitions and harms others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the constraint. Progressive reform coalitions are beneficiaries with arbitrage options (high institutional power, mobile exit — they can shift between courts, legislatures, social movements): d ≈ 0.15 (low). Supreme Court justices are beneficiaries with constrained exit (institutional power, but constrained by legitimacy requirement): d ≈ 0.35 (moderate-low). Originalists are victims with constrained exit (powerful institutional position, but constrained by legal culture that treats living reading as orthodox): d ≈ 0.55 (moderate-high). Textual stability and federalist constitutional limits are victims with no exit (powerless abstract principles, trapped by the definition of what 'Constitution' means under the living reading): d ≈ 0.90 (very high). The engine derives these d values automatically from the beneficiary/victim declarations and exit options; they feed the directionality sigmoid f(d) to compute experienced extractiveness chi for each perspective. The result: beneficiaries experience low or negative chi (they see rope); victims experience high chi (they see snare or high extraction); mixed agents experience intermediate chi (they see tangled rope). The perspectival gap is the gap in d values across the perspectives — the fact that the same constraint produces radically different experienced extractiveness depending on where you stand.
 *
 * MANDATROPHY ANALYSIS:
 *   The living reading resolves the mandatrophy by showing that the constraint is genuinely tangled: it coordinates constitutional meaning (judges and parties can resolve disputes through constitutional argument; social change can be achieved without formal amendment) while extracting authority (judges gain discretionary power; textual stability is sacrificed; alternatives are suppressed). The constraint cannot be reduced to either pure coordination (rope) or pure extraction (snare) because both elements are structural. The perspectival gap reveals the mandatrophy: beneficiaries see coordination, victims see extraction, and the constraint enables both simultaneously. The classification resists mandate because it is mandatorily hybrid — the living reading is not a clever disguise for extraction, nor is it a pure coordination mechanism. It is a mechanism that genuinely enables some forms of coordination while enabling other forms of extraction. Which parties gain and which lose depends on their structural position, and the perspectival analysis maps the gains and losses.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_discretion_escape_valve,
    'Is the living reading a mechanism for adaptive constitutional evolution, or is it a discretionary escape valve that allows judges to implement policy preferences while maintaining the appearance of textual fidelity?',
    'Empirical analysis of Supreme Court decisions: correlation between ''evolving meaning'' justifications and (a) consistency with prior precedent, (b) alignment with clearly stated public preferences, (c) predictability of outcomes. If correlation with predictable doctrinal logic is high, evolution is genuine; if correlation with justice voting blocs or policy outcomes is high, escape-valve hypothesis confirmed.',
    'If escape valve: reclassify toward snare (judges extracting authority without accountability). If adaptive evolution: tangled rope confirmed (real coordination with distributed extraction). Classification depends critically on whether judges are constrained by textual reasoning or constrained only by legitimacy optics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_escape_valve, empirical, 'Whether living reading enables genuine adaptive evolution or discretionary escape valve').

omega_variable(
    reform_coalition_capture_risk,
    'As the living reading becomes the dominant interpretive paradigm, does it create an institutional position that enables new forms of elite capture — where ''evolving norms'' become synonymous with preferences of dominant cultural and economic coalitions rather than genuinely plural society?',
    'Analysis of whose ''evolving values'' courts recognize: temporal mapping of rights expansions against (a) actual demographic/cultural shift vs (b) articulation by institutional reform coalitions and legal academia. If elite-led values precede or contradict grassroots change, capture hypothesis gains traction.',
    'If capture occurs: the beneficiary position of progressive coalitions is vulnerable — the constraint becomes a mechanism through which a new dominant class extracts normative authority. Reclassifies toward snare or higher-extraction tangled rope as institutional judges begin serving elite consensus rather than pluralistic input.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_coalition_capture_risk, empirical, 'Risk of elite capture via ''evolving norms'' language').

omega_variable(
    originalist_sibling_foreclosure,
    'Does the living reading logically foreclose the originalist reading within a single coherent constitutional framework, or do the readings coexist as competing institutional positions held by different parties?',
    'Logical analysis: originalism claims the text has a fixed public meaning at ratification; living reading claims meaning evolves with society. Can both be true in the same framework? Only if one party is simply wrong. But both readings survive in US constitutional discourse because institutional contexts permit them (different justices, different lower courts, different academic fields). Coexistence suggests neither forecloses — they are held by different institutional actors with conflicting interests.',
    'If forecloses: originalism would be structurally impossible under living reading dominance — the reading should prevent originalist arguments from having any coherent standing. In practice, originalism remains live, suggesting coexistence relation is more accurate. If coexistence confirmed: the true constraining mechanism is institutional power distribution, not logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalist_sibling_foreclosure, conceptual, 'Whether living reading forecloses originalism or they coexist as competing positions').

omega_variable(
    aspiration_vs_aspiration_capture,
    'When the living reading reframes the Constitution as aspirational framework, does this enable marginalized groups to claim inclusion, or does it enable judges and dominant coalitions to reframe constitutional meaning in terms that serve existing power while appearing neutral and forward-looking?',
    'Case analysis: in constitutional litigation brought by marginalized groups using aspirational framing, does the living reading support or oppose their claims? Compare success rates of aspirational-frame litigation by marginalized groups vs established interests. If aspirational framing equally advantages both, it is neutral; if it systematically advantages established interests in practice despite aspiration language, then aspirational framing is a mechanism for benevolent-sounding extraction.',
    'If marginalized groups gain meaningful relief: tangled rope confirmed with genuine (if asymmetric) coordination. If established interests extract greater doctrine control while appearing to honor aspiration: reclassify toward snare with sophisticated cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aspiration_vs_aspiration_capture, empirical, 'Whether aspirational framing enables or constrains marginalized group claims').

omega_variable(
    kernel_reading_choice_rationale,
    'This constraint instantiates the living reading as ONE interpretation of the Constitution kernel. Why this reading rather than originalism or positivism? What epistemic or normative premises guide the choice?',
    'Authorial declaration: the reading was chosen because living reading has become institutionally dominant in elite US legal culture (law schools, progressive judiciary, reform coalitions). Originalism and positivism are real sibling readings with structural warrant. The choice reflects the fact that living reading is the current dominant framework shaping constitutional meaning — hence understanding it as a constraint is necessary for mapping the contemporary legal field. This is not a normative endorsement; it is an empirical choice based on institutional salience.',
    'This omega documents that constraint authoring maps DOMINANT constraint regimes, not all logically possible regimes. The originalist and positivist readings are equally real constraints with different ε values and beneficiary structures; they are simply less institutionally dominant in 2026 US legal culture. Future constraint corpus should include all three as sibling stories linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_choice_rationale, conceptual, 'Why living reading was chosen among sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usconst_living_tr_t0, us_constitution_1787__living_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(usconst_living_tr_t1, us_constitution_1787__living_reading, theater_ratio, 1, 0.42).
narrative_ontology:measurement(usconst_living_tr_t2, us_constitution_1787__living_reading, theater_ratio, 2, 0.64).
narrative_ontology:measurement(usconst_living_tr_t3, us_constitution_1787__living_reading, theater_ratio, 3, 0.64).

% Extraction over time
narrative_ontology:measurement(usconst_living_be_t0, us_constitution_1787__living_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(usconst_living_be_t1, us_constitution_1787__living_reading, base_extractiveness, 1, 0.35).
narrative_ontology:measurement(usconst_living_be_t2, us_constitution_1787__living_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(usconst_living_be_t3, us_constitution_1787__living_reading, base_extractiveness, 3, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(usconst_living_su_t0, us_constitution_1787__living_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(usconst_living_su_t1, us_constitution_1787__living_reading, suppression_requirement, 1, 0.32).
narrative_ontology:measurement(usconst_living_su_t2, us_constitution_1787__living_reading, suppression_requirement, 2, 0.52).
narrative_ontology:measurement(usconst_living_su_t3, us_constitution_1787__living_reading, suppression_requirement, 3, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% The US Constitution 1787 kernel admits three structurally distinct constraint readings. The living reading (this story) has ε≈0.52 (tangled rope); the originalist reading has lower ε (rope/mountain boundary) because it constrains interpretation; the positivist reading has lower ε but higher suppression (snare or tangled rope). Each reading is a valid constraint with its own beneficiary/victim structure. They coexist as competing institutional positions held by different legal actors. This network declares the kinship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__living_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
