% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__positivist_reading, []).

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
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: Constitutional Validity via Formal Enactment and Institutional Authority (Positivist Reading)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   The positivist reading of American constitutional law asserts that
 *   constitutional validity derives exclusively from formal enactment
 *   procedures and institutional authority structures, not from external
 *   moral principles or evolving social understanding. Under this reading,
 *   judges are constrained to interpret the Constitution as written and
 *   formally amended; moral reasoning about what the Constitution should mean
 *   is categorically excluded from constitutional analysis. The positivist
 *   reading is one of three live interpretive traditions competing to
 *   establish the authoritative meaning of the Constitution itself. This
 *   constraint story models the positivist reading as a single structural
 *   position within that contest, generating a Tangled Rope classification:
 *   the reading simultaneously provides a coordination mechanism (judges
 *   interpreting law neutrally rather than imposing personal morality) and an
 *   asymmetric extraction mechanism (substantive justice claims without
 *   explicit textual grounding are foreclosed, and the amendment process is
 *   structurally gridlocked, making formal revision nearly impossible). The
 *   rising theater ratio (0.52 → 0.68 over 30 years) reflects that the gap
 *   between positivist legal theory and actual judicial practice has widened:
 *   judges increasingly recognize unenumerated rights (Obergefell, Roe
 *   pre-Dobbs, Lawrence, Griswold) through interpretive methods that violate
 *   positivist strictures, yet the formalist legitimacy claim persists in
 *   legal education and official doctrine. The rising extractiveness reflects
 *   the increasing cost borne by substantive justice claimants as the
 *   amendment process has become more gridlocked and the judiciary has more
 *   frequently invoked formalist reasoning to deny novel rights claims.
 *
 * KEY AGENTS:
 *   - Substantive Justice Claimants: Primary victims (powerless/trapped) — litigants asserting rights lacking explicit textual grounding; foreclosed by positivist reading with no appeal forum
 *   - Lower Court Judges: Secondary victims (moderate/constrained) — required to suppress equity reasoning and stick to text; experience mixed coordination (neutral judging) and extraction (constraint on reasoning)
 *   - Supreme Court Formalist Majority: Primary beneficiary (institutional/arbitrage) — gains legitimacy claim and discretion; experiences constraint as pure coordination
 *   - Constitutional Amendment Coalition: Organized secondary victim (organized/constrained) — benefits from formal legitimacy of amendment pathway but bears extraction cost of structural gridlock
 *   - Positivist Legal Academy: Institutional maintainer (institutional/arbitrage) — perpetuates formalist theory through pedagogical ritual despite divergence from practice; piton perspective reflects degradation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing proceduralist constraint as inherent to constitutional systems rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.58).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.62).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "Constitutional Validity via Formal Enactment and Institutional Authority (Positivist Reading)").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, 'd2253e8a-2e07-4667-8cb2-df277c74c1ae').
narrative_ontology:cs_kernel_codification('d2253e8a-2e07-4667-8cb2-df277c74c1ae', fixed_text).
narrative_ontology:cs_authority_grounding('d2253e8a-2e07-4667-8cb2-df277c74c1ae', extraction).
narrative_ontology:cs_interpretation_layer_present('d2253e8a-2e07-4667-8cb2-df277c74c1ae').
narrative_ontology:cs_reading_relation('d2253e8a-2e07-4667-8cb2-df277c74c1ae', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2253e8a-2e07-4667-8cb2-df277c74c1ae', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('d2253e8a-2e07-4667-8cb2-df277c74c1ae', foundational, constitutional_validity_requires_formal_procedure).
narrative_ontology:cs_axiom_status(constitutional_validity_requires_formal_procedure, holdable).
narrative_ontology:cs_axiom_grounding('d2253e8a-2e07-4667-8cb2-df277c74c1ae', constitutional_validity_requires_formal_procedure, deontological).
narrative_ontology:cs_axiom('d2253e8a-2e07-4667-8cb2-df277c74c1ae', foundational, moral_reasoning_excludes_constitutional_validity).
narrative_ontology:cs_axiom_status(moral_reasoning_excludes_constitutional_validity, holdable).
narrative_ontology:cs_axiom_grounding('d2253e8a-2e07-4667-8cb2-df277c74c1ae', moral_reasoning_excludes_constitutional_validity, deontological).
narrative_ontology:cs_reference_frame('d2253e8a-2e07-4667-8cb2-df277c74c1ae', procedural_validity_through_enactment).
narrative_ontology:cs_drift_state('d2253e8a-2e07-4667-8cb2-df277c74c1ae', contemporary_unenumerated_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d2253e8a-2e07-4667-8cb2-df277c74c1ae', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, institutional_legitimacy_claim).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, formalist_judiciary).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, unenumerated_rights_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSTANTIVE JUSTICE CLAIMANT (SNARE) — A litigant asserting a moral right or substantive justice claim that lacks explicit textual grounding in the Constitution (e.g., reproductive autonomy under pre-Dobbs jurisprudence, right to marriage equality pre-Obergefell). Under positivist reading, their claim is categorically foreclosed: if it is not in the text or formal amendment record, it has no constitutional validity. No exit from this constraint — the agent cannot reorganize the interpretation framework. Maximum extraction: the judiciary's refusal to acknowledge the claim as constitutionally valid, combined with no alternative forum for constitutional-level protection.
constraint_indexing:constraint_classification(us_constitution_meaning__positivist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOWER COURT JUDGE (TANGLED ROPE) — Constrained by appellate precedent and the positivist reading's requirement to stick to text and formal amendment. But also coordinating a real function: providing predictable legal interpretation based on enacted law rather than judges' personal moral views. Benefits from the constraint's legitimacy function (can claim to be 'applying law, not making it') while bearing the cost of having to suppress substantive justice reasoning. Mixed experience: genuine coordination (neutral judging) and real extraction (foreclosure of equity considerations).
constraint_indexing:constraint_classification(us_constitution_meaning__positivist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUPREME COURT MAJORITY / FORMALIST COALITION (ROPE) — Net beneficiary of the positivist reading. Gains legitimacy claim ('we follow the Constitution, not our preferences') and institutional discretion (can reject morality-based arguments without explanation). Experiences the constraint as pure coordination: the formalist frame enables the Court to adjudicate without admitting to policy choices. High arbitrage capacity — can access originalist reasoning when convenient, can shift to living constitutionalism when the text is sufficiently ambiguous. The extraction runs toward this agent; they are the beneficiary.
constraint_indexing:constraint_classification(us_constitution_meaning__positivist_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL AMENDMENT COALITION (TANGLED ROPE) — Organized agents (Congress, state legislatures, advocacy organizations) who can formally amend the Constitution through Article V procedures. Benefits from the constraint's formalist logic: amendments provide a legitimate pathway for constitutional change (coordination function). But bears significant extraction: the amendment process is extraordinarily difficult (requires supermajority consensus), making formal textual change rarely available. The constraint both enables (formal legitimacy of amendment) and extracts (structural impossibility of most proposed amendments). Suppression is built into the amendment procedure itself.
constraint_indexing:constraint_classification(us_constitution_meaning__positivist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POSITIVIST LEGAL ACADEMY (PITON) — Institutional advocates for the positivist reading as a philosophical position. Theater ratio measures the degree to which positivist formalism is maintained as ritual despite internal contradictions (the Court regularly reads unenumerated rights into the text, changes constitutional meaning through reinterpretation, etc.). The academic positivist position persists through institutional inertia: law schools teach formalism as the official theory while courts practice living constitutionalism in effect. High theater because the performative claim ('this is objective legal interpretation') is substantially disconnected from actual judicial behavior. Piton classification reflects degradation: the theory is maintained through pedagogical repetition and professional identity, not because it explains judicial practice.
constraint_indexing:constraint_classification(us_constitution_meaning__positivist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, positivism asserts that constitutional validity is necessarily grounded in formal enactment and institutional authority procedures — that external moral principles cannot determine constitutional meaning because legitimacy requires consent through proper procedure. This reads as a necessary feature of legal systems themselves: any constitution must derive validity from enactment, not from extra-legal moral truth. However, this is a FALSE SUMMIT: the positivist reading naturalizes what is a contingent institutional choice (rejecting moral reasoning) as inherent to constitutional law. Alternative readings (originalism, living constitutionalism) also accept formal enactment as the foundation but diverge on what 'constitutional meaning' means and whether moral reasoning can inform interpretation.
constraint_indexing:constraint_classification(us_constitution_meaning__positivist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__positivist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution_meaning__positivist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution_meaning__positivist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, TR),
    TR >= 0.70.

:- end_tests(us_constitution_meaning__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The positivist reading creates real asymmetric costs: substantive justice claimants are foreclosed from constitutional protection unless they can manufacture explicit textual support or organize a supermajority amendment coalition. But the extraction is not total (χ ≈ 0.70 under maximum targeting) because the Supreme Court frequently violates positivist strictures in practice, creating some space for justice-based reasoning even if not officially acknowledged. The measurement trajectory (0.48 → 0.58) reflects increasing extraction over time: as gridlock deepens, the amendment pathway becomes functionally closed, shifting more pressure onto the judiciary to either violate positivism or deny substantive claims. Suppression (0.62): High. The positivist reading explicitly suppresses moral reasoning as a tool for constitutional analysis, declares it categorically illegitimate, and provides no alternative mechanism for advancing justice claims that lack textual support. The suppression is structural (built into the proceduralist axiom) and enforced through professional norms (law students are taught formalism; judges who reason morally are criticized for 'activism'). Theater ratio (0.68): Moderate-high. Legal formalism functions as ritual: judges claim to be 'following the law' while actually making discretionary choices about meaning, scope, and applicability. The theater has risen (0.52 → 0.68) because the discrepancy between theory and practice has widened — the Court recognizes rights that positivism forbids, yet continues using formalist language to describe its reasoning. The pedagogical theater is high: law schools teach positivism as the authoritative theory while studying case law that violates it.
 *
 * PERSPECTIVAL GAP:
 *   The positivist reading generates a full perspectival divide. Substantive justice claimants see extraction and foreclosure (Snare) — their claims are declared categorically invalid. Lower court judges see mixed coordination and extraction (Tangled Rope) — they get legitimacy from formalism but lose reasoning capacity. The Supreme Court beneficiary sees pure coordination (Rope) — formalism provides the legitimacy cover they need. The amendment coalition sees mixed coordination and extraction (Tangled Rope) — they have a formal pathway but it is structurally impossible to use. The legal academy sees degraded ritual (Piton) — formalism persists through institutional inertia despite disconnection from practice. The analytical observer risks seeing a natural law (Mountain) — positing that constitutional validity must necessarily derive from procedure — but this is a false summit: the choice to exclude moral reasoning is contingent, not necessary. The reading contest itself appears in the perspectives: origina­list and living constitutionalist judges would classify differently, showing that the readings are structurally distinct positions, not mere differences of opinion.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives directionality (d) from the agent's structural position relative to this specific reading. Substantive justice claimants as powerless victims with trapped exit: d ≈ 0.95 (full target of extraction), yielding high f(d) ≈ 1.42 and high experienced χ. Lower court judges as moderate agents constrained but also coordinating: d ≈ 0.60 (slight victim, receiving some benefit from legitimacy coordination), yielding f(d) ≈ 0.90. The Supreme Court as institutional beneficiary with arbitrage options: d ≈ 0.15 (full beneficiary, can exit by invoking living constitutionalism when needed), yielding negative f(d) ≈ -0.01. The amendment coalition as organized agents facing structural gridlock: d ≈ 0.55 (slight victim of the constraint on change), yielding f(d) ≈ 0.75. The legal academy as institutional arbitrageurs perpetuating the theory: d ≈ 0.20 (beneficiary of maintaining professional doctrine), yielding f(d) ≈ 0.02. The analytical observer: d ≈ 0.72 (observing the whole structure from outside), yielding f(d) ≈ 1.15. These directionalities track beneficiary/victim declarations: substantive justice claimants are declared victims; institutional legitimacy and formalist judiciary are declared beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in the positivist reading is resolved by recognizing it as one reading of a contested kernel, not a universal truth about constitutional systems. The reading is not 'wrong' — it correctly identifies real constraints on judicial reasoning and real extraction costs for justice claimants. But it is not inevitable or necessary. The originalist reading also grounds validity in formal enactment but differs on what constitutes 'the text's meaning' and whether historical context is admissible. The living constitutionalist reading also accepts formal enactment as foundational but argues that constitutional principles endure and application must evolve. The three readings coexist as live positions held by competing judicial coalitions. The mandatrophy resolves when we recognize that the question 'Is constitutional validity determined by procedure or substance?' has no single true answer — it is a choice about institutional design that different reading communities make differently. The positivist reading's classification (Tangled Rope: genuine coordination + asymmetric extraction) is the correct reading of its structure; the other readings would have different ε values and classifications, reflecting their different structural features.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivism_vs_originalism_collapse,
    'Does positivism collapse into originalism in practice when the formal amendment process is gridlocked?',
    'Historical and comparative analysis: examine whether positivist judges facing amendment gridlock systematize to originalist interpretive methods (historical public meaning as proxy for ''what the text says'') rather than admitting discretion. Compare judicial rhetoric across periods of amendment viability vs gridlock.',
    'If collapse occurs: positivism is not a stable independent position but a temporary one that reverts to originalism under structural pressure. If positivism holds: the reading provides a genuinely distinct judicial constraint that persists even with gridlocked amendment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivism_vs_originalism_collapse, empirical, 'Whether positivism collapses into originalism under amendment gridlock').

omega_variable(
    moral_reasoning_exclusion_coherence,
    'Can judges coherently exclude moral reasoning from constitutional interpretation while still interpreting ambiguous text?',
    'Conceptual analysis of judicial decisions that claim to avoid moral reasoning: trace the logical chain from text to holding and identify whether moral premises appear (implicitly or explicitly). Identify decisions where text-only interpretation would yield multiple defensible outcomes, and examine whether the chosen interpretation tracks a moral principle.',
    'If coherence fails: the positivist axiom (moral reasoning excluded) cannot be implemented; the constraint becomes aspirational/theatrical rather than structural. If coherence holds: positivism is a genuine executable constraint on judicial reasoning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_reasoning_exclusion_coherence, conceptual, 'Whether moral reasoning can be coherently excluded from constitutional interpretation').

omega_variable(
    unenumerated_rights_under_positivism,
    'What is the status of unenumerated rights (recognized via substantive due process, privileges or immunities, or other doctrines) under strict positivist reading?',
    'Logical analysis: if constitutional validity requires explicit textual grounding or formal amendment, are unenumerated rights categorically invalid? Can substantive due process doctrine be reconciled with positivism, or does recognition of unenumerated rights require abandoning positivism?',
    'If unenumerated rights are categorically invalid under positivism: the reading requires overruling major precedent (Roe, Obergefell, Griswold). If unenumerated rights can be accommodated: positivism is less determinate than claimed, and the reading collapses toward originalism or living constitutionalism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unenumerated_rights_under_positivism, conceptual, 'Logical status of unenumerated rights under positivist reading').

omega_variable(
    reading_contest_framing,
    'Which alternative readings (''living constitutionalism'' vs ''originalism'') genuinely compete with positivism, and which are mutually exclusive?',
    'Logical analysis of the three readings'' core commitments: Does originalism (historical public meaning) require rejecting formal enactment procedures as the basis for validity? Does living constitutionalism? Can an originalist judge simultaneously hold a positivist view of constitutional validity (procedure-grounded)? Can a living constitutionalist?',
    'If readings are genuinely orthogonal (one answer to ''what determines validity'' vs another to ''what is the meaning?''): the constraint family has three independent stories. If readings are nested or overlapping: the decomposition requires revision and some stories should be consolidated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_framing, conceptual, 'Logical relationships between competing constitutional readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usconst_pos_tr_t0, us_constitution_meaning__positivist_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(usconst_pos_tr_t15, us_constitution_meaning__positivist_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement(usconst_pos_tr_t30, us_constitution_meaning__positivist_reading, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(usconst_pos_be_t0, us_constitution_meaning__positivist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(usconst_pos_be_t15, us_constitution_meaning__positivist_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(usconst_pos_be_t30, us_constitution_meaning__positivist_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(usconst_pos_su_t0, us_constitution_meaning__positivist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(usconst_pos_su_t15, us_constitution_meaning__positivist_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(usconst_pos_su_t30, us_constitution_meaning__positivist_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% The US Constitution Meaning kernel has three structurally distinct constraint stories: positivist_reading (this story), originalist_reading, and living_constitutionalist_reading. Each represents a competing interpretation community's way of treating the same fixed text. The stories have different ε values because they generate different structural relationships between text, meaning, and interpretation. All three are linked via network.affects_constraints because they represent competing claims about how to treat the same kernel. The positivist reading (ε≈0.58, Tangled Rope) forecloses moral reasoning and creates extraction through gridlocked amendment. The originalist reading would have lower extractiveness (ε≈0.42) but also forecloses living evolution. The living constitutionalist reading would have higher theater (reflecting gap between principle-commitment and actual practice) and lower suppression (because it accommodates moral reasoning). These are not different measurements of one constraint; they are different constraints resulting from different readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
