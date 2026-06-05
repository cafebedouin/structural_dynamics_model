% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__living_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__living_constitution_reading, []).

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
 *   constraint_id: us_constitution_interpretive__living_constitution_reading
 *   human_readable: US Constitution: Living Constitution Interpretive Reading
 *   domain: constitutional_law/legal_interpretation/political_theory
 *
 * SUMMARY:
 *   The living constitution reading interprets the US Constitution as a
 *   document whose meaning evolves with societal values and contemporary
 *   conditions. Under this reading, the judiciary possesses authority to
 *   recognize unenumerated rights (privacy, dignity, equal protection of
 *   persons), to interpret enumerated powers (Commerce Clause, Necessary and
 *   Proper Clause) expansively in light of modern conditions, and to adapt
 *   doctrine as social understanding changes. The reading emerged as dominant
 *   judicial philosophy during the Warren and Burger Courts (1953–1986) and
 *   has become the default framework in American law schools despite episodic
 *   challenges from originalism. This constraint story instantiates ONE
 *   reading of the contested US Constitution kernel — specifically, the
 *   living constitution reading. The sibling readings are: originalist
 *   reading (constitutional meaning is fixed at ratification; judges should
 *   not recognize rights absent textual or historical warrant), and popular
 *   constitutionalism reading (constitutional meaning derives from popular
 *   sovereign will expressed through democratic politics, not judicial
 *   interpretation). This story models the living reading as a Tangled Rope:
 *   it possesses genuine coordination functions (enabling rights recognition
 *   without amendment; providing flexibility to address novel conditions;
 *   mobilizing political coalitions around constitutional claims) AND
 *   asymmetric extraction (expanding judicial authority; locking doctrine
 *   into beneficiary-favorable positions; constraining legislative choice;
 *   suppressing alternative interpretive frameworks). The constraint's
 *   extractiveness has increased over the 50-year interval from 0.35 to 0.58,
 *   reflecting the reading's accumulation of precedent and normalization in
 *   legal culture. The theater ratio has risen from 0.42 to 0.68, reflecting
 *   increasing performativity in living-constitutionalist discourse —
 *   scholarly debate often uses 'evolution' as a cover for discretionary
 *   doctrinal choices that amount to judicial policy-making.
 *
 * KEY AGENTS:
 *   - Civil Rights Expansion Claimants: Moderate power agents (constrained exit) — women, racial minorities, LGBTQ+ persons who rely on judicially-recognized unenumerated rights. Beneficiaries who experience Tangled Rope coordination function (enables rights mobilization) with extraction risk (rights can be narrowed by hostile court, as in Dobbs; dependent on judicial goodwill).
 *   - Original-Meaning Textualists / States-Rights Advocates: Powerless agents (trapped exit) — constitutional scholars, judges, and state officials who believe the Constitution's meaning is fixed at ratification and that living constitutionalism is usurpation. Experience Snare: trapped by doctrine that labels their framework as defeated and illegitimate; extraction flows toward judicial authority and away from enumerated powers.
 *   - Judicial Branch / Federal Regulatory Apparatus: Institutional beneficiary (arbitrage exit) — federal judges, administrative agencies, Congress's delegation-friendly jurisprudence. Primary institutional beneficiary; living constitutionalism expands their authority sphere with minimal textual constraint. Experience Rope (pure coordination benefit).
 *   - Organized Textualist-Originalist Movement: Organized agents (constrained exit) — Federalist Society, conservative law schools, state legislatures. Coordinate alternative framework but face high cost to overturn established precedent. Experience Tangled Rope (coordination function around originalism with extraction from living dominance).
 *   - Law School and Constitutional Scholarship Establishment: Institutional actors (arbitrage exit) — law faculties, major law reviews, constitutional law casebooks. Maintain living constitutionalism as default framework through curriculum and hiring. Experience Piton (performative role in legitimating the reading; institutional inertia maintains the framework despite internal methodological conflicts).
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating living constitutionalism as natural law inevitable to textual interpretation itself, which the engine will detect as false summit via structural beneficiary data.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, 0.58).
domain_priors:suppression_score(us_constitution_interpretive__living_constitution_reading, 0.52).
domain_priors:theater_ratio(us_constitution_interpretive__living_constitution_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__living_constitution_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__living_constitution_reading, "US Constitution: Living Constitution Interpretive Reading").
narrative_ontology:topic_domain(us_constitution_interpretive__living_constitution_reading, "constitutional_law/legal_interpretation/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__living_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__living_constitution_reading, '650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed').
narrative_ontology:cs_kernel_codification('650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed', fixed_text).
narrative_ontology:cs_authority_grounding('650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed', lineage).
narrative_ontology:cs_interpretation_layer_present('650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed').
narrative_ontology:cs_reading_relation('650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed', us_constitution_interpretive__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed', foundational, constitutional_meaning_evolves_with_conditions).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_conditions, holdable).
narrative_ontology:cs_axiom_grounding('650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed', constitutional_meaning_evolves_with_conditions, empirically_contingent).
narrative_ontology:cs_axiom('650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed', foundational, judicial_reasoned_interpretation_legitimate_source).
narrative_ontology:cs_axiom_status(judicial_reasoned_interpretation_legitimate_source, holdable).
narrative_ontology:cs_axiom_grounding('650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed', judicial_reasoned_interpretation_legitimate_source, deontological).
narrative_ontology:cs_axiom('650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed', secondary, unenumerated_rights_recognizable_under_liberty_dignity).
narrative_ontology:cs_axiom_status(unenumerated_rights_recognizable_under_liberty_dignity, holdable).
narrative_ontology:cs_axiom_grounding('650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed', unenumerated_rights_recognizable_under_liberty_dignity, deontological).
narrative_ontology:cs_reference_frame('650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed', judicial_authority_reasoned_adaptation).
narrative_ontology:cs_drift_state('650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed', contemporary_originalist_ascendance, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('650daf9f-a6bd-4d9a-8e5e-cd6a44eaf2ed', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, federal_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, judicial_branch).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, states_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, localist_governance_defenders).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, federal_power_skeptics).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVIL RIGHTS EXPANSION CLAIMANTS (TANGLED ROPE) — Beneficiaries of living constitutionalism who rely on judicially-recognized unenumerated rights (privacy, dignity, equal protection expansions). Genuine coordination function: the reading enables political mobilization around constitutional claims without waiting for amendment. Asymmetric extraction: claimants remain dependent on judicial interpretation; their rights can be narrowed by a different court (Dobbs precedent risk). Constrained exit — mobility exists but at high cost (federal relocation, political organizing, or accepting rights deprivation).
constraint_indexing:constraint_classification(us_constitution_interpretive__living_constitution_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: ORIGINAL-MEANING TEXTUALISTS / STATES-RIGHTS ADVOCATES (SNARE) — Trapped by a reading that forecloses their interpretive framework. From their structural position, living constitutionalism is pure extraction: it redistributes authority from enumerated powers (state legislatures, limited federal reach per original meaning) to unenumerated judicial authority. They experience suppression through doctrine that labels their position as 'originalist' (a pejorative in progressive legal culture) and through the accumulated precedent that treats their framework as historically defeated. Trapped because exit requires abandoning constitutional legitimacy claims within the existing legal order.
constraint_indexing:constraint_classification(us_constitution_interpretive__living_constitution_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIAL BRANCH / FEDERAL REGULATORY APPARATUS (ROPE) — Primary institutional beneficiary. Living constitutionalism dramatically expands their power: judges can recognize unenumerated rights; the federal government can regulate commerce and health with minimal textual constraint; administrative agencies operate under broad delegated authority justified by evolving needs. The reading coordinates their activity by providing legitimacy for expanded scope. The extraction is minimal from this perspective because the institutional actors ARE the authority structure that benefits. Arbitrage exit: they can maintain their authority under either reading, but living constitutionalism gives them more discretion.
constraint_indexing:constraint_classification(us_constitution_interpretive__living_constitution_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED TEXTUALIST-ORIGINALIST MOVEMENT (TANGLED ROPE) — Organized actors (Federalist Society, law review networks, state legislatures) coordinate alternative constitutional interpretation, creating genuine intellectual and institutional coordination around original meaning. But they also experience extraction: living constitutionalism limits their ability to reshape doctrine through ordinary advocacy because judicial precedent locks in the expansive reading. Constrained exit: they can organize, publish, and recruit, but the established judicial consensus (living constitutionalism) sets a high barrier to overturning precedent. Recent Supreme Court shift (2016-2024) shows exit is possible at high cost (requires sustained political organizing and judicial appointments over decades).
constraint_indexing:constraint_classification(us_constitution_interpretive__living_constitution_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LAW SCHOOL / CONSTITUTIONAL SCHOLARSHIP ESTABLISHMENT (PITON) — The living constitutionalist reading is deeply embedded in law school curricula, casebooks, and elite legal discourse. It functions as the default intellectual framework taught to all lawyers. However, the scholarship community itself recognizes the reading's performative aspects: 'living constitutionalism' is an umbrella term covering multiple methodologies (purposivism, evolutionary constitutionalism, pragmatism) that often conflict. The theater ratio reflects that the reading provides professional legitimacy and career incentives for constitutional scholars regardless of whether it resolves the underlying interpretive questions. Institutional inertia: law schools maintain the framework because it justifies the discipline's existence and prestige, not necessarily because it produces clearer constitutional meaning.
constraint_indexing:constraint_classification(us_constitution_interpretive__living_constitution_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL EVOLUTION THESIS (MOUNTAIN) — From a civilizational and universal perspective, this reading can appear to rest on a natural law: constitutional meaning MUST evolve as society changes, because written texts cannot remain fixed while their contexts transform. This perspective treats the reading as an immutable law of textual interpretation itself — meanings shift with usage, legal concepts reflect contemporary understanding, and stasis would ossify the system. However, the structural data contradicts this mountain classification: the living reading has identifiable beneficiaries and victims; doctrine is actively enforced; interpretation is contested. The engine will detect this as a false summit — a naturalized institutional arrangement.
constraint_indexing:constraint_classification(us_constitution_interpretive__living_constitution_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__living_constitution_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution_interpretive__living_constitution_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution_interpretive__living_constitution_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution_interpretive__living_constitution_reading, TR),
    TR >= 0.70.

:- end_tests(us_constitution_interpretive__living_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.58): The living constitution reading redistributes interpretive authority from the text and original meaning toward the judiciary and federal government. This is not pure extraction (which would be 0.66+) because the reading enables genuine coordination: rights claimants can mobilize around constitutional claims without waiting for amendment; the federal government can address novel problems (interstate commerce, environmental externalities, healthcare coordination) that the original text does not explicitly authorize. However, the extraction component is substantial: textualists and states-rights advocates lose authority; the judiciary gains discretion that can be exercised opportunistically (as suggested by Roe-Dobbs volatility); beneficiaries become dependent on judicial goodwill. The 0.58 value reflects this genuine-but-asymmetric structure. Suppression (0.52): Moderate-high. The living reading suppresses alternatives through: (1) doctrinal entrenchment (beneficiaries of prior decisions resist reversal); (2) professional culture (originalism is labeled 'originalist' and treated as historically defunct in law schools); (3) resource asymmetry (living constitutionalism attracts funding and faculty positions; originalism has been historically marginalized until the Federalist Society rebuilt it). Suppression is not total (originalism has mounted a sustained intellectual and institutional challenge) but is real. Theater Ratio (0.68): Moderately high. Living constitutionalist discourse relies heavily on performative claims ('the Constitution is a living document'; 'meaning evolves'; 'the framers could not have anticipated...') that do work in legal rhetoric but do not resolve the underlying interpretive questions. Scholars disagree profoundly about what methodology 'living constitutionalism' entails (purposivism, pragmatism, evolutionary constitutionalism are distinct). The reading functions as a tent that accommodates divergent judicial philosophies, suggesting significant performative content.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces the full spectrum of DR classifications across its six perspectives. Civil rights claimants experience Tangled Rope: the reading enables their mobilization AND makes them vulnerable to hostile courts. Textualists experience Snare: trapped by a reading that locks them out of authoritative interpretation. The judiciary experiences Rope: pure coordination benefit with minimal cost. The organized originalist movement experiences Tangled Rope in response: they coordinate an alternative framework but face extraction from living dominance. The law school establishment experiences Piton: maintains the framework through institutional inertia while recognizing its performative elements. The analytical observer risks Mountain: treating constitutional evolution as a natural law of interpretation itself. The perspectival gap reflects genuine structural differences in how agents relate to the constraint — some benefit from interpretive flexibility, others suffer from its unpredictability; some have political exit (through Court appointments) while others are trapped by doctrine; some maintain the framework professionally while doubting its coherence. No single classification is 'correct' — the presheaf over the observation site reveals the constraint's hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's structural relationship to the living reading: how much they benefit versus how much they bear costs. Civil rights claimants (moderate/constrained) are moderate beneficiaries with constraint-level barriers to exit — d ≈ 0.45. Textualists (powerless/trapped) are full targets with no exit — d ≈ 0.95. The judiciary (institutional/arbitrage) are strong beneficiaries with full arbitrage exit (can maintain authority under either reading) — d ≈ 0.05. The organized originalist movement (organized/constrained) are victims with exit at high cost (requires sustained political organizing) — d ≈ 0.70. Law schools (institutional/arbitrage) are beneficiaries with arbitrage exit (can teach originalism or living constitutionalism and maintain prestige) — d ≈ 0.15. The analytical observer is a neutral observer — d ≈ 0.72. These d values feed into the f(d) sigmoid function: higher d (targeted agents) produce higher f(d) values (stronger experienced extraction); lower d (beneficiaries) produce lower or negative f(d) values (benefits rather than extraction). The effective extractiveness χ = ε × f(d) × σ(S) scales this by the scope modifier — national scope (σ=1.0) does not dampen or amplify. The resultant chi values differ across perspectives: textualists experience high χ (trapped/high d); civil rights claimants experience moderate χ (beneficiary but constrained); judiciary experiences low or negative χ (strong beneficiary); originalists experience moderate χ (organized resistance).
 *
 * MANDATROPHY ANALYSIS:
 *   The living constitution reading avoids mandatrophy classification (ε = 0.58 < 0.70) by having genuine coordination function alongside extraction. The reading coordinates around several real problems: (1) constitutional adaptation without amendment (the coordination function for federal power adaptation); (2) rights expansion without formal amendment (the coordination function for civil rights); (3) flexibility for novel problems (Commerce Clause coordination around modern economic realities). These coordination functions prevent the reading from being pure extraction (which would trigger mandatrophy). However, the reading shows warning signs of mandatrophy drift: (1) increasing theater ratio over time (0.42→0.68) suggests growing performativity; (2) reversals in doctrine (Roe overturned by Dobbs) suggest the flexibility enables opportunistic expansion and contraction rather than genuine coordination; (3) institutional beneficiaries (judiciary, federal government) benefit regardless of whether the reading actually solves problems, suggesting extraction disguised as coordination. The rising extractiveness measurement (0.35→0.58) tracks increasing entropy in the system — the reading's flexibility has enabled doctrine to drift further from textual or historical grounding. Mandatrophy would occur if: theater ratio exceeded 0.75, extractiveness exceeded 0.70, or beneficiary benefit decoupled entirely from coordination function (e.g., rights recognized but then immediately reversed, leaving only institutional authority expansion).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the living constitution reading one legitimate interpretation of the constitutional kernel, or does it fundamentally transform the kernel into a different object?',
    'Textual comparison of the Constitution''s actual language and original structure vs. contemporary doctrine; analysis of whether the reading''s conclusions could be derived from the text under any reasonable interpretive methodology',
    'If legitimate reading: the contest is between coexisting frameworks. If transformation: the reading may foreclose the originalist reading within a single legal system, requiring institutional choice between frameworks. Current state: both readings coexist; no institutional foreclosure has occurred despite decades of contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether living constitutionalism interprets the kernel or transforms it').

omega_variable(
    beneficiary_identification_ambiguity,
    'Are the primary beneficiaries of living constitutionalism the specific rights-claimants and marginalized groups, or is the primary beneficiary the institutional apparatus (courts, federal government) that gains interpretive authority?',
    'Historical analysis of who captured the most value: did rights-claimant groups achieve outcomes they could not have achieved through textual reading? Did the institutional apparatus expand in scope regardless of whether individual rights claimants succeeded? Did the reading redistribute power between institutions more than it expanded substantive rights?',
    'If group-centered: the reading is Tangled Rope coordinating civil rights. If institution-centered: the reading is Tangled Rope coordinating federal power expansion, with civil rights as secondary effects. Current empirical signal: both occur simultaneously; the reading enables institutional expansion AND enables rights claims. Attribution remains contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Who captures most value from living constitutionalism').

omega_variable(
    stasis_costs_vs_adaptation_costs,
    'Are the costs of constitutional stasis (inability to address new conditions) greater or less than the costs of living constitutionalism (unpredictable doctrine, judicial override of legislative choices)?',
    'Comparison of outcomes: legislative dysfunction under fixed constitution vs. judicial overreach under evolving constitution; rate of constitutional gridlock in periods of textualist ascendance vs. periods of living constitutionalism; public confidence in government institutions under each reading',
    'If stasis costs exceed adaptation costs: living constitutionalism is justified as coordination solution. If adaptation costs exceed stasis costs: the reading is extractive apparatus that imposes unpredictability and reduces legislative capacity. Current empirical signal: both effects are real; the reading redistributes power between institutions in ways that can be both enabling (for minorities) and disabling (for democratic legislation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stasis_costs_vs_adaptation_costs, empirical, 'Whether stasis costs or adaptation costs dominate').

omega_variable(
    false_summit_natural_law_claim,
    'Does the living constitution reading rest on a genuine natural law (meaning must evolve with context as an immutable feature of textual interpretation), or does it rest on contested normative claims about what the Constitution should do?',
    'Textual analysis: are there interpretive methodologies that claim to be neutral and literal-reading that nonetheless produce evolving meanings? Can the originalist reading produce the same flexibility through its own methodological apparatus (e.g., ''original public meaning'' adapts as usage norms shift)? Is the evolution/stasis choice truly forced by linguistic necessity, or is it a policy choice?',
    'If natural law: the reading is mountain-like inevitable. If policy choice: the reading is tangled rope (legitimate but contingent coordination). Current theoretical position: living constitutionalism sometimes claims natural law status but simultaneously defends its policy virtues — the dual claim suggests the mountain status is rhetorical rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether constitutional evolution is natural law or policy choice').

omega_variable(
    institutional_capture_risk,
    'Does living constitutionalism create structural incentives for judicial power-seeking, whereby judges expand doctrine beyond what the normative case actually justifies, because the reading gives them cover to do so?',
    'Analysis of Supreme Court doctrine in periods of living constitutionalism vs. originalism: do living periods show more dramatic expansions of rights or powers that later get trimmed (Roe-Dobbs cycle, voting rights (Shelby County), privacy (Lawrence-Obergefell)? Are reversals more common under living constitutionalism than under originalism, suggesting the reading enabled overreach? Do judges explicitly justify power grabs using ''evolution'' language?',
    'If capture risk is high: the reading contains a structural extraction mechanism (judicial discretion disguised as interpretation). If capture risk is low: the reading''s flexibility is genuine coordination benefit. Current empirical signal: substantial reversals occur, but courts frame them as correction rather than overreach, suggesting the reading does enable opportunistic expansion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_risk, empirical, 'Whether living constitutionalism creates institutional capture risk').

omega_variable(
    original_meaning_stability,
    'Is the originalist reading actually as stable and constraining as its proponents claim, or does it too require interpretation that evolves over time (reading original meaning differently as evidence accumulates)?',
    'Historical analysis: has originalist interpretation remained constant as historical research revealed new facts about founding-era meaning? Or have originalists revised their positions about what the original meaning actually was? Does original-meaning originalism collapse into living constitutionalism at sufficient time distance (e.g., what was the original meaning of ''Internet commerce'' — there is none, so how does originalism handle it)?',
    'If originalism is also unstable: the distinction between readings is not stability vs. evolution, but rather explicit evolution (living) vs. implicit evolution (originalism denying its own evolution). This reduces the living reading''s extractiveness by showing both readings are hermeneutically similar in kind, differing only in acknowledgment and transparency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_stability, empirical, 'Whether originalism is actually stable or covertly evolving').

omega_variable(
    amendment_foreclosure_mechanism,
    'Does living constitutionalism functionally foreclose formal constitutional amendment by reducing the political incentive to amend (if judges will recognize rights anyway) and reducing the political capacity to amend (if the reading locks in beneficiaries)?',
    'Comparative analysis: amendment frequency and success rates in periods of living constitutionalism vs. originalism; analysis of whether specific substantive areas (civil rights, structural powers) show amendment attempts blocked by prior judicial doctrine under the living reading; interviews with legislators and amendment advocates about their strategic choices',
    'If amendment is foreclosed: the living reading creates a structural lock-in that prevents subsequent generations from choosing different doctrines through formal process. This would be a form of extraction (current beneficiaries lock future generations into their reading). If amendment remains available: the reading operates as Tangled Rope rather than Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_foreclosure_mechanism, empirical, 'Whether living constitutionalism forecloses formal amendment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__living_constitution_reading, 1960, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(living_const_theater_1960s, us_constitution_interpretive__living_constitution_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(living_const_theater_1990s, us_constitution_interpretive__living_constitution_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(living_const_theater_2010s, us_constitution_interpretive__living_constitution_reading, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(living_const_extract_1960s, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(living_const_extract_1990s, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(living_const_extract_2010s, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(living_const_suppress_1960s, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(living_const_suppress_1990s, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement(living_const_suppress_2010s, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__living_constitution_reading, identity_coordination).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive__popular_constitutionalism_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, judicial_discretion_extraction).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, federal_regulatory_scope_expansion).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, amendment_foreclosure_mechanism).

% DUAL FORMULATION NOTE:
% The US Constitution kernel decomposes into three structurally distinct constraint stories corresponding to the three major readings: originalist, living, and popular constitutionalist. Each reading has its own extractiveness value, beneficiary/victim structure, and perspectival classification. The three readings are not observables of a single constraint — they are distinct constraints that affect each other. The living reading story (this file) links to the originalist and popular readings via affects_constraints. Each reading has different ε values reflecting its different structural properties: originalist reading emphasizes constraint and predictability (lower theater); living reading emphasizes flexibility and adaptation (higher theater); popular reading emphasizes legitimacy and democratic accountability (different beneficiary/victim structure). Decomposition is necessary because the same natural-language concept ('the Constitution') covers three structurally distinct claims with different extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__living_constitution_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
