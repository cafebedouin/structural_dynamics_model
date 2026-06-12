% ============================================================================
% CONSTRAINT STORY: human_dignity_ai_governance_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_dignity_ai_governance_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: human_dignity_ai_governance_flat_control
 *   human_readable: Catholic Social Doctrine Authority in AI Governance
 *   domain: theological_ethics/technology_governance/political_economy
 *
 * SUMMARY:
 *   Catholic Social Doctrine (CST) claims authority to adjudicate legitimate
 *   action in the AI era through a stabilized kernel: human dignity as imago
 *   Dei (humans bear the image of God) and principles derived from it (common
 *   good, subsidiarity, solidarity, universal destination of goods, social
 *   justice). The encyclical tradition presents this kernel as immutable
 *   natural law, discovered rather than constructed, and universally binding
 *   regardless of whether individuals or societies accept Catholic
 *   theological premises. The constraint operates across multiple scales: at
 *   the individual level (AI developers constrained by CST-influenced
 *   regulations), at the institutional level (corporations and governments
 *   adopting CST frameworks for AI ethics), and at the civilizational level
 *   (CST competing with secular and other religious frameworks for authority
 *   in technology governance). The AI era creates a crisis of governance —
 *   rapid technological change outpaces existing ethical and regulatory
 *   frameworks — and CST positions itself as providing comprehensive,
 *   time-tested principles. The constraint's extractiveness has increased
 *   over the 75-year interval (from 0.35 to 0.48) as the Church has extended
 *   CST principles from labor and economic questions (their original domain
 *   in 19th-century encyclicals) to technology governance, where the Church's
 *   institutional authority is less established and more contested.
 *   Suppression has also increased (from 0.45 to 0.62) as CST-influenced
 *   regulations have been enacted in jurisdictions where the Church has
 *   political influence, creating barriers to AI development that does not
 *   conform to CST principles. Theater ratio remains relatively low (0.35)
 *   because CST's application to AI governance is substantive rather than
 *   performative: the principles provide actionable guidance, even if their
 *   theological grounding is contested.
 *
 * KEY AGENTS:
 *   - Catholic Institutional Hierarchy: Primary beneficiary (institutional/arbitrage) — gains authority in AI governance domain; can engage or withdraw without institutional cost
 *   - AI Ethics Practitioners: Mixed position (moderate/constrained) — benefit from CST's comprehensive framework but constrained by its theological grounding; must accept or translate premises
 *   - Secular AI Developers: Primary victim (powerless/trapped in Catholic-majority contexts) — constrained by CST-influenced regulations they did not consent to; cannot exit without career disruption
 *   - Workers in AI-Impacted Sectors: Beneficiary (powerless/constrained) — CST's labor protection principles (subsidiarity, solidarity) provide framework for resisting AI-driven displacement, but workers lack power to enforce these principles without institutional support
 *   - Ecumenical AI Ethics Coalition: Organized agents (organized/mobile) — building interfaith frameworks that draw on CST but do not depend on Catholic theological authority; see CST as transitional resource with functional sunset
 *   - Magisterial Teaching Authority: Identity-locked institutional actor (institutional/identity_locked) — the Magisterium's self-concept is constituted through its teaching authority; cannot abandon claim that CST principles are universally binding without dissolving institutional identity
 *   - Secular AI Governance Frameworks: Victim (institutional/constrained) — marginalized in contexts where CST has influence; must either incorporate CST principles or be framed as inadequate for lacking comprehensive anthropology
 *   - Non-Catholic Religious Traditions: Victim (institutional/constrained) — CST's claim to natural law universality implicitly subordinates other traditions' AI ethics frameworks; must either align with CST or be framed as particular rather than universal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_dignity_ai_governance_flat_control, 0.48).
domain_priors:suppression_score(human_dignity_ai_governance_flat_control, 0.62).
domain_priors:theater_ratio(human_dignity_ai_governance_flat_control, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_dignity_ai_governance_flat_control, extractiveness, 0.48).
narrative_ontology:constraint_metric(human_dignity_ai_governance_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(human_dignity_ai_governance_flat_control, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_dignity_ai_governance_flat_control, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(human_dignity_ai_governance_flat_control, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_dignity_ai_governance_flat_control, rope).
narrative_ontology:human_readable(human_dignity_ai_governance_flat_control, "Catholic Social Doctrine Authority in AI Governance").
narrative_ontology:topic_domain(human_dignity_ai_governance_flat_control, "theological_ethics/technology_governance/political_economy").

domain_priors:requires_active_enforcement(human_dignity_ai_governance_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_dignity_ai_governance_flat_control, '46d5142b-8f5e-4300-bda4-e588083efbe5').
narrative_ontology:cs_kernel_codification('46d5142b-8f5e-4300-bda4-e588083efbe5', formalized).
narrative_ontology:cs_authority_grounding('46d5142b-8f5e-4300-bda4-e588083efbe5', lineage).
narrative_ontology:cs_interpretation_layer_present('46d5142b-8f5e-4300-bda4-e588083efbe5').
narrative_ontology:cs_created_at('46d5142b-8f5e-4300-bda4-e588083efbe5', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(human_dignity_ai_governance_flat_control, human_dignity_ai_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_dignity_ai_governance_flat_control, catholic_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(human_dignity_ai_governance_flat_control, ai_ethics_practitioners_seeking_frameworks).
narrative_ontology:constraint_beneficiary(human_dignity_ai_governance_flat_control, workers_in_ai_impacted_sectors).
narrative_ontology:constraint_victim(human_dignity_ai_governance_flat_control, secular_ai_governance_frameworks).
narrative_ontology:constraint_victim(human_dignity_ai_governance_flat_control, non_catholic_religious_traditions).
narrative_ontology:constraint_victim(human_dignity_ai_governance_flat_control, ai_developers_constrained_by_doctrine).
narrative_ontology:constraint_vindicates(human_dignity_ai_governance_flat_control, imago_dei_anthropology).
narrative_ontology:constraint_vindicates(human_dignity_ai_governance_flat_control, natural_law_universality).
narrative_ontology:constraint_vindicates(human_dignity_ai_governance_flat_control, magisterial_teaching_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECULAR AI DEVELOPER (SNARE) — Trapped by regulatory frameworks shaped by Catholic Social Doctrine in jurisdictions where the Church has institutional influence. Cannot exit national regulatory environment without career disruption. Experiences the constraint as pure extraction: theological premises they do not accept determine what AI systems they can build, what labor practices are permissible, what data uses are legitimate. The coordination story (protecting human dignity) is experienced as cover for imposing sectarian values through state power.
constraint_indexing:constraint_classification(human_dignity_ai_governance_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AI ETHICS PRACTITIONER (TANGLED ROPE) — Benefits from Catholic Social Doctrine's comprehensive framework for AI governance (subsidiarity, common good, solidarity provide actionable principles where secular frameworks often remain abstract). But also constrained by the framework's theological grounding: must either accept imago Dei anthropology or translate principles into secular language at cost of coherence. Coordination function is real (the framework solves genuine problems about AI's impact on labor, dignity, and social goods), but extraction is also real (the framework's authority depends on accepting premises many practitioners do not hold).
constraint_indexing:constraint_classification(human_dignity_ai_governance_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CATHOLIC INSTITUTIONAL HIERARCHY (ROPE) — Primary beneficiary. The AI governance crisis creates demand for comprehensive ethical frameworks; Catholic Social Doctrine supplies one grounded in 130+ years of encyclical tradition. The constraint coordinates action (provides shared principles for evaluating AI systems) and the hierarchy experiences minimal extraction: they set the terms, interpret the kernel, and benefit from institutional authority in a domain (technology ethics) where secular frameworks are fragmented. Arbitrage exit: can engage or withdraw from AI governance debates without institutional cost.
constraint_indexing:constraint_classification(human_dignity_ai_governance_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ECUMENICAL COALITION (SCAFFOLD) — Organized religious and secular groups building interfaith AI governance frameworks see Catholic Social Doctrine as a transitional resource: its principles (subsidiarity, common good) are useful starting points, but the coalition is working toward frameworks that do not depend on any single tradition's theological premises. The constraint has a functional sunset: as interfaith and secular frameworks mature, Catholic Social Doctrine's unique authority diminishes. The coalition benefits from the framework's current comprehensiveness while building alternatives.
constraint_indexing:constraint_classification(human_dignity_ai_governance_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: MAGISTERIAL AUTHORITY (MOUNTAIN / FALSE SUMMIT CANDIDATE) — From within the Catholic tradition's self-understanding, the imago Dei kernel and its derived principles are not constructed but discovered: human dignity is an ontological fact, not a negotiable premise. The constraint appears as natural law — immutable, universal, binding regardless of acceptance. Identity-locked exit: the Magisterium's institutional identity is constituted through this teaching authority; abandoning the claim that these principles are universally binding would dissolve the institution's self-concept. However, the structural data contradicts the mountain classification: identifiable beneficiaries (the institutional hierarchy), substantial resistance (0.68), and low accessibility collapse (0.15) indicate this is a constructed constraint naturalized through theological framing.
constraint_indexing:constraint_classification(human_dignity_ai_governance_flat_control, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint has genuine coordination function: Catholic Social Doctrine provides actionable principles (subsidiarity, common good, solidarity) that address real problems in AI governance where secular frameworks often fail (how to balance innovation and worker protection, how to prevent AI-driven concentration of power, how to ensure technology serves human flourishing). But the constraint also extracts: its authority depends on accepting theological premises (imago Dei, natural law universality, magisterial teaching authority) that are contested outside the tradition. The framework coordinates those who accept its premises while suppressing alternative governance approaches in contexts where the Church has institutional power. Asymmetric extraction is structural: the hierarchy benefits from authority in a new domain; secular frameworks and non-Catholic traditions bear the cost of marginalization in Catholic-influenced jurisdictions.
constraint_indexing:constraint_classification(human_dignity_ai_governance_flat_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_dignity_ai_governance_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(human_dignity_ai_governance_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(human_dignity_ai_governance_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_dignity_ai_governance_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(human_dignity_ai_governance_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts in two ways: (1) institutional extraction — the Catholic hierarchy gains authority in AI governance without bearing the costs of technological development or regulatory compliance; (2) epistemic extraction — CST's claim to natural law universality marginalizes secular and non-Catholic frameworks, requiring them to either adopt CST language or be framed as inadequate. However, extraction is not maximal because CST does provide genuine coordination function (see below), and many actors voluntarily adopt CST principles because they find them useful, not only because of institutional pressure. The increase from 0.35 to 0.48 over the interval reflects CST's expansion from its traditional domain (labor, economic justice) into technology governance, where its authority is more contested and its extraction mechanisms more visible. Suppression (0.62): Moderate-high. Suppression operates through: (1) regulatory barriers — CST-influenced laws in Catholic-majority jurisdictions constrain what AI systems can be built and deployed; (2) epistemic barriers — CST's theological grounding creates barriers to participation for those who do not accept Catholic premises (you must either accept imago Dei anthropology or invest effort in translating CST principles into secular language, often at cost of coherence); (3) institutional barriers — in contexts where the Church has political influence, alternative AI governance frameworks face higher costs to adoption. Suppression has increased over the interval as CST principles have been codified into law and as the Church has intensified its engagement with AI governance questions. However, suppression is not maximal because: (1) exit is possible (developers can relocate to jurisdictions with less CST influence, though at career cost); (2) alternative frameworks exist and are growing (secular AI ethics, other religious traditions, industry self-regulation); (3) CST's influence is geographically concentrated (strong in parts of Europe and Latin America, weak in much of Asia and the secular West). Theater ratio (0.35): Low-moderate. CST's application to AI governance is substantive rather than performative. The principles (subsidiarity, common good, solidarity, universal destination of goods) provide actionable guidance: subsidiarity suggests AI governance should be decentralized where possible; common good requires AI development to serve collective flourishing, not only private profit; solidarity requires attention to AI's impact on vulnerable populations; universal destination of goods suggests AI-generated wealth should be broadly distributed. These principles have real policy implications (e.g., they support worker protections against AI-driven displacement, data governance frameworks that prioritize privacy and dignity, and regulatory approaches that prevent AI-driven concentration of power). The theater that does exist (0.35) comes from: (1) the gap between CST's comprehensive principles and the Church's limited capacity to enforce them (the Church can advocate but has limited coercive power outside jurisdictions where it has political influence); (2) the tension between CST's claim to universality and its actual particularity (the principles are presented as natural law but are contested outside the Catholic tradition); (3) the performative invocation of CST principles by actors who do not implement them substantively (corporations and governments citing CST in ethics statements while pursuing policies that violate CST principles). Theater has increased modestly over the interval (from 0.25 to 0.35) as CST has become more prominent in AI ethics discourse, creating incentives for performative adoption.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon — Catholic Social Doctrine's authority in AI governance — appears radically different from different structural positions. The Catholic institutional hierarchy sees pure coordination (Rope): CST provides comprehensive principles for AI governance, and the hierarchy experiences minimal extraction because they set the terms and benefit from institutional authority. The AI ethics practitioner sees mixed coordination and extraction (Tangled Rope): CST's principles are useful, but their theological grounding creates barriers to adoption and requires costly translation for secular contexts. The secular AI developer in a Catholic-majority context sees pure extraction (Snare): theological premises they do not accept constrain what they can build, and they cannot exit without career disruption. The ecumenical coalition sees a temporary coordination mechanism with a sunset (Scaffold): CST's principles are useful now, but the coalition is building interfaith frameworks that will eventually replace CST's unique authority. The Magisterial teaching authority sees immutable natural law (Mountain): human dignity as imago Dei is not constructed but discovered, and CST's principles are universally binding regardless of acceptance. The analytical observer sees the mountain classification as a false summit: the structural data (identifiable beneficiaries, substantial resistance, low accessibility collapse, increasing extraction over time) indicates CST is a constructed framework naturalized through theological framing, not a discovered natural law. The perspectival gap is not a measurement error — it is the phenomenon. Each perspective is a legitimate reading of the structural data from a different position in the extraction/coordination flow. The constraint IS this presheaf of perspectives, not any single type.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from each agent's structural relationship to the constraint — their position as beneficiary or victim, their power level, and their exit options. The Catholic institutional hierarchy is the primary beneficiary: they gain authority in a new domain (AI governance) by extending a framework they already control (CST). Their directionality is low (near 0.0) — extraction flows toward them, not away from them. They experience the constraint as pure coordination: CST provides a comprehensive framework for addressing AI's challenges, and the hierarchy benefits from institutional authority without bearing the costs of compliance. Their arbitrage exit options further reduce their effective extraction: they can engage with AI governance when it serves institutional interests and withdraw when it does not. AI ethics practitioners have mixed directionality (near 0.4-0.5). They benefit from CST's comprehensive framework — it provides actionable principles where secular frameworks often remain abstract — but they also bear costs: if they do not accept Catholic theological premises, they must invest effort in translating CST principles into secular language, often at cost of coherence. Their constrained exit options (they can switch frameworks but at professional cost) moderate their effective extraction. Secular AI developers in Catholic-majority contexts are primary victims with high directionality (near 0.8-0.9). They bear the costs of CST-influenced regulations they did not consent to, and their trapped exit options (cannot leave national regulatory environment without career disruption) amplify their effective extraction. They experience the constraint as pure extraction: theological premises they do not accept determine what they can build. Workers in AI-impacted sectors have complex directionality (near 0.3-0.4). They benefit from CST's labor protection principles (subsidiarity and solidarity provide frameworks for resisting AI-driven displacement), but they lack power to enforce these principles without institutional support. Their powerless position and constrained exit options (cannot easily exit labor markets being transformed by AI) create moderate effective extraction, but the extraction is lower than for secular developers because workers are declared beneficiaries (CST's principles are designed to protect labor). The Magisterial teaching authority has identity-locked exit options, which creates a unique directionality profile. The Magisterium is a beneficiary (gains authority through CST's extension to AI governance), but its identity-locked position means it cannot abandon the claim that CST principles are universally binding without dissolving its institutional self-concept. This creates a form of self-imposed constraint: the Magisterium must maintain CST's natural law framing even when that framing generates resistance and reduces CST's influence. The analytical observer perspective computes directionality from the full structural picture: CST has genuine coordination function (the principles address real problems in AI governance) but also embedded extraction (the framework's authority depends on accepting contested theological premises, and the hierarchy benefits from institutional authority without bearing compliance costs). This produces moderate directionality (near 0.5), reflecting the tangled rope classification: coordination and extraction are structurally inseparable.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that CST's coordination function and its extraction mechanism are structurally inseparable. CST provides genuine coordination: its principles (subsidiarity, common good, solidarity) address real problems in AI governance where secular frameworks often fail. Subsidiarity provides guidance on decentralization; common good provides a framework for balancing innovation and social welfare; solidarity provides principles for protecting vulnerable populations from AI-driven harm. These are not merely rhetorical — they have policy implications and have influenced actual AI governance frameworks in Europe and Latin America. However, the coordination function is embedded in an extraction mechanism: CST's authority depends on accepting theological premises (imago Dei, natural law universality, magisterial teaching authority) that are contested outside the Catholic tradition. The hierarchy benefits from institutional authority in a new domain without bearing the costs of compliance. Secular frameworks and non-Catholic traditions are marginalized in contexts where CST has influence. The extraction is not incidental to the coordination — it is structural. CST's comprehensiveness (what makes it useful for coordination) derives from its theological grounding (what makes it extractive for those who do not accept that grounding). You cannot separate the coordination function from the extraction mechanism without dissolving the framework's coherence. This is the definition of tangled rope from the analytical perspective: genuine coordination function with asymmetric extraction, requiring active enforcement (CST-influenced regulations, institutional pressure) to maintain. The mandatrophy is resolved not by choosing between 'CST is pure coordination' and 'CST is pure extraction' but by recognizing that both are true simultaneously from different structural positions, and that the analytical perspective reveals the inseparability of coordination and extraction in this constraint's operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_framework,
    'Is the imago Dei kernel and its derived principles a discovered natural law (immutable, universal, binding regardless of acceptance) or a constructed theological framework (legitimate within its tradition but not universally binding)?',
    'Philosophical analysis of natural law claims; cross-cultural and cross-religious examination of whether human dignity principles converge or diverge; historical analysis of how Catholic Social Doctrine has adapted its applications while claiming kernel immutability',
    'If natural law: mountain classification is correct, and resistance/low accessibility collapse indicate humanity''s failure to recognize binding truth. If constructed: mountain classification is false summit, and the constraint is tangled rope (genuine coordination function with embedded extraction through theological authority claims).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_framework, conceptual, 'Whether imago Dei kernel is natural law or constructed framework').

omega_variable(
    subsidiarity_principle_operationalization,
    'Does the subsidiarity principle (decisions should be made at the most local competent level) provide determinate guidance for AI governance, or does its application depend on prior theological commitments about what counts as ''competent'' and ''local''?',
    'Case analysis of subsidiarity applications in AI governance contexts; identification of where Catholic and secular interpreters diverge on what subsidiarity requires; examination of whether divergences trace to theological premises or to empirical disagreements',
    'If determinate: coordination function is stronger, extraction is lower (the principle works independently of theological grounding). If theology-dependent: extraction is higher (the principle''s apparent universality masks sectarian premises).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_principle_operationalization, empirical, 'Whether subsidiarity provides determinate AI governance guidance').

omega_variable(
    secular_translation_coherence,
    'Can Catholic Social Doctrine''s AI governance principles be translated into secular language without loss of coherence or action-guiding force?',
    'Comparative analysis of Catholic and secular AI ethics frameworks; identification of where secular translations of CST principles (common good, solidarity, subsidiarity) produce equivalent policy recommendations vs. where they diverge; assessment of whether divergences trace to theological premises or to different empirical assumptions',
    'If coherent translation possible: the framework''s coordination function can be preserved while reducing extraction (theological authority becomes optional). If translation fails: the framework''s coherence depends on its theological grounding, and extraction is structural (you must accept the theology to use the framework effectively).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_translation_coherence, empirical, 'Whether CST principles retain coherence in secular translation').

omega_variable(
    magisterial_authority_scope,
    'Does the Magisterium''s teaching authority extend to technical AI governance questions (algorithm design, data governance, liability frameworks), or only to the moral principles that should guide such decisions?',
    'Theological analysis of magisterial authority scope; examination of how the Church has historically distinguished between binding moral principles and prudential applications; case analysis of where recent encyclicals on AI have made technical vs. moral claims',
    'If authority extends to technical questions: suppression is higher (the Church claims competence to adjudicate engineering and policy questions). If authority limited to moral principles: suppression is lower (technical experts retain autonomy within moral constraints).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(magisterial_authority_scope, conceptual, 'Scope of magisterial authority over AI technical questions').

omega_variable(
    worker_protection_vs_innovation,
    'When Catholic Social Doctrine''s worker protection principles (derived from human dignity and solidarity) conflict with AI innovation that could increase productivity but displace labor, which takes priority, and who decides?',
    'Analysis of how Catholic Social Doctrine has historically balanced labor protection and economic development; examination of whether the framework provides determinate guidance or leaves the balance to prudential judgment; identification of who has authority to make that judgment (local bishops, national conferences, the Vatican, secular authorities in consultation with the Church)',
    'If worker protection has lexical priority: the framework is more extractive toward AI developers (innovation is subordinated to labor protection). If balance is prudential: the framework is more coordinative (allows context-sensitive tradeoffs). If authority is unclear: suppression is higher (the framework creates uncertainty about what is permissible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_protection_vs_innovation, preference, 'Priority and authority in worker protection vs. innovation tradeoffs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_dignity_ai_governance_flat_control, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csd_ai_theater_1950, human_dignity_ai_governance_flat_control, theater_ratio, 0, 0.25).
narrative_ontology:measurement(csd_ai_theater_1970, human_dignity_ai_governance_flat_control, theater_ratio, 20, 0.28).
narrative_ontology:measurement(csd_ai_theater_1990, human_dignity_ai_governance_flat_control, theater_ratio, 40, 0.32).
narrative_ontology:measurement(csd_ai_theater_2000, human_dignity_ai_governance_flat_control, theater_ratio, 50, 0.35).
narrative_ontology:measurement(csd_ai_theater_2010, human_dignity_ai_governance_flat_control, theater_ratio, 60, 0.35).
narrative_ontology:measurement(csd_ai_theater_2025, human_dignity_ai_governance_flat_control, theater_ratio, 75, 0.35).

% Extraction over time
narrative_ontology:measurement(csd_ai_extract_1950, human_dignity_ai_governance_flat_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(csd_ai_extract_1970, human_dignity_ai_governance_flat_control, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(csd_ai_extract_1990, human_dignity_ai_governance_flat_control, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(csd_ai_extract_2000, human_dignity_ai_governance_flat_control, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(csd_ai_extract_2010, human_dignity_ai_governance_flat_control, base_extractiveness, 60, 0.47).
narrative_ontology:measurement(csd_ai_extract_2025, human_dignity_ai_governance_flat_control, base_extractiveness, 75, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(csd_ai_suppress_1950, human_dignity_ai_governance_flat_control, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(csd_ai_suppress_1970, human_dignity_ai_governance_flat_control, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(csd_ai_suppress_1990, human_dignity_ai_governance_flat_control, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(csd_ai_suppress_2000, human_dignity_ai_governance_flat_control, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(csd_ai_suppress_2010, human_dignity_ai_governance_flat_control, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(csd_ai_suppress_2025, human_dignity_ai_governance_flat_control, suppression_requirement, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_dignity_ai_governance_flat_control, identity_coordination).

% DUAL FORMULATION NOTE:
% This is the flat construction of Catholic Social Doctrine's AI governance authority. The constraint could be decomposed into multiple readings (originalist vs. developmental, conservative vs. progressive, Global North vs. Global South interpretations), but this story models the substrate as a single constraint with perspectival disagreement captured through the (P,T,E,S) tuple and omega variables. The contestation over whether CST principles are natural law or constructed framework is routed through omega natural_law_vs_constructed_framework rather than through separate reading stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
