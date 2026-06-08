% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method_flat_control, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: usul_al_fiqh_method_flat_control
 *   human_readable: Usul al-Fiqh Jurisprudential Methodology
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   Usul al-fiqh is the methodological framework for deriving Islamic law
 *   from foundational sources (Quran, Sunnah, ijma', qiyas). Developed
 *   systematically in the 8th-10th centuries CE, it represents one of the
 *   world's most sophisticated pre-modern legal methodologies. The framework
 *   coordinates scholarly work across schools (madhahib), geographies, and
 *   centuries by providing shared interpretive principles, training
 *   infrastructure, and criteria for valid legal reasoning. However, the same
 *   methodological requirements that enable coordination also create
 *   extraction: textual primacy constrains reform efforts, consensus
 *   mechanisms can function as gatekeeping, and analogical reasoning
 *   requirements may limit innovation. The constraint exhibits different
 *   types from different structural positions: established scholars
 *   experience genuine coordination (rope), reform-oriented scholars
 *   experience mixed coordination and constraint (tangled_rope),
 *   legally-bound subjects in state-enforcement contexts experience coercion
 *   (snare), modernist movements see transitional infrastructure (scaffold),
 *   and some state religious authorities maintain it as performative
 *   legitimation (piton). The temporal measurements show gradual increases in
 *   both theater_ratio and extractiveness over 1200 years, reflecting
 *   institutional ossification and the gap between methodological ideals and
 *   actual practice, particularly in contexts where usul al-fiqh citations
 *   serve state legitimation rather than genuine jurisprudential reasoning.
 *
 * KEY AGENTS:
 *   - Legal Scholars (Ulama): Primary beneficiaries (institutional/constrained) — the methodology coordinates their work, provides career structure, and grants interpretive authority
 *   - Judicial Institutions: Beneficiaries (institutional/constrained) — usul al-fiqh provides systematic decision-making framework and legitimacy
 *   - Interpretive Schools (Madhahib): Beneficiaries (institutional/constrained) — the methodology enables school identity and cumulative tradition
 *   - Legal Practitioners (Muftis, Qadis): Beneficiaries (moderate/mobile) — the framework provides tools for addressing novel questions
 *   - Reform-Oriented Scholars: Mixed position (moderate/constrained) — benefit from tradition's authority while bearing costs of methodological constraints
 *   - Legally Bound Subjects: Victims in state-enforcement contexts (powerless/trapped) — bear costs of rulings without voice in derivation
 *   - Modernist Reform Movements: Organized agents (organized/mobile) — working within and beyond the methodology toward alternative frameworks
 *   - State Religious Authorities: Institutional actors (institutional/arbitrage) — in some contexts, maintain usul al-fiqh as performative legitimation
 *   - Comparative Legal Scholars: Analytical observers (analytical/analytical) — see sophisticated coordination mechanism with moderate extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method_flat_control, 0.28).
domain_priors:suppression_score(usul_al_fiqh_method_flat_control, 0.42).
domain_priors:theater_ratio(usul_al_fiqh_method_flat_control, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method_flat_control, extractiveness, 0.28).
narrative_ontology:constraint_metric(usul_al_fiqh_method_flat_control, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method_flat_control, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method_flat_control, rope).
narrative_ontology:human_readable(usul_al_fiqh_method_flat_control, "Usul al-Fiqh Jurisprudential Methodology").
narrative_ontology:topic_domain(usul_al_fiqh_method_flat_control, "islamic_jurisprudence/legal_theory/comparative_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method_flat_control, 'a09ba15f-789e-4152-a6ce-842a92f5c1e7').
narrative_ontology:cs_kernel_codification('a09ba15f-789e-4152-a6ce-842a92f5c1e7', formalized).
narrative_ontology:cs_authority_grounding('a09ba15f-789e-4152-a6ce-842a92f5c1e7', lineage).
narrative_ontology:cs_interpretation_layer_present('a09ba15f-789e-4152-a6ce-842a92f5c1e7').
narrative_ontology:cs_created_at('a09ba15f-789e-4152-a6ce-842a92f5c1e7', '2026-01-15T14:32:00Z').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(usul_al_fiqh_method_flat_control, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, legal_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, judicial_institutions).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, interpretive_schools).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, legal_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, established_madhhab_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, contemporary_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, reform_oriented_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, state_religious_authorities).
narrative_ontology:constraint_victim(usul_al_fiqh_method_flat_control, reform_oriented_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method_flat_control, legally_bound_subjects).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method_flat_control, textual_primacy_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method_flat_control, systematic_derivation_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior scholars within established schools (Hanafi, Maliki, Shafi'i, Hanbali) who have spent decades mastering usul al-fiqh methodology. They train students, issue fatwas, and participate in cross-school scholarly dialogue. The methodology provides their authority, coordinates their work with scholars across time and space, and structures their careers. Exit is constrained — abandoning usul al-fiqh would mean leaving the tradition entirely — but they are clear beneficiaries of the system.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, established_madhhab_scholars, beneficiary,
    institutional, civilizational, constrained, global).

% Practicing jurists addressing contemporary questions (bioethics, Islamic finance, technology) using usul al-fiqh tools. They could work in secular legal systems but choose Islamic jurisprudence. The methodology provides systematic tools for deriving rulings on novel questions while maintaining continuity with tradition. Mobile exit options — they have transferable legal skills — and experience the framework as enabling rather than constraining.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, contemporary_jurists, beneficiary,
    moderate, biographical, mobile, national).

% Scholars advocating for reforms (gender equality, human rights compatibility, contextualist interpretation) who work within usul al-fiqh to build legitimacy but experience its methodological requirements as constraints. They benefit from the tradition's authority and systematic tools but bear costs of textual primacy requirements, consensus mechanisms that can suppress minority positions, and peer review gatekeeping. Constrained exit — leaving the framework means losing Islamic legitimacy for their reform efforts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, reform_oriented_scholars, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method_flat_control, reform_oriented_scholars, beneficiary).

% Individuals subject to fiqh rulings derived through usul methodology in contexts where Islamic law is state-enforced (family law, inheritance, criminal law in some jurisdictions). They have no voice in the interpretive process, no ability to challenge rulings through the methodology, and no exit from the legal framework due to geographic, economic, and identity constraints. They bear the costs of rulings (legal obligations, restrictions, penalties) without participating in their derivation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, legally_bound_subjects, payer,
    powerless, immediate, trapped, local).

% Organized coalitions (progressive scholars, feminist jurists, human rights advocates) working to reform Islamic jurisprudence. They use usul al-fiqh strategically to build legitimacy while simultaneously developing alternative interpretive frameworks (maqasid-centered approaches that prioritize higher objectives over textual literalism, contextualist hermeneutics). They see the classical methodology as transitional infrastructure — a bridge to more flexible systems, not the permanent structure. Mobile exit options and generational time horizon give them agency to build alternatives.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, modernist_reform_movements, agenda_setter,
    organized, generational, mobile, global).

% State-appointed religious councils and official muftis in some national contexts who maintain usul al-fiqh as performative legitimation. Actual legal decisions often follow state policy priorities, with classical methodology cited post-hoc to justify predetermined outcomes. They benefit from the framework's legitimating function while the methodology's genuine jurisprudential function has atrophied. Arbitrage exit options — they could work in other institutional contexts — and they experience the constraint as theatrical maintenance rather than functional coordination.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, state_religious_authorities, beneficiary,
    institutional, biographical, arbitrage, national).

% The madhahib (Hanafi, Maliki, Shafi'i, Hanbali schools) as institutional entities. These are not individual agents but organizational structures that persist across centuries. Usul al-fiqh enables their identity, cumulative tradition, and cross-school dialogue. Listed for narrative completeness but excluded from beneficiary/victim derivation (agent=false) because a school is an organizational abstraction, not an actor that collects from the constraint.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, interpretive_schools, beneficiary,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_non_agent(usul_al_fiqh_method_flat_control, interpretive_schools).

% Scholars studying Islamic jurisprudence from comparative legal theory perspective. They analyze usul al-fiqh as a sophisticated coordination solution to the problem of deriving coherent legal systems from foundational texts across diverse contexts and time periods. They neither collect from nor pay into the constraint — their relationship is analytical observation of its structure and function.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Usul al-fiqh coordinates the derivation of Islamic law across schools, geographies, and centuries by providing shared interpretive principles (textual primacy, analogical reasoning, consensus mechanisms), systematic training infrastructure, and criteria for valid legal reasoning. It solves the problem of maintaining legal coherence and cross-school dialogue while allowing for contextual adaptation.
% TRANSFER_FUNCTION: The methodology transfers interpretive authority and career structure to legal scholars (ulama), judicial legitimacy to institutions, and systematic decision-making tools to practitioners. It moves legal obligations and restrictions to subjects (particularly in state-enforcement contexts) and constrains reform efforts through methodological requirements (textual primacy, consensus mechanisms, analogical reasoning).
% ABSENT_VOICES: Legally bound subjects in state-enforcement contexts have minimal voice in the interpretive process despite bearing costs of rulings. Women scholars historically excluded from formal usul al-fiqh training and institutional positions. Non-Muslim minorities subject to Islamic law in some jurisdictions. Reform advocates whose positions fall outside consensus boundaries. These groups would object to aspects of the methodology's operation (gatekeeping, constraint of reform, lack of participatory mechanisms) but are not in the interpretive conversation or have limited institutional power within it.
% DISAPPEARANCE_RATIONALE: If usul al-fiqh disappeared overnight, Islamic legal systems would lose their coordination infrastructure. Scholars would lack shared criteria for valid reasoning, cross-school dialogue would collapse, training would become ad-hoc, and legal derivation would fragment into competing approaches without systematic methodology. The arrangements depend on the constraint — it is not a natural fact but a constructed coordination mechanism. However, the degree of rearrangement would vary by context: in voluntary Islamic law contexts, alternative interpretive frameworks might fill the gap relatively smoothly; in state-enforcement contexts, the disruption would be more severe as judicial systems lost their legitimating methodology.
% FOUNDING_PROBLEM: The founding problem was the need for systematic methodology to derive Islamic law from foundational sources (Quran, Sunnah) in the absence of the Prophet Muhammad's direct guidance. Early Muslim communities faced novel questions not explicitly addressed in revelation, requiring principles for analogical reasoning, criteria for valid hadith, and mechanisms for scholarly consensus. Without systematic methodology, legal derivation was ad-hoc and inconsistent across regions and scholars. Usul al-fiqh emerged in the 8th-10th centuries CE to solve this coordination problem by codifying interpretive principles and establishing shared criteria for valid legal reasoning.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live: contemporary Muslim communities still face novel questions (bioethics, technology, finance) requiring systematic derivation from foundational sources. This status is corroborated by: (1) continued production of usul al-fiqh scholarship and training programs across all major madhahib, (2) active use of the methodology by contemporary jurists addressing novel questions (documented in fatwa databases and Islamic finance institutions), (3) reform scholars who critique aspects of usul al-fiqh but still engage with the problem of systematic derivation (maqasid scholars, contextualist interpreters), and (4) comparative legal scholars who analyze usul al-fiqh as a functioning coordination mechanism, not a historical artifact. Corroboration comes from multiple seats: practicing jurists (beneficiaries), reform scholars (payers who still engage the problem), and analytical observers (outside the beneficiary set).
narrative_ontology:disappearance_verdict(usul_al_fiqh_method_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method_flat_control, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ESTABLISHED MADHHAB SCHOLAR (ROPE) — Experiences usul al-fiqh as genuine coordination infrastructure. The shared methodology enables cross-school dialogue, systematic training, and cumulative legal development. Constrained exit (cannot abandon the framework without leaving the tradition) but clear beneficiary — the methodology coordinates scholarly work across centuries and geographies.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: CONTEMPORARY JURIST (ROPE) — Mobile practitioner who could work in secular legal systems but chooses Islamic jurisprudence. Experiences usul al-fiqh as coordination: the methodology provides systematic tools for addressing novel questions (bioethics, finance, technology) within the tradition. Low extraction — the framework enables rather than constrains their work.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORM-ORIENTED SCHOLAR (TANGLED ROPE) — Experiences both coordination and extraction. The methodology provides legitimacy and systematic tools (coordination function) but also constrains reform efforts through requirements for textual grounding and consensus mechanisms. Active enforcement through peer review and institutional gatekeeping. Moderate extraction — benefits from the tradition's authority while bearing costs of methodological constraints.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: LEGALLY BOUND SUBJECT (SNARE) — Individual subject to fiqh rulings derived through usul methodology but with no voice in the interpretive process and no exit from the legal framework (in contexts where Islamic law is state-enforced). Trapped by geographic, economic, and identity constraints. Experiences high extraction — bears costs of rulings without participating in their derivation.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: MODERNIST REFORM MOVEMENT (SCAFFOLD) — Organized coalition (progressive scholars, feminist jurists, human rights advocates) who see usul al-fiqh as transitional infrastructure. They work within the methodology to build legitimacy for reforms while simultaneously developing alternative interpretive frameworks (maqasid-centered approaches, contextualist hermeneutics). Sunset logic: the classical methodology is a bridge to more flexible interpretive systems, not the permanent structure.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: STATE RELIGIOUS AUTHORITY (PITON) — In some contexts, state-appointed religious councils maintain usul al-fiqh as performative legitimation while actual legal decisions follow state policy priorities. The methodology's function has atrophied to theatrical citation of classical sources to justify predetermined outcomes. High theater ratio from this institutional position — the framework persists through inertia and legitimation needs, not functional jurisprudence.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (ROPE) — From a civilizational analytical perspective, usul al-fiqh represents a sophisticated coordination solution to the problem of deriving coherent legal systems from foundational texts across diverse contexts and time periods. The methodology's core function (systematic derivation, cross-school dialogue, training infrastructure) remains robust. Extraction is moderate and largely consists of legitimate coordination costs plus some institutional gatekeeping. Not a natural law (emerges from historical development, not physical necessity) but a genuine coordination mechanism.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(usul_al_fiqh_method_flat_control, TR),
    TR >= 0.70.

:- end_tests(usul_al_fiqh_method_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The methodology creates genuine coordination value (systematic derivation, cross-school dialogue, training infrastructure) but also imposes costs: textual primacy requirements constrain reform, consensus mechanisms can suppress minority positions, analogical reasoning requirements may limit innovation addressing novel contexts. The extraction is real but not severe — much of what appears as constraint is legitimate coordination cost. The value reflects that usul al-fiqh is primarily a coordination mechanism with embedded gatekeeping, not primarily an extraction mechanism with coordination cover. Suppression (0.42): Moderate. Significant barriers exist for those seeking to work outside the framework: institutional gatekeeping (peer review, appointment to judicial positions), requirement for madhhab affiliation in many contexts, and in some regions state enforcement of usul-derived rulings. However, suppression is not total — alternative interpretive approaches exist (maqasid-centered reasoning, contextualist hermeneutics), cross-madhhab borrowing occurs, and in many contexts Islamic law is voluntary rather than state-enforced. Theater ratio (0.35): Moderate-low. Some performative elements exist (ritual citation of classical authorities, formulaic application of qiyas in predetermined conclusions, state religious councils citing usul al-fiqh to legitimize policy decisions) but substantial functional content remains. The methodology genuinely coordinates scholarly work, enables systematic training, and provides tools for addressing novel questions. Theater has increased over the 1200-year interval as institutional contexts have shifted and the gap between methodological ideals and actual practice has grown.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same methodological framework appears differently from different structural positions. Established madhhab scholars see genuine coordination (rope) — the methodology enables their work across centuries and geographies. Contemporary jurists with mobile exit options also see coordination (rope) — the framework provides systematic tools rather than constraints. Reform-oriented scholars see tangled rope — both coordination (legitimacy, systematic tools) and extraction (constraints on reform, gatekeeping). Legally bound subjects in state-enforcement contexts see snare — they bear costs without voice or exit. Modernist reform movements see scaffold — transitional infrastructure being superseded by alternative frameworks. State religious authorities in some contexts see piton — performative legitimation maintained through inertia. The analytical observer sees rope — genuine coordination mechanism with moderate extraction. The perspectival gaps are not disagreements about facts but structural differences in experienced extraction based on power, exit options, and relationship to the constraint's benefits and costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (legal scholars, judicial institutions, interpretive schools, legal practitioners) experience low directionality — the methodology flows toward them in the form of authority, career structure, and coordination infrastructure. Their effective extraction is low or negative (they collect from the constraint). Reform-oriented scholars occupy a mixed position — they benefit from the tradition's authority while bearing costs of methodological constraints, producing moderate directionality and moderate effective extraction. Legally bound subjects in state-enforcement contexts are victims with high directionality — they bear costs of rulings without participating in derivation, producing high effective extraction. The analytical observer sees moderate extraction overall — the coordination function is genuine but embedded gatekeeping and constraint of reform are real costs. No directionality overrides are needed — the beneficiary/victim declarations plus exit options produce appropriate directionality values across perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing coordination function from extraction. Usul al-fiqh genuinely coordinates: it enables systematic legal derivation, cross-school dialogue, cumulative tradition, and training infrastructure. This is not cover for pure extraction — the coordination function is real and substantial. However, the same methodological requirements that enable coordination also create extraction: textual primacy constrains reform, consensus mechanisms can function as gatekeeping, analogical reasoning requirements may limit innovation. The tangled_rope classification from reform-oriented scholars captures this duality — both functions coexist in the same structure. The snare classification from legally bound subjects in state-enforcement contexts reflects that when the methodology's outputs are coercively enforced, the extraction function dominates for those without voice or exit. The scaffold classification from modernist movements reflects that some organized agents are building alternative frameworks while working within the current one. The piton classification from some state religious authorities reflects that in certain institutional contexts the methodology's function has atrophied to performative legitimation. All classifications are structurally valid from their respective positions — the presheaf over observation sites is the complete picture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_primacy_necessity,
    'Is the requirement for textual grounding (Quran/Sunnah primacy) an inherent feature of Islamic legal reasoning or a contingent historical development that could be revised?',
    'Historical analysis of pre-usul legal reasoning; examination of maqasid-centered approaches that subordinate textual literalism to higher objectives; comparative study of other religious legal traditions'' source hierarchies',
    'If inherent: usul al-fiqh is closer to mountain (immutable framework). If contingent: current extraction from reform scholars is avoidable through methodological revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_primacy_necessity, conceptual, 'Whether textual primacy is inherent or contingent to Islamic jurisprudence').

omega_variable(
    ijma_consensus_mechanism,
    'Does ijma'' (scholarly consensus) function as genuine epistemic convergence or as institutional gatekeeping that suppresses minority positions?',
    'Analysis of historical ijma'' claims: correlation between consensus declarations and power concentration; tracking of initially-minority positions that later gained acceptance; comparison of consensus mechanisms across different madhahib',
    'If genuine convergence: coordination function dominates (rope from more perspectives). If gatekeeping: extraction function dominates (snare/tangled_rope from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ijma_consensus_mechanism, empirical, 'Whether consensus mechanism is epistemic or extractive').

omega_variable(
    qiyas_analogical_constraint,
    'Does qiyas (analogical reasoning) enable creative legal adaptation or does it constrain innovation by requiring precedent-matching?',
    'Comparative analysis of legal systems with and without analogical reasoning requirements; examination of novel rulings'' success rates under qiyas vs. maqasid-based reasoning; historical study of legal innovation patterns',
    'If enabling: coordination function confirmed. If constraining: extraction from contemporary jurists addressing novel questions is higher than base metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_analogical_constraint, empirical, 'Whether analogical reasoning enables or constrains legal innovation').

omega_variable(
    madhhab_lock_in,
    'Do the established schools (madhahib) represent cumulative wisdom that reduces error, or do they create path-dependent lock-in that prevents correction of historical mistakes?',
    'Analysis of cross-madhhab differences: are divergences random drift or systematic responses to different contexts? Examination of inter-madhhab borrowing rates and conditions. Study of rulings later recognized as errors — were they correctable within the madhhab framework?',
    'If cumulative wisdom: low extraction, genuine coordination. If lock-in: higher extraction than base metrics suggest, particularly for reform-oriented scholars.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(madhhab_lock_in, empirical, 'Whether madhhab structures represent wisdom or lock-in').

omega_variable(
    state_enforcement_boundary,
    'Where usul-derived fiqh is state-enforced, does the methodology''s extraction shift from coordination costs to coercive extraction?',
    'Comparison of extraction levels in contexts with voluntary vs. state-enforced Islamic law; analysis of legal subject exit options and experienced suppression across different political contexts',
    'If state enforcement fundamentally changes extraction: the constraint should be decomposed into separate stories for voluntary vs. coercive contexts. If extraction remains similar: single story with perspectival variation is appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_enforcement_boundary, empirical, 'Whether state enforcement changes the constraint''s extractive character').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method_flat_control, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_fiqh_theater_classical, usul_al_fiqh_method_flat_control, theater_ratio, 0, 0.2).
narrative_ontology:measurement(usul_fiqh_theater_medieval, usul_al_fiqh_method_flat_control, theater_ratio, 400, 0.25).
narrative_ontology:measurement(usul_fiqh_theater_ottoman, usul_al_fiqh_method_flat_control, theater_ratio, 800, 0.28).
narrative_ontology:measurement(usul_fiqh_theater_colonial, usul_al_fiqh_method_flat_control, theater_ratio, 1000, 0.32).
narrative_ontology:measurement(usul_fiqh_theater_contemporary, usul_al_fiqh_method_flat_control, theater_ratio, 1200, 0.35).

% Extraction over time
narrative_ontology:measurement(usul_fiqh_extract_classical, usul_al_fiqh_method_flat_control, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(usul_fiqh_extract_medieval, usul_al_fiqh_method_flat_control, base_extractiveness, 400, 0.18).
narrative_ontology:measurement(usul_fiqh_extract_ottoman, usul_al_fiqh_method_flat_control, base_extractiveness, 800, 0.22).
narrative_ontology:measurement(usul_fiqh_extract_colonial, usul_al_fiqh_method_flat_control, base_extractiveness, 1000, 0.26).
narrative_ontology:measurement(usul_fiqh_extract_contemporary, usul_al_fiqh_method_flat_control, base_extractiveness, 1200, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method_flat_control, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This is the flat construction of usul al-fiqh as a single constraint. Contestation over textual primacy, consensus mechanisms, and analogical reasoning requirements is captured through perspectival disagreement and omega variables rather than through decomposition into separate readings. If the constraint were to be decomposed, natural split points would be: (1) voluntary vs. state-enforced contexts (different suppression and extraction profiles), (2) classical vs. modernist interpretive frameworks (different methodological requirements), (3) different madhahib (different analogical reasoning patterns and consensus mechanisms). The current flat construction treats these as perspectival variations within a single constraint rather than as structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
