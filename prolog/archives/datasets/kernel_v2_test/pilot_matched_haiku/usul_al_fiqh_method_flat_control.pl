% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: usul_al_fiqh_method_flat_control
 *   human_readable: Usul al-Fiqh: Jurisprudential Methodology in Islamic Law
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   Usul al-fiqh (jurisprudential methodology) represents the shared
 *   commitment within Islamic legal tradition to deriving law from
 *   foundational sources (Quran, Sunnah, ijma', qiyas) through systematic
 *   interpretive principles. This constraint exhibits the full range of DR
 *   classification from different structural positions. The established legal
 *   schools (Hanafi, Maliki, Shafi'i, Hanbali) experience usul as
 *   coordination — the shared methodology legitimates their jurisprudence and
 *   enables their fatwas to be recognized across the Islamic world. Heterodox
 *   interpreters experience it as extraction — they are suppressed through
 *   accusations of bid'ah (innovation) and takfir (excommunication) when they
 *   propose alternative methodologies. Institutional ulama (state-appointed
 *   muftis, Al-Azhar scholars) experience it as a tool for legitimizing
 *   predetermined conclusions. Reform movements experience it as mixed
 *   coordination and constraint — the framework enables their fatwas to be
 *   recognized as Islamic law while limiting the scope of acceptable
 *   innovation. The analytical observer risks naturalizing this institutional
 *   arrangement as an immutable law of legal reasoning itself. The
 *   constraint's extractiveness (0.35) reflects moderate asymmetry:
 *   established schools benefit from methodological authority, but the
 *   extraction is not total because heterodox interpreters can sometimes
 *   reframe their positions within usul principles. Suppression (0.42)
 *   reflects institutional enforcement mechanisms (takfir, exclusion from
 *   fatwa authority, scholarly delegitimization) that prevent heterodox
 *   methodologies from being recognized. Theater ratio (0.38) reflects that
 *   while usul principles are still invoked, much contemporary Islamic
 *   jurisprudence operates through informal consensus and institutional
 *   authority rather than rigorous methodological application.
 *
 * KEY AGENTS:
 *   - Established Legal Schools (Hanafi, Maliki, Shafi'i, Hanbali): Primary beneficiaries (institutional/arbitrage) — usul methodology legitimates their jurisprudence and enables their authority across the Islamic world
 *   - Institutional Ulama (state-appointed muftis, Al-Azhar, Dar al-Ifta): Secondary beneficiaries (institutional/arbitrage) — use usul framework to legitimize state-sanctioned fatwas and maintain institutional authority
 *   - Heterodox Interpreters (Akhbari school, reformist scholars, contemporary innovators): Primary victims (powerless/identity_locked) — suppressed through takfir accusations and institutional exclusion; identity-fused with Islamic jurisprudential tradition so exit is unthinkable
 *   - Regional Jurists: Secondary actors (moderate/constrained) — benefit from usul framework for scholarly legitimacy while bearing costs of justifying departures from established positions
 *   - Reform Movements: Organized victims (organized/constrained) — seek to reinterpret usul principles for contemporary issues; benefit from framework's legitimacy while facing accusations of innovation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as inherent to legal reasoning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method_flat_control, 0.35).
domain_priors:suppression_score(usul_al_fiqh_method_flat_control, 0.42).
domain_priors:theater_ratio(usul_al_fiqh_method_flat_control, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method_flat_control, extractiveness, 0.35).
narrative_ontology:constraint_metric(usul_al_fiqh_method_flat_control, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method_flat_control, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method_flat_control, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method_flat_control, "Usul al-Fiqh: Jurisprudential Methodology in Islamic Law").
narrative_ontology:topic_domain(usul_al_fiqh_method_flat_control, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(usul_al_fiqh_method_flat_control, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, established_legal_schools).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, institutional_ulama).
narrative_ontology:constraint_victim(usul_al_fiqh_method_flat_control, heterodox_interpreters).
narrative_ontology:constraint_victim(usul_al_fiqh_method_flat_control, legal_innovation_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, regional_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, reform_movements).
narrative_ontology:constraint_victim(usul_al_fiqh_method_flat_control, regional_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method_flat_control, reform_movements).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method_flat_control, quranic_textual_authority).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method_flat_control, prophetic_precedent_binding).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method_flat_control, scholarly_consensus_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The four major schools (Hanafi, Maliki, Shafi'i, Hanbali) use usul al-fiqh to legitimize their jurisprudence and maintain authority across the Islamic world. They can reinterpret usul principles to justify their positions, shift between schools, and claim methodological authority. They collect authority rents from the constraint's operation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, established_legal_schools, beneficiary,
    institutional, generational, arbitrage, global).

% State-appointed muftis, Al-Azhar scholars, and Dar al-Ifta officials administer the usul framework and use it to legitimize state-sanctioned fatwas. They set the agenda for what counts as valid Islamic jurisprudence and maintain institutional authority over fatwa issuance. They have high exit optionality and collect institutional power rents.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, institutional_ulama, beneficiary,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method_flat_control, institutional_ulama, agenda_setter).

% Scholars proposing alternative methodologies (Akhbari school, reformist approaches, contemporary innovators) face suppression through takfir accusations, institutional exclusion from fatwa authority, and scholarly delegitimization. Their identity is constituted through Islamic jurisprudential tradition, so exit would require abandoning the framework itself. They bear the full cost of methodological enforcement.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, heterodox_interpreters, payer,
    powerless, biographical, identity_locked, global).

% Local scholars responding to regional legal questions benefit from usul framework (provides methodological authority for their fatwas, enables participation in scholarly discourse) while bearing costs (must justify departures from established school positions, face criticism for innovation). They have constrained exit and experience mixed coordination and extraction.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, regional_jurists, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method_flat_control, regional_jurists, beneficiary).

% Modernist scholars and Islamic reformers seek to reinterpret usul principles to address contemporary issues. They benefit from the framework's legitimacy (their fatwas are recognized as Islamic law) while bearing costs (face accusations of bid'ah/innovation, must justify departures from classical methodology). They have constrained exit and experience mixed coordination and constraint.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, reform_movements, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method_flat_control, reform_movements, beneficiary).

% The abstract capacity for Islamic jurisprudence to develop new legal solutions for novel problems is constrained by usul methodology. While innovation is not eliminated (it is redirected into usul-compliant forms), the constraint limits the scope and speed of legal adaptation. This is a non-agent entity kept for narrative completeness — it represents the collective good of legal innovation capacity.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, legal_innovation_capacity, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(usul_al_fiqh_method_flat_control, legal_innovation_capacity).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Usul al-fiqh solves the genuine coordination problem of how to derive Islamic law from foundational sources in a systematic way that enables cross-school dialogue and fatwa recognition. Without shared methodology, Islamic jurisprudence would fragment into incompatible schools with no common framework for resolving disputes or recognizing each other's authority.
% TRANSFER_FUNCTION: The constraint transfers authority from heterodox interpreters to established legal schools and institutional ulama. It moves legitimacy (heterodox fatwas are delegitimized while established school fatwas are legitimized), scholarly recognition (heterodox scholars are excluded from fatwa authority while established scholars are recognized), and institutional power (state-appointed ulama control fatwa issuance through usul methodology).
% ABSENT_VOICES: Heterodox interpreters and contemporary innovators are partially excluded from the conversation about what counts as valid Islamic jurisprudence. While they can publish and maintain scholarly communities, they are excluded from institutional fatwa authority and face accusations of bid'ah. Their exclusion is enforced through takfir accusations and institutional barriers rather than formal rules.
% DISAPPEARANCE_RATIONALE: If usul al-fiqh disappeared overnight, Islamic jurisprudence would rearrange itself significantly. The established legal schools would lose their methodological legitimacy and would need to justify their fatwas through alternative frameworks. Institutional ulama would lose their primary tool for legitimizing state-sanctioned fatwas. Heterodox interpreters would gain space to propose alternative methodologies. The constraint's disappearance would fundamentally alter the distribution of authority in Islamic legal systems.
% FOUNDING_PROBLEM: Islamic jurisprudence needed a systematic methodology for deriving law from foundational sources (Quran, Sunnah, ijma', qiyas) in a way that would enable cross-school dialogue and prevent arbitrary interpretation. Usul al-fiqh was developed to solve this coordination problem by establishing shared principles for textual interpretation, analogical reasoning, and consensus determination.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live across Islamic jurisprudential tradition. Contemporary scholars across schools acknowledge the need for systematic methodology to derive law from foundational sources. However, institutional ulama increasingly use usul principles post-hoc to legitimize predetermined conclusions rather than as genuine methodological constraints, suggesting the founding problem is being solved through institutional authority rather than rigorous methodology. Independent scholars and reform movements attest that the founding problem persists and that usul methodology is still necessary, though they dispute whether current institutional applications genuinely solve it.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method_flat_control, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HETERODOX INTERPRETER (SNARE) — Structurally mobile (could propose alternative methodologies) but identity-fused with Islamic jurisprudential tradition. Exit would require abandoning the interpretive framework itself, not merely disagreeing within it. Faces suppression through takfir (excommunication) accusations, institutional exclusion from fatwa authority, and loss of scholarly legitimacy. The constraint operates as pure extraction: the heterodox interpreter bears the cost of methodological enforcement while the established schools collect the authority rent.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL JURIST (TANGLED ROPE) — Constrained by the need to maintain scholarly legitimacy within the tradition while also responding to local legal questions. Benefits from the usul framework (provides methodological authority for their fatwas, enables participation in scholarly discourse) while bearing costs (must justify departures from established school positions, faces criticism for innovation). Mixed coordination and extraction: the framework enables their work but also constrains their authority.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESTABLISHED LEGAL SCHOOL (ROPE) — Institutional beneficiary with high exit optionality (can reinterpret usul principles to justify school positions, can claim methodological authority). Experiences the constraint as coordination: usul al-fiqh provides the shared framework that legitimates their school's jurisprudence and enables their fatwas to be recognized across the Islamic world. Net beneficiary — the constraint subsidizes their authority.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL ULAMA (ROPE) — Institutional actors (Al-Azhar, Dar al-Ifta, state-appointed muftis) who administer the usul framework. Experience it as coordination: the methodology provides legitimacy for state-sanctioned fatwas and enables institutional authority over legal interpretation. Arbitrage exit (can shift between schools, reinterpret principles, or claim methodological innovation). Net beneficiary — the constraint enables their institutional power.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM MOVEMENT (TANGLED ROPE) — Organized agents (modernist scholars, Islamic reformers) who seek to reinterpret usul principles to address contemporary issues. Benefit from the framework's legitimacy (their fatwas are recognized as Islamic law) while bearing costs (face accusations of bid'ah/innovation, must justify departures from classical methodology). The constraint both enables and constrains their work: it provides the authority structure they need while limiting the scope of acceptable innovation.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CLASSICAL USUL APPARATUS (PITON) — The formal methodology (qiyas rules, istihsan procedures, maslaha reasoning) has become increasingly theatrical in contemporary practice. Modern fatwas often invoke usul principles post-hoc to justify conclusions reached through other means (political pressure, institutional mandate, practical necessity). The apparatus persists through institutional inertia and scholarly tradition rather than functional necessity — it is maintained as a legitimacy ritual. Theater ratio (0.38) reflects that while the methodology is still invoked, much contemporary Islamic jurisprudence operates through informal consensus and institutional authority rather than rigorous usul application.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of jurisprudential methodology is inherent to any legal system: all law requires principles for interpreting foundational texts, resolving conflicts, and extending rules to new cases. The usul framework appears as a natural law of legal reasoning itself — an immutable requirement of how law functions. However, the structural data (beneficiaries, victims, enforcement requirements) contradicts this classification. The engine will compute this as a false summit, revealing that what appears as natural law is actually a contingent institutional arrangement that benefits established schools and suppresses heterodox interpretation.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, mountain,
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
    constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.35): Moderate. The constraint exhibits asymmetric benefit distribution — established schools and institutional ulama collect authority rents while heterodox interpreters bear suppression costs. However, the extraction is not severe because: (1) heterodox interpreters can sometimes reframe positions within usul principles, redirecting rather than eliminating innovation capacity; (2) the framework does provide genuine coordination benefits (shared methodology enables cross-school dialogue and fatwa recognition); (3) established schools themselves are constrained by usul principles (they cannot simply impose fatwas without methodological justification). The rising trajectory (0.28 → 0.38 over the interval) reflects increasing institutional enforcement and state capture of usul methodology, particularly through institutional ulama using usul to legitimize predetermined political conclusions. Suppression (0.42): Moderate-high. Institutional enforcement mechanisms include takfir accusations, exclusion from fatwa authority, scholarly delegitimization, and institutional barriers to publication and recognition. However, suppression is not total because heterodox interpreters can maintain scholarly communities, publish in alternative venues, and sometimes gain recognition through reframing within usul principles. The rising trajectory (0.38 → 0.44) reflects increasing institutional enforcement as state-appointed ulama consolidate control over fatwa authority. Theater ratio (0.38): Moderate. Contemporary Islamic jurisprudence invokes usul principles, but much decision-making operates through informal consensus, institutional mandate, and political pressure rather than rigorous methodological application. The theater is lower than in some institutional constraints because usul principles are still genuinely applied in many contexts, particularly in independent scholarship. The rising trajectory (0.32 → 0.38) reflects increasing post-hoc invocation of usul principles to legitimize conclusions reached through other means.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence. The established legal schools see coordination (Rope) — usul methodology enables their authority and legitimates their fatwas. The institutional ulama see coordination (Rope) — the framework provides legitimacy for state-sanctioned fatwas. The heterodox interpreters see extraction (Snare) — they are suppressed through takfir accusations and institutional exclusion. The regional jurists see mixed coordination and extraction (Tangled Rope) — the framework enables their work but constrains their authority. The reform movements see mixed coordination and constraint (Tangled Rope) — they benefit from the framework's legitimacy while facing accusations of innovation. The classical usul apparatus sees degraded function (Piton) — the methodology persists through institutional inertia rather than rigorous application. The analytical observer risks seeing natural law (Mountain) — jurisprudential methodology appears inherent to legal reasoning — but the structural data reveals this as a false summit: the beneficiaries, victims, and enforcement mechanisms indicate a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the constraint. Established schools and institutional ulama are beneficiaries with arbitrage exit options (can reinterpret usul principles, shift between schools, claim methodological authority) — they experience low d (0.1-0.3), producing negative or low effective extraction (they are subsidized by the constraint). Heterodox interpreters are victims with identity-locked exit (structurally mobile but identity-fused with Islamic jurisprudential tradition, so exit is unthinkable) — they experience high d (0.7-0.9), producing high effective extraction. Regional jurists are moderate agents with constrained exit (can reinterpret within limits, but face career risk and scholarly criticism) — they experience moderate d (0.4-0.6), producing moderate effective extraction. Reform movements are organized agents with constrained exit (can propose reinterpretations, but face accusations of innovation) — they experience moderate d (0.4-0.6), producing moderate effective extraction. The constraint's effective extraction is amplified for trapped/identity-locked targets and damped for beneficiaries with arbitrage exit, following the standard directionality derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to provide a shared methodology for deriving Islamic law from foundational sources. This mandate is still live — usul al-fiqh remains the recognized framework for Islamic jurisprudence across schools and regions. However, the constraint exhibits signs of mandatrophy in specific contexts: (1) institutional ulama increasingly use usul principles post-hoc to legitimize predetermined political conclusions rather than as genuine methodological constraints; (2) the classical usul apparatus (qiyas rules, istihsan procedures) has become increasingly theatrical in contemporary practice; (3) the constraint's function has shifted from enabling legal innovation to suppressing heterodox innovation. The constraint resolves the mandatrophy by showing that the mandate (shared methodology) persists while the function (genuine methodological reasoning) has partially atrophied in institutional contexts. The tangled_rope classification captures this: the constraint still provides coordination benefits (shared framework for fatwa recognition) while exhibiting extraction (suppression of heterodox interpretation and post-hoc legitimization of institutional conclusions). The piton perspective captures the theatrical degradation of the classical usul apparatus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed,
    'Is usul al-fiqh a natural law of jurisprudential reasoning (inherent to any legal system) or a constructed institutional arrangement that benefits specific actors?',
    'Comparative analysis of jurisprudential methodologies across legal traditions (common law, civil law, customary law); examination of whether non-Islamic legal systems require equivalent usul frameworks; historical analysis of whether usul principles were discovered or invented',
    'If natural law: mountain classification confirmed, beneficiary presence is FSM false alarm. If constructed: false summit confirmed, tangled_rope classification appropriate, beneficiary extraction is real structural feature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed, conceptual, 'Whether usul al-fiqh is inherent to legal reasoning or contingent institutional arrangement').

omega_variable(
    methodological_closure_mechanism,
    'What mechanism prevents heterodox methodologies from being recognized as legitimate Islamic jurisprudence? Is it logical necessity (the heterodox method is incoherent) or institutional enforcement (the heterodox method is suppressed)?',
    'Analysis of specific heterodox proposals (Akhbari school, Zaydi methodology, contemporary reformist approaches); examination of whether rejection is based on logical refutation or institutional exclusion; study of takfir accusations and their justification',
    'If logical necessity: suppression is low, constraint is closer to rope. If institutional enforcement: suppression is high, constraint is closer to snare. Current assessment (suppression 0.42) assumes mixed mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_closure_mechanism, empirical, 'Whether methodological closure is logically necessary or institutionally enforced').

omega_variable(
    innovation_capacity_measurement,
    'How much legal innovation capacity is actually lost due to usul constraints versus how much is merely redirected into usul-compliant forms (reinterpretation, maslaha reasoning, istihsan)?',
    'Comparative analysis of fatwa innovation rates across periods and schools; examination of whether contemporary fatwas on novel issues (AI, genetic engineering, digital finance) represent genuine innovation or repackaging of classical principles; study of whether usul constraints prevent solutions or merely require different justification pathways',
    'If capacity is lost: victims classification is strong, extraction is real. If capacity is redirected: victims classification is weaker, constraint functions more as coordination with friction. Current assessment (extractiveness 0.35) assumes partial redirection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_capacity_measurement, empirical, 'Whether usul constraints eliminate or redirect legal innovation capacity').

omega_variable(
    institutional_ulama_capture,
    'To what extent do institutional ulama (state-appointed muftis, Al-Azhar scholars) use usul principles to legitimize predetermined political conclusions versus using them as genuine methodological constraints?',
    'Analysis of fatwa justifications in politically sensitive cases (governance, war, economic policy); comparison of usul reasoning in institutional versus independent fatwas; examination of whether institutional ulama invoke different usul principles than independent scholars for similar questions',
    'If high capture: institutional ulama are beneficiaries extracting through false legitimacy, constraint is closer to snare. If low capture: institutional ulama are genuine coordinators, constraint is closer to rope. Current assessment (beneficiary status for institutional ulama) assumes partial capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_ulama_capture, empirical, 'Degree of institutional capture of usul methodology by state-appointed ulama').

omega_variable(
    qiyas_necessity_debate,
    'Is qiyas (analogical reasoning) a necessary component of Islamic jurisprudence or a contingent methodological choice that could be replaced by alternative reasoning frameworks?',
    'Historical analysis of pre-qiyas jurisprudence; examination of Akhbari rejection of qiyas and its consequences; study of whether contemporary Islamic jurisprudence could function without qiyas; analysis of whether qiyas is logically necessary or institutionally mandated',
    'If necessary: qiyas is part of natural law, mountain classification gains support. If contingent: qiyas is institutional choice, false summit classification gains support, beneficiary extraction is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_necessity_debate, conceptual, 'Whether qiyas is logically necessary or institutionally contingent').

omega_variable(
    ijma_consensus_authenticity,
    'Does ijma'' (scholarly consensus) represent genuine agreement among qualified jurists or does it function as a mechanism for enforcing established school positions?',
    'Historical analysis of how ijma'' is determined and who counts as qualified; examination of whether dissenting scholars are excluded from ijma'' calculations; study of whether ijma'' is invoked to suppress heterodox positions; analysis of contemporary ijma'' claims and their actual support base',
    'If genuine consensus: ijma'' is coordination mechanism, constraint is closer to rope. If enforcement mechanism: ijma'' is suppression tool, constraint is closer to snare. Current assessment (suppression 0.42) assumes mixed function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_consensus_authenticity, empirical, 'Whether ijma'' represents genuine consensus or enforces established positions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method_flat_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method_flat_control, theater_ratio, 0, 0.32).
narrative_ontology:measurement(usul_tr_t3, usul_al_fiqh_method_flat_control, theater_ratio, 3, 0.35).
narrative_ontology:measurement(usul_tr_t6, usul_al_fiqh_method_flat_control, theater_ratio, 6, 0.37).
narrative_ontology:measurement(usul_tr_t10, usul_al_fiqh_method_flat_control, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(usul_be_t3, usul_al_fiqh_method_flat_control, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(usul_be_t6, usul_al_fiqh_method_flat_control, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(usul_be_t10, usul_al_fiqh_method_flat_control, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method_flat_control, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(usul_su_t3, usul_al_fiqh_method_flat_control, suppression_requirement, 3, 0.4).
narrative_ontology:measurement(usul_su_t6, usul_al_fiqh_method_flat_control, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(usul_su_t10, usul_al_fiqh_method_flat_control, suppression_requirement, 10, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method_flat_control, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method_flat_control, islamic_legal_school_authority).
narrative_ontology:affects_constraint(usul_al_fiqh_method_flat_control, state_fatwa_legitimacy).
narrative_ontology:affects_constraint(usul_al_fiqh_method_flat_control, heterodox_suppression_mechanism).

% DUAL FORMULATION NOTE:
% Usul al-fiqh is a foundational constraint that affects multiple downstream constraints in Islamic legal systems. The methodology's extractiveness and suppression characteristics propagate through the network: institutional capture of usul affects state fatwa legitimacy; suppression of heterodox methodologies affects legal innovation capacity; beneficiary concentration in established schools affects their institutional authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method_flat_control, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
