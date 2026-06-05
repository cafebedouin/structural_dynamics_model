% ============================================================================
% CONSTRAINT STORY: opacity_resistance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_opacity_resistance, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: opacity_resistance
 *   human_readable: Opacity Resistance: Structural Asymmetry Between Knowledge Production and Knowledge Access
 *   domain: epistemology/information_systems
 *
 * SUMMARY:
 *   Opacity resistance describes the structural constraint that emerges when
 *   knowledge production and knowledge access are decoupled through
 *   deliberate gatekeeping mechanisms. The constraint creates asymmetric
 *   value capture: those who control knowledge access (publishers,
 *   credentialers, proprietary research firms) extract rents from those who
 *   need knowledge but cannot produce it independently. This is not inherent
 *   to knowledge production — it is a contingent institutional arrangement
 *   defended through control of distribution channels, credentialing
 *   monopolies, intellectual property law, and the naturalization narrative
 *   that 'knowledge quality requires gatekeeping.' The constraint exhibits
 *   all six classification types depending on perspective, making it
 *   diagnostic for how contingent institutional asymmetries get naturalized
 *   as laws. The theater_ratio (0.68) reflects that much contemporary
 *   gatekeeping activity (academic journal peer review, professional
 *   credential verification, publishing infrastructure) is performative
 *   ritual rather than functional necessity — the open-access revolution has
 *   demonstrated that knowledge quality assurance is achievable through
 *   transparent mechanisms. Yet the constraint persists through institutional
 *   inertia, cognitive capture of gatekeepers, and structural incentives that
 *   reward opacity maintenance.
 *
 * KEY AGENTS:
 *   - Knowledge Seekers: Primary victims (powerless/trapped) — need information but cannot produce independently; face cost barriers to access; cannot exit without abandoning knowledge pursuit
 *   - Institutional Knowledge Workers: Secondary victims (moderate/constrained) — employed within credential systems; experience mixed coordination and extraction; career advancement requires participation in opacity-maintaining structures
 *   - Opacity Maintainers: Primary beneficiaries (institutional/arbitrage) — control access channels and distribution mechanisms; extract rents through paywalls, credential monopolies, IP restrictions; can exit but do not because opacity sustains business models
 *   - Open Knowledge Coalition: Organized countervailing agent (organized/mobile) — building transparent alternatives (arXiv, Wikipedia, open-source communities); reducing opacity's extractive power through decentralized verification systems
 *   - Credentialing Institutions: Institutional beneficiary (institutional/arbitrage) — maintain opacity through credential requirements; derive market value from artificial scarcity; perform gatekeeping function through inertia rather than necessity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing opacity as inherent to knowledge production; structural analysis reveals it as contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(opacity_resistance, 0.58).
domain_priors:suppression_score(opacity_resistance, 0.62).
domain_priors:theater_ratio(opacity_resistance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(opacity_resistance, extractiveness, 0.58).
narrative_ontology:constraint_metric(opacity_resistance, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(opacity_resistance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(opacity_resistance, tangled_rope).
narrative_ontology:human_readable(opacity_resistance, "Opacity Resistance: Structural Asymmetry Between Knowledge Production and Knowledge Access").
narrative_ontology:topic_domain(opacity_resistance, "epistemology/information_systems").

domain_priors:requires_active_enforcement(opacity_resistance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(opacity_resistance, opacity_maintainers).
narrative_ontology:constraint_beneficiary(opacity_resistance, expert_gatekeepers).
narrative_ontology:constraint_victim(opacity_resistance, knowledge_seekers).
narrative_ontology:constraint_victim(opacity_resistance, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KNOWLEDGE SEEKER (SNARE) — Structurally trapped by information asymmetries with no meaningful exit. Cannot produce knowledge independently at scale; cannot pay to access it without exploitation; cannot avoid needing it. Bears full extraction cost while beneficiary controls the production and access mechanisms. Maximum experienced extraction.
constraint_indexing:constraint_classification(opacity_resistance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL KNOWLEDGE WORKER (TANGLED ROPE) — Constrained by employment dependency and credentialing systems but genuinely coordinated through disciplinary knowledge sharing. Experiences both extraction (credential rent-seeking, intellectual property lock-in) and coordination benefit (access to shared methodologies, peer review, professional networks). Career advancement requires participation in opacity-maintaining institutions.
constraint_indexing:constraint_classification(opacity_resistance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPACITY MAINTAINER (ROPE) — Institutional beneficiary (academic publishers, paywalled platforms, proprietary research firms) experiences the constraint primarily as coordination: opacity creates a legitimate problem they solve by gatekeeping access and controlling distribution. Net beneficiary with full arbitrage options. Can exit opacity arrangements and remain viable; chooses not to because opacity sustains their business model.
constraint_indexing:constraint_classification(opacity_resistance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN KNOWLEDGE COALITION (SCAFFOLD) — Organized agents (open-access movements, Wikipedia, arXiv, open-source communities) see opacity as a temporary institutional arrangement with an exit path. Building parallel knowledge production and distribution systems (preprints, wiki-based documentation, open-source communities) that reduce opacity without requiring permission from legacy gatekeepers. Sunset logic: as transparent alternatives mature, opacity's extractive value declines. Theater is declining as transparency norms spread.
constraint_indexing:constraint_classification(opacity_resistance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDENTIALING INSTITUTION (PITON) — Universities and professional licensing bodies maintain opacity through credential requirements despite reduced functional necessity. Knowledge is increasingly accessible through transparent channels, yet institutional credentialing remains gatekeeping mechanism primarily through inertia. Theater ratio high: much credentialing activity (accreditation, degree granting) is performative ritual rather than functional verification of competence. Can arbitrage out of opacity maintenance but does not, because credentials derive market value from scarcity.
constraint_indexing:constraint_classification(opacity_resistance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED OPACITY VIEW (MOUNTAIN) — From a universal perspective, some degree of opacity appears immutable: knowledge production requires specialization, specialization requires access barriers, access barriers create asymmetries. This perspective risks naturalizing contingent institutional arrangements (paywalls, credentialism, publication gatekeeping) as inherent to knowledge production itself. The mountain classification is a false summit — the analysis will reveal that most measured opacity is institutional rather than structural.
constraint_indexing:constraint_classification(opacity_resistance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(opacity_resistance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(opacity_resistance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(opacity_resistance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(opacity_resistance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(opacity_resistance, TR),
    TR >= 0.70.

:- end_tests(opacity_resistance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Opacity creates substantial value asymmetry — knowledge workers cannot access cutting-edge research without paying gatekeepers or risking legal sanctions; knowledge seekers cannot participate in knowledge production without credentials controlled by monopolies. The extraction is not total because transparent alternatives are emerging and reducing opacity's monopoly power. Suppression (0.62): High. Multiple reinforcing mechanisms prevent exit: credential monopolies that control professional access, legal IP frameworks (copyright, patent) that block sharing, technological paywalls, publishing concentrations, and the normalization narrative that opacity is necessary. These barriers are collectively substantial but not absolute — some agents can and do circumvent them through open-access and open-source communities. Theater ratio (0.68): High. Academic peer review, professional credentialing, and publishing quality assurance are increasingly performative rather than functionally necessary. Open-access publications demonstrate equivalent quality control through transparent mechanisms. Yet the theatrical components (rejection rates, peer review turnaround time, journal impact factors) persist as status signals and market mechanisms rather than quality necessities. Theater has increased over the interval as gatekeepers have relied more heavily on performative legitimacy as transparency alternatives mature.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence. The powerless knowledge seeker sees a snare (pure extraction, no coordination value, trapped). The institutional opacity maintainer sees rope (coordination problem solved by gatekeeping, benefits from arrangement). The open knowledge coalition sees a scaffold (temporary institutional arrangement being superseded by transparent alternatives). The credentialing institution sees its own function as piton (maintained through inertia, increasingly performative). The analytical observer at civilizational scale risks seeing mountain (opacity inherent to knowledge production), but structural analysis reveals this as false summit — the measured opacity is contingent institutional arrangement, not natural law. The gap between mountain and snare perspectives is diagnostic: it reveals where naturalization is occurring. The gap between beneficiary rope and victim snare reveals asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position relative to the opacity constraint. Knowledge seekers have no exit options (trapped) and are targets of extraction, producing high d (0.95) and high experienced extraction chi. Opacity maintainers have full exit options (arbitrage) and are beneficiaries, producing low d (0.10) and negative/low chi. Institutional knowledge workers are constrained (high exit cost but not zero) and are secondary victims, producing moderate-high d (0.70). The open knowledge coalition has mobile exit options and is organized, producing moderate d (0.50). Credentialing institutions have arbitrage options despite inertia, producing low d (0.15). The analytical observer uses canonical d for analytical power (0.72). The beneficiary/victim declarations feed these derivations: opacity_maintainers and expert_gatekeepers are marked as beneficiaries; knowledge_seekers and epistemic_commons are marked as victims. The engine computes d automatically from these declarations and exit_options, producing chi values that differentiate perspectives structurally.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy (the tension between coordination and extraction narratives) by showing that opacity functions as BOTH simultaneously depending on structural position. For the beneficiary, opacity IS coordination — it solves the problem of knowledge distribution and quality assurance. For the victim, opacity IS extraction — it creates barriers to access that concentrate value. The tangled_rope classification captures this duality: genuine coordination mechanism (quality assurance, organized knowledge distribution) coupled with asymmetric extraction (gatekeeping rents, credential monopolies, access barriers). The scaffold perspective is crucial: open-science alternatives demonstrate that the coordination function (quality assurance, knowledge distribution) can be achieved with substantially lower extraction. This means opacity's 'coordination value' is partly real (some quality assurance is necessary) and partly Theater (performative ritual that persists through inertia). As transparent alternatives mature, the coordination function becomes available without extraction, making the remaining opacity purely extractive — snare-like. The mandatrophy resolves: the constraint is currently tangled_rope (mixed genuine coordination with extraction), but it is evolving toward snare (extraction without coordination value) as open alternatives mature, IF AND ONLY IF the open alternatives can solve the coordination problem. If open alternatives fail to match paywalled systems on quality/accessibility tradeoffs, opacity remains tangled_rope. The resolution depends on whether the coordination function is genuine or performative — an empirical question, not a definitional one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opacity_necessity_threshold,
    'How much opacity is genuinely necessary for knowledge quality assurance versus how much is institutional rent-seeking?',
    'Comparative analysis of open-access vs paywalled knowledge base quality metrics; error rates in open-source vs proprietary software; Wikipedia accuracy studies vs paywalled encyclopedias.',
    'If necessity_threshold < 0.20: most measured opacity is extraction. If threshold > 0.60: opacity is legitimate coordination cost. Resolves whether constraint is snare (primarily extraction) or tangled_rope (mixed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_necessity_threshold, empirical, 'Threshold for distinguishing necessary quality assurance from extractive gatekeeping').

omega_variable(
    alternative_verification_sufficiency,
    'Do transparent peer review systems (open journals, preprint servers, decentralized review) provide equivalent knowledge verification as traditional paywalled gatekeeping?',
    'Comparative citation analysis, error correction rates, institutional adoption rates of transparent alternatives, community trust metrics in open vs paywalled systems.',
    'If sufficiency > 0.80: transparency is viable alternative with sunset logic for traditional opacity. If sufficiency < 0.40: paywalled gatekeeping provides unique value, reducing snare classification to tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_verification_sufficiency, empirical, 'Whether transparent verification systems provide adequate knowledge quality assurance').

omega_variable(
    cognitive_capture_of_opacity_maintainers,
    'To what extent do opacity maintainers (publishers, credentialers) believe their own framing that opacity is necessary rather than recognizing it as extractive business model?',
    'Discourse analysis of internal institutional communications, executive strategy documents, historical shift toward open-access among major players, interviews with institutional decision-makers.',
    'If capture > 0.70: maintainers are genuinely identity-locked into opacity narrative. If capture < 0.40: maintainers are consciously choosing extraction. Affects whether constraint is naturalized false summit or recognized asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_capture_of_opacity_maintainers, conceptual, 'Cognitive capture of opacity maintainers by their own naturalizing narratives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(opacity_resistance, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(opr_tr_t0, opacity_resistance, theater_ratio, 0, 0.55).
narrative_ontology:measurement(opr_tr_t2, opacity_resistance, theater_ratio, 2, 0.6).
narrative_ontology:measurement(opr_tr_t5, opacity_resistance, theater_ratio, 5, 0.68).

% Extraction over time
narrative_ontology:measurement(opr_be_t0, opacity_resistance, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(opr_be_t2, opacity_resistance, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(opr_be_t5, opacity_resistance, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(opacity_resistance, information_standard).
narrative_ontology:affects_constraint(opacity_resistance, credentialing_monopoly).
narrative_ontology:affects_constraint(opacity_resistance, intellectual_property_enclosure).
narrative_ontology:affects_constraint(opacity_resistance, platform_algorithmic_gatekeeping).

% DUAL FORMULATION NOTE:
% Opacity resistance is a higher-level constraint describing the structural dynamic that manifests in specific domains (academic publishing, professional licensing, proprietary research, platform algorithms). The upstream constraints are domain-specific implementations (e.g., journal paywalls, credential requirements); opacity_resistance describes their common structural logic. Decomposition per ε-invariance: paywalled journal publishing has ε ≈ 0.52 (coordination + extraction); credential monopolies have ε ≈ 0.48 (professional coordination + access extraction); algorithmic gatekeeping has ε ≈ 0.61 (platform coordination + visibility extraction). These are separate stories with distinct beneficiaries and institutional dynamics, linked by their common structural signature (opacity as extraction mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(opacity_resistance, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
