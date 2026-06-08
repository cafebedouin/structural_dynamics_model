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
 *   human_readable: Usul al-Fiqh: Jurisprudential Methodology Commitment
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   Usul al-fiqh (jurisprudential methodology) is the shared Islamic
 *   scholarly commitment to deriving specific legal rulings from foundational
 *   sources—Quran, Sunnah, ijma' (consensus), qiyas (analogical
 *   reasoning)—through systematic interpretive principles. This constraint
 *   describes the institutional arrangement that enables Islamic
 *   jurisprudence to function as a coherent, legitimized system rather than
 *   competing interpretations. The constraint exhibits real coordination
 *   function: jurisprudential schools, muftis, and judges benefit from a
 *   shared methodology that prevents legal reasoning from becoming purely
 *   arbitrary. However, the coordination is asymmetrically distributed.
 *   Jurisprudential schools and scholarly consensus benefit from the
 *   constraint's legitimacy without bearing proportional costs; lay Muslims
 *   subject to fiqhi rulings bear costs they cannot exit or contest. The
 *   constraint shows increasing theater over the measured interval:
 *   state-administered Islamic courts cite usul methodology while following
 *   political directives; the performative citation of jurisprudential
 *   reasoning masks instrumental outcomes. The measurement drift
 *   (extractiveness rising from 0.22 to 0.35, theater rising from 0.28 to
 *   0.44) suggests the constraint is accumulating extractive dynamics as
 *   modernization pressures compete with traditional authority structures.
 *
 * KEY AGENTS:
 *   - Jurisprudential Schools (Maliki, Hanafi, Shafi'i, Hanbali, Zaydi, Twelver, etc.): Institutional beneficiaries (institutional/arbitrage) — derive legitimacy and authority from their role in the usul framework; control interpretation of methodology
 *   - Islamic Scholarly Consensus (Ijma'): Institutional actors (institutional/constrained) — elevated as primary source but whose participation and agreement is gatekept through usul definitions; both benefit and are constrained
 *   - Lay Muslims (Religious subjects): Powerless subjects (powerless/trapped) — subject to fiqhi rulings derived through methodology they cannot access, verify, or exit; bear costs of jurisprudential disagreement and evolving interpretations
 *   - Practicing Jurists (Muftis, Islamic judges): Moderate institutional actors (moderate/constrained) — benefit from legitimate framework for issuing rulings but constrained by source hierarchy and methodological requirements
 *   - Modern Reformist Scholars (Maqasid, Islamic law modernizers): Organized reformers (organized/mobile) — seek to innovate methodology while remaining within Islamic jurisprudential tradition; have real but contested capacity to modify framework
 *   - State-Administered Islamic Courts: Institutional actors (institutional/arbitrage) — formally apply usul methodology; actually use it performatively to legitimize state policy; maintain the framework through bureaucratic inertia
 *   - Secular Law Systems (in plural jurisdictions): Alternative institutional actors (institutional/mobile) — competing framework offering exit from usul methodology in some contexts; not fully displaced in Muslim-majority nations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method_flat_control, 0.35).
domain_priors:suppression_score(usul_al_fiqh_method_flat_control, 0.28).
domain_priors:theater_ratio(usul_al_fiqh_method_flat_control, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method_flat_control, extractiveness, 0.35).
narrative_ontology:constraint_metric(usul_al_fiqh_method_flat_control, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(usul_al_fiqh_method_flat_control, theater_ratio, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method_flat_control, rope).
narrative_ontology:human_readable(usul_al_fiqh_method_flat_control, "Usul al-Fiqh: Jurisprudential Methodology Commitment").
narrative_ontology:topic_domain(usul_al_fiqh_method_flat_control, "islamic_jurisprudence/legal_theory/comparative_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method_flat_control, '6e8ab081-7cad-4288-a412-9034ae03997e').
narrative_ontology:cs_kernel_codification('6e8ab081-7cad-4288-a412-9034ae03997e', formalized).
narrative_ontology:cs_authority_grounding('6e8ab081-7cad-4288-a412-9034ae03997e', lineage).
narrative_ontology:cs_interpretation_layer_present('6e8ab081-7cad-4288-a412-9034ae03997e').
narrative_ontology:cs_created_at('6e8ab081-7cad-4288-a412-9034ae03997e', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(usul_al_fiqh_method_flat_control, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, jurisprudential_schools).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, islamic_scholarly_consensus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, practicing_muftis_islamic_judges).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, reformist_modernist_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, state_administered_islamic_courts).
narrative_ontology:constraint_victim(usul_al_fiqh_method_flat_control, lay_muslims_religious_subjects).
narrative_ontology:constraint_victim(usul_al_fiqh_method_flat_control, practicing_muftis_islamic_judges).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method_flat_control, revealed_sources_sufficiency).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method_flat_control, systematic_reason_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classical jurisprudential schools set and maintain the usul framework. They define valid sources, their hierarchy, and interpretive principles. They control which scholars count as authorized reasoners. They benefit from the framework's legitimacy while remaining free to adopt or propose methodological modifications (maqasid, reweighting sources) if they can gain consensus. Their agenda-setting role is structural: the methodology persists because these institutions maintain it.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, maliki_hanafi_shafi_i_hanbali_schools, agenda_setter,
    institutional, generational, arbitrage, global).

% The collective body of recognized Islamic scholars whose agreement constitutes ijma' (consensus—itself a primary source in usul) participate in setting methodological norms and validating rulings. They benefit from elevated status as consensus-makers. However, their participation is constrained: who counts as a recognized scholar, how consensus is determined, whether disagreement breaks consensus—these are all controlled through gates that usul al-fiqh itself specifies. Ijma' is both a source that empowers scholars and a mechanism that usul controls.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, islamic_scholarly_consensus, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method_flat_control, islamic_scholarly_consensus, beneficiary).

% Non-specialist Muslims seeking religious and legal guidance are subject to rulings derived through usul al-fiqh methodology. They cannot directly participate in the methodology's application. They face consequences when different qualified muftis produce contradictory rulings from the same sources, and they cannot arbitrate between them. When state law conflicts with fiqhi rulings, they navigate multiple legal frameworks without choosing which methodology to follow. They bear the costs of jurisprudential complexity and disagreement with no recourse.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, lay_muslims_religious_subjects, payer,
    powerless, biographical, trapped, local).

% Muftis and Islamic judges derive professional legitimacy and authority from usul al-fiqh—the methodology enables them to issue binding or advisory fatwas and judicial rulings. They benefit from the framework's legitimacy. They also bear constraint: they must justify rulings through the source hierarchy and cannot override Quranic or canonical Sunnah citations with personal judgment. They exercise ijtihad (independent reasoning) but only within gates the methodology defines. Career risk and delegitimization follow if they deviate too far from recognized methodology.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, practicing_muftis_islamic_judges, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method_flat_control, practicing_muftis_islamic_judges, payer).

% Modern Islamic law scholars pursuing maqasid al-Shariah, dharura (necessity), and other methodological innovations benefit from the usul framework as a vehicle for systematic reform. They can propose reweighting the source hierarchy, introducing new principles, or reinterpreting existing sources—and these proposals gain legitimacy if framed as developments within Islamic jurisprudential tradition rather than departures from it. They have genuine capacity to shape the methodology's evolution, though acceptance depends on building consensus among traditional scholars.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, reformist_modernist_jurists, beneficiary,
    organized, generational, mobile, global).

% National and regional Islamic courts formally apply usul al-fiqh methodology in public rulings and judicial opinions. They benefit from the methodology's legitimacy: citing fiqhi sources and jurisprudential reasoning confers Islamic authority on state rulings. They maintain the apparatus through institutional inertia. However, their actual reasoning often follows political directives, state policy, or administrative convenience, with usul methodology cited performatively to legitimize non-jurisprudential outcomes. The framework persists in state systems because it serves state legitimacy, not because it constrains state authority.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, state_administered_islamic_courts, agenda_setter,
    institutional, immediate, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method_flat_control, state_administered_islamic_courts, beneficiary).

% Civil law and common law systems operate in parallel to usul al-fiqh in many Muslim-majority and Muslim-minority jurisdictions. They represent an institutional alternative that some Muslims access or are subject to. The relationship between secular law and Islamic jurisprudence varies: compartmentalization (different domains), supplementation (secular law for civil/commercial matters, fiqh for family/religious), and in some cases competition for authority over the same questions. Secular systems are listed as non-agent because they do not directly participate in the usul al-fiqh constraint; they are structural alternatives.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, secular_legal_systems, observer,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_non_agent(usul_al_fiqh_method_flat_control, secular_legal_systems).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Usul al-fiqh solves the genuine collective-action problem of deriving specific legal guidance from general, revealed texts in a way that multiple independent reasoners can perform, with legitimate plurality of outcomes (different schools reaching different conclusions from the same methodology). Without systematic principles for source hierarchy and interpretive logic, Islamic law would fragment into arbitrary individual claims unanchored in any shared framework. Usul al-fiqh enables jurisprudence: systematic, legitimized, teachable derivation of law from texts.
% TRANSFER_FUNCTION: The constraint transfers authority and interpretive control from the community of all Muslims to the credentialed scholarly classes (jurisprudential schools, recognized muftis, Islamic judges). It transfers legitimacy from individual opinion to methodologically-grounded reasoning. It transfers the power to define what counts as valid Islamic legal guidance from democratic or egalitarian processes to gates controlled by traditional schools and scholars. In exchange, it transfers certainty and coherence to legal outcomes: Muslims can appeal to recognized sources and principles rather than competing claims of personal revelation or arbitrary preference.
% ABSENT_VOICES: Lay Muslims subject to fiqhi rulings are absent from the process that derives those rulings. Women historically were absent from jurisprudential deliberation despite being subjects of family law rulings. Minority legal schools (Zaydi, Twelver, Ismaili traditions) have varying degrees of formal inclusion. Sufi and folk Islamic practitioners using different reasoning methods are marginal to formal usul al-fiqh structures. Muslim-majority states have inserted themselves into control of Islamic courts, raising questions about whether state administrators' interests (political legitimacy, policy objectives) should have standing in jurisprudential reasoning. These absent voices reappear when their rulings are applied to them, but they do not participate in determining the methodology that produces those rulings.
% DISAPPEARANCE_RATIONALE: If usul al-fiqh disappeared, Islamic law would not disappear, but its form would fragment. The world would rearrange at the jurisprudential level: Muslims would lose a shared framework for deriving law from texts; different communities would adopt different methodologies (Quranic literalism, Salafi rejectionism, secular law, customary law, individualized fatwas without systematic grounding). Some scholars argue this fragmentation has already occurred and usul al-fiqh maintains the fiction of unity more than the reality. Others contend that even with plurality, usul al-fiqh provides enough common ground that Islamic law remains a coherent tradition rather than competing interpretations. The contestation is real: traditionalists see world rearrangement if the framework is lost; reformists see necessary rearrangement if innovation is permitted within it; modernists see the framework as already lost, replaced by state law and individual choice.
% FOUNDING_PROBLEM: Islamic jurisprudence emerged historically to solve the problem of how a community of believers continuing after the Prophet Muhammad's death could derive binding legal guidance from revealed texts (Quran) and the Prophet's example (Sunnah) without the Prophet's interpretive authority. Early Muslims faced questions the Quran and Sunnah did not directly address. Usul al-fiqh systematized the derivation process: establishing principles for source hierarchy (Quran supreme, Sunnah authoritative, consensus of scholars, analogical reasoning from established rulings), methods for resolving apparent contradictions in texts, and principles for deriving general principles from specific rulings and vice versa. The founding problem remains live: Islamic law still requires principles for moving from text to application.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Islamic jurisprudence, modernist Islamic legal scholarship, and state Islamic court systems all attest that the need for systematic derivation of law from foundational sources persists. The corroboration is strongest outside the beneficiary set: secular legal theorists studying Islamic law recognize that some systematic methodology (whether usul al-fiqh or an alternative) is necessary for any legal system based on textual foundation. Muslim reformers introducing maqasid and other innovations explicitly state they are solving the problem of how to apply Quranic principles to novel situations. The problem's liveness is also attested by its contestation: the fact that different schools, reformers, and modernists dispute how the methodology should evolve or be reweighted proves the problem remains active, not historical.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method_flat_control, contested).
narrative_ontology:founding_problem_status(usul_al_fiqh_method_flat_control, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JURISPRUDENTIAL SCHOOL (ROPE) — Institutional actors (Maliki, Hanafi, Shafi'i, Hanbali schools and modern reformist methodologies) experience usul al-fiqh as a coordination mechanism that enables systematic derivation while preserving school-level interpretive authority. Benefits from the legitimacy framework without bearing significant extraction costs. Arbitrage-grade exit: schools can adopt alternative methodologies or reweight the hierarchy of sources, but do so within the usul framework itself. Net beneficiary position.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIVIDUAL JURIST (TANGLED ROPE) — Practicing muftis and Islamic judges face both coordination benefits and extraction costs. They benefit from the shared methodology as it provides legitimate basis for issuing fatwa and judicial rulings; they bear the cost of constraint within a strict interpretive hierarchy where certain sources (Quran, Sunnah) are treated as immovable. Constrained exit: can exercise ijtihad (independent reasoning) but only within the usul framework; deviation risks delegitimization. Experience real coordination function alongside meaningful constraint.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LAY MUSLIM SUBJECT (SNARE) — Non-specialist Muslims seeking guidance on Islamic law are subject to the outcomes of usul al-fiqh methodology without direct participation in its application or contestation. They cannot exit their need for reliable legal guidance within Islamic frameworks; cannot escape the consequences of jurisprudential disagreement (different muftis producing incompatible rulings from the same methodology); cannot verify the reasoning. Trapped: the constraint produces the authoritative rulings that govern religious and civic life with no exit option for dissenters. Maximum experienced extraction from powerless position.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: ISLAMIC SCHOLARLY CONSENSUS (TANGLED ROPE) — The collective body of recognized scholars whose agreement (ijma') legitimizes rulings experiences the usul framework as both constitutive and constraining. Benefits: ijma' is elevated as a primary source in the hierarchy, giving consensus real authority. Bears: the determination of who counts as a recognized scholar, how consensus is established, and what breaks consensus is controlled through interpretive gates that usul al-fiqh administers. Constrained exit: consensus can shift but only through processes the usul framework defines. Mixed coordination (enabling legitimate collective judgment) and extraction (controlling who participates, how decisions are recorded).
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ISLAMIC LEGAL MODERNIZATION (ROPE) — Organized reformist actors (modern Islamic law institutions, legal scholars pursuing maqasid al-Shariah, comparative law scholars) see usul al-fiqh as a coordination framework enabling systematic innovation without theological rupture. Can modify the framework itself (shift source hierarchy, introduce new reasoning principles like maqasid) while remaining within Islamic jurisprudential legitimacy. Mobile exit: can propose alternative methodologies that reweight sources or introduce new principles; acceptance depends on persuasiveness and scholarly consensus, not coercion. Net beneficiary: the framework enables their reform agenda by providing systematic pathways.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: STATE-ADMINISTERED COURT SYSTEM (PITON) — National and regional Islamic court systems that formally apply usul al-fiqh methodology experience the constraint as increasingly performative. Theaters: courts publish fiqhi reasoning chains and cite usul sources; actual rulings often follow state policy, political pressure, or bureaucratic convenience masked within usul language. The usul framework persists because state legitimacy in Muslim-majority societies depends on Islamic legal credentials; the framework's actual function (constraining judicial discretion via source hierarchy) has atrophied. Maintained through institutional inertia rather than functional necessity. Theater ratio high: the performative citation of methodology masks instrumental use.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LOGICAL SUFFICIENCY (MOUNTAIN) — From a civilizational/analytical perspective, usul al-fiqh can appear as a logical necessity: systematic jurisprudence requires some hierarchy of interpretive sources; revelation-based law requires principles for deriving specific rulings from general texts; derivation of particular from universal is a structural feature of all normative systems. This perspective risks naturalizing what is actually a contested institutional commitment. However, the structural data contradicts the mountain classification: the beneficiaries, the constraints on lay Muslims, the state performative use, and the ongoing methodological contestation all point to a contingent arrangement, not a logical law.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: COMPARATIVE LEGAL ANALYST (ROPE) — Academic observers analyzing usul al-fiqh as a comparative jurisprudential system (alongside civil law, common law, statutory systems) classify it as a genuine coordination mechanism: it solves the collective-action problem of deriving law from texts in ways different traditions do (statutory interpretation, constitutional hermeneutics, precedent systems). Extraction is low because the framework's legitimacy derives from genuine scholarly agreement on its necessity. This perspective avoids the mountain trap: acknowledges contingency (Islamic legal tradition chose this methodology; alternatives exist in other traditions) while recognizing real coordination function.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.35): Moderate, with upward drift. The constraint generates real coordination benefits (systematic jurisprudence, prevention of arbitrary law-making, legitimate plurality of schools) that justify some overhead. However, the distribution is asymmetric: jurisprudential schools and consensual scholars benefit without proportional cost, while lay Muslims bear costs they cannot contest. The upward drift reflects accumulating extraction: modernization creates pressure to reweight sources and introduce new principles (maqasid, dharura), but the gates for doing so remain controlled by traditional schools, creating a tension between claimed flexibility and actual rigidity. Suppression (0.28): Moderate, stable. Real barriers exist to exit: lay Muslims cannot easily adopt competing methodologies without social/legal consequences; practicing jurists cannot deviate from usul hierarchy without delegitimization; reformers must frame innovations as consistent with traditional methodology rather than departures from it. However, suppression is not maximal because scholarly debate, school plurality, and some methodological evolution are permitted. Theater ratio (0.44): Moderate-high, with sharp upward drift (0.28 → 0.44 over the interval). The performative use is concentrated in state-administered systems, where courts cite usul reasoning while following political or administrative directives. In pure scholarly jurisprudence (traditional schools, academic Islamic law), theater is lower; the reasoning chains are genuine. The upward drift reflects state courts' increasing reliance on performative citation as a legitimacy mechanism for non-fiqhi outcomes. The combined measurement profile (moderate extractiveness with drift, moderate suppression, rising theater) points to a constraint that began as pure coordination but is accumulating extraction and performativity as institutional pressures (state authority, modernization demands) compete with traditional scholarly authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival disagreement across power levels and exit options. Jurisprudential schools (institutional/arbitrage) see rope: a coordination mechanism enabling legitimate interpretive authority. Practicing jurists (moderate/constrained) see tangled rope: benefits from the framework alongside real constraint in the source hierarchy. Lay Muslims (powerless/trapped) see snare: they bear the costs of jurisprudential disagreement and cannot exit or contest the rulings applied to them. Modern reformers (organized/mobile) see rope: the methodology enables systematic innovation within Islamic legitimacy. State courts (institutional/arbitrage) see piton: the methodology is maintained performatively while actual reasoning follows administrative directives. The analytical observer risks mountain classification (logical necessity of systematic jurisprudence) but the structural data contradicts this: the beneficiaries, the constraints on powerless agents, the state performativity, and the controlled gates for innovation all reveal a contingent institutional arrangement, not a logical law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from beneficiary/victim declarations and exit options. Jurisprudential schools (beneficiaries, institutional power, arbitrage exit) experience low directionality (d ≈ 0.15–0.25) because they benefit and can pivot. Lay Muslims (victims, powerless, trapped exit) experience high directionality (d ≈ 0.85–0.95) because they bear costs and cannot escape. Practicing jurists (mixed: benefit from legitimacy, constrained by source hierarchy; moderate power, constrained exit) experience middle directionality (d ≈ 0.50–0.65). The effective extractiveness (chi) is then modulated by the agent's structural position: powerless agents experience higher chi from the same base extractiveness than institutional agents do. The upward measurement drift in chi values reflects not just increasing base extractiveness but also increasing concentration of extraction on powerless agents—the constraint becomes more asymmetric over time as state courts and institutional pressures amplify the gap between the schools/scholars (who navigate the framework) and lay Muslims (who are subject to its outcomes).
 *
 * MANDATROPHY ANALYSIS:
 *   Usul al-fiqh does not exhibit classic mandatrophy (mandate outliving function). The founding problem—how to derive law from texts in a revealed religion—remains live. However, the constraint does show tension between its stated function (systematic jurisprudence enabling legitimate plurality) and its actual operation (gatekeeping control by traditional schools over who counts as a valid reasoner and how the hierarchy of sources is weighted). The theater drift (0.28 → 0.44) suggests a secondary mandatrophy emerging at the state level: state-administered Islamic courts maintain the usul apparatus to legitimize state authority while using it performatively (the mandate of 'deriving law from foundational sources' is honored in citation form but not determinative reasoning). This is not classic mandatrophy but a version where the institutional purpose (legitimize state authority) has diverged from the stated function (enable systematic jurisprudence). The reformist perspective (organized/mobile) sees the constraint differently: as an evolving methodology enabling innovation rather than a degraded ritual. The perspectival gap reveals that mandatrophy is contextual—live at the scholarly level, emergent at the state institutional level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    source_hierarchy_naturality,
    'Is the ranking of sources (Quran > Sunnah > ijma'' > qiyas > istislah > etc.) a logical necessity or a chosen institutional convention?',
    'Historical analysis of source-ranking in competing Islamic jurisprudential schools; comparison with how other legal traditions (Hindu dharmaśāstra, Talmudic jurisprudence, constitutional law) rank analogous foundational sources; examination of whether alternative hierarchies produce logically incoherent results or merely different outcomes',
    'If logical necessity (universal constraint on any revelation-based system): mountain classification gains support. If institutional convention: the rope/snare classifications are revealed as capturing real extractive dynamics that a natural-law framing obscures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_hierarchy_naturality, conceptual, 'Whether source hierarchy is logically necessary or institutionally chosen').

omega_variable(
    lay_muslim_exit_capacity,
    'What are the real alternatives available to a Muslim subject who rejects a ruling derived through usul al-fiqh methodology? Exit or voice?',
    'Ethnographic study of non-compliance patterns; legal and social consequences of seeking alternative rulings (from different schools, from non-credentialed scholars, or from secular law); measurement of actual switching costs; documentation of communities that have adopted alternative methodologies (Quranic literalism, Salafi rejectionism, secular law) and what enabled that exit',
    'If alternatives exist with reasonable switching costs: snare classification softens to tangled_rope or constrained exit. If alternatives are effectively foreclosed by social or legal penalties: snare classification is confirmed and suppression values require upward revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lay_muslim_exit_capacity, empirical, 'Real alternatives and switching costs for lay Muslims rejecting usul-derived rulings').

omega_variable(
    ijma_determination_control,
    'Who determines what counts as ijma'' (scholarly consensus)? Is the determination process controlled through usul al-fiqh itself, creating a self-referential legitimacy loop?',
    'Documentary analysis of how contemporary Islamic jurisprudential institutions identify and validate ijma''; comparison of which scholars count as participants in consensus; identification of gatekeeping mechanisms in major Islamic scholarly bodies; examination of cases where ijma'' was claimed but contested',
    'If determination is self-referential (usul al-fiqh controls definition of valid scholarly participation, whose agreement counts, how consensus is recorded): the constraint exhibits recursive extraction (the beneficiaries control the mechanism that legitimizes their benefit). If determination is open or contestable: extraction is lower, mechanism more genuinely coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_determination_control, empirical, 'Control mechanisms for determining and validating scholarly consensus').

omega_variable(
    methodological_innovation_gates,
    'What is the effective barrier to introducing new interpretive principles (maqasid, dharura, maslaha) or reweighting the source hierarchy in contemporary Islamic jurisprudence?',
    'Historical documentation of successful methodological innovations (maqasid al-Shariah adoption, acceptance of istislah in some schools); identification of rejected alternatives and why they failed; analysis of what legitimacy conditions allow innovation within usul framework vs. what conditions would force external challenge to the framework itself',
    'If barriers are high and innovation is effectively gatekept by traditional schools: the constraint serves extraction (maintaining traditional school authority). If barriers are low and innovation flows naturally: the constraint is genuinely coordinative. If barriers are shifting over time (high → low): measurement drift should show decreasing extractiveness and theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_innovation_gates, empirical, 'Barriers to methodological innovation within usul al-fiqh').

omega_variable(
    lay_muslim_plural_guidance,
    'When multiple qualified muftis applying the same usul methodology produce contradictory rulings, does the lay Muslim experience this as coordinated legal pluralism (rope outcome) or as an unresolved extraction problem (snare outcome)?',
    'Survey of Muslim communities with documented jurisprudential disagreement (inheritance law, finance, family law); ethnographic documentation of how Muslims choose between competing rulings; measurement of confidence in the methodology when it produces conflicting guidance; analysis of whether the methodology provides principles for resolving disagreement or merely legitimizing both sides',
    'If experienced as legitimate pluralism: rope classification for lay perspective is confirmed. If experienced as guidance failure: snare classification is supported; suppression may be higher (confusion increases trappedness).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lay_muslim_plural_guidance, empirical, 'How lay Muslims experience jurisprudential disagreement within usul framework').

omega_variable(
    state_performative_use_measurement,
    'In state-administered Islamic court systems, what proportion of rulings visibly deviate from what a strict usul al-fiqh application would produce? What is the ratio of performative citation to determinative reasoning?',
    'Comparative legal analysis of published court opinions; identification of cases where political pressure, state policy, or administrative convenience appears to have overridden fiqhi logic; measurement of citation patterns (frequency of source citation vs. frequency of actual source-based reasoning); expert coding of judicial reasoning chains',
    'High deviation ratio (>0.4): piton classification is strongly supported; theater_ratio may be understated. Low deviation ratio (<0.2): the constraint''s coordination function is preserved in state systems despite institutional complexity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_performative_use_measurement, empirical, 'Proportion of state court rulings deviating from usul al-fiqh application').

omega_variable(
    secular_law_alternative_adoption,
    'In jurisdictions where Muslims have adopted secular legal frameworks (civil law, common law) instead of Islamic jurisprudence, what percentage of the Muslim population experiences this as a genuine alternative exit vs. a forced replacement?',
    'Survey and ethnographic research in plural legal systems (Malaysia, Egypt, Lebanon, diaspora communities); documentation of how Muslims navigate between legal frameworks; measurement of whether secular law adoption is experienced as choice or coercion; analysis of whether usul al-fiqh methodology survives in parallel (religious law alongside civil law) or is genuinely displaced',
    'If experienced as genuine exit: trapped exit classification for lay Muslims is too severe; constrained may be more accurate. If experienced as coerced replacement: both the trap and the supplementary role of usul al-fiqh (parallel to secular law) reinforce the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_law_alternative_adoption, empirical, 'Experience of secular law adoption as alternative or coerced replacement').

omega_variable(
    traditional_school_innovation_capacity,
    'Are the classical jurisprudential schools (Maliki, Hanafi, etc.) genuinely open to adopting maqasid-based innovations, or does school loyalty function as an identity lock that prevents departure from established doctrine?',
    'Genealogical analysis of school membership and switching; documentation of scholars who departed from school doctrine and the social/institutional consequences; analysis of whether maqasid innovations are adopted as evolution of existing schools or as external challenges to school authority; measurement of identity fusion (school affiliation as core to scholar identity vs. instrumental methodological choice)',
    'If school loyalty is identity-locked: jurisprudential schools are constrained not by usul al-fiqh itself but by identity dynamics; the constraint''s extraction is amplified by cognitive capture. If schools are methodologically open: the constraint functions as pure coordination with low extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditional_school_innovation_capacity, conceptual, 'Whether school membership functions as identity lock vs. methodological choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method_flat_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method_flat_control, theater_ratio, 0, 0.28).
narrative_ontology:measurement(usul_tr_t3, usul_al_fiqh_method_flat_control, theater_ratio, 3, 0.35).
narrative_ontology:measurement(usul_tr_t6, usul_al_fiqh_method_flat_control, theater_ratio, 6, 0.4).
narrative_ontology:measurement(usul_tr_t9, usul_al_fiqh_method_flat_control, theater_ratio, 9, 0.44).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method_flat_control, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(usul_be_t3, usul_al_fiqh_method_flat_control, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(usul_be_t6, usul_al_fiqh_method_flat_control, base_extractiveness, 6, 0.32).
narrative_ontology:measurement(usul_be_t9, usul_al_fiqh_method_flat_control, base_extractiveness, 9, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method_flat_control, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(usul_su_t3, usul_al_fiqh_method_flat_control, suppression_requirement, 3, 0.24).
narrative_ontology:measurement(usul_su_t6, usul_al_fiqh_method_flat_control, suppression_requirement, 6, 0.26).
narrative_ontology:measurement(usul_su_t9, usul_al_fiqh_method_flat_control, suppression_requirement, 9, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method_flat_control, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method_flat_control, islamic_legal_school_authority).
narrative_ontology:affects_constraint(usul_al_fiqh_method_flat_control, fatwa_binding_force).
narrative_ontology:affects_constraint(usul_al_fiqh_method_flat_control, quranic_interpretation_hierarchy).
narrative_ontology:affects_constraint(usul_al_fiqh_method_flat_control, ijma_consensus_determination).

% DUAL FORMULATION NOTE:
% Usul al-fiqh is an overarching methodological constraint that structures the operation of specific jurisprudential claims (particular fiqh questions: inheritance law, finance, family law). This story describes the methodology itself; downstream constraints describe specific domains where the methodology produces extractive or coordinative outcomes. Upstream: Quranic interpretation hierarchy (how particular surahs are ranked or harmonized) feeds into usul source hierarchy. Downstream: Islamic legal school authority (which schools have legitimate standing to interpret), fatwa binding force (whether fatwas are binding or advisory), ijma' consensus determination (how consensus is identified and validated) all depend on usul al-fiqh's framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
