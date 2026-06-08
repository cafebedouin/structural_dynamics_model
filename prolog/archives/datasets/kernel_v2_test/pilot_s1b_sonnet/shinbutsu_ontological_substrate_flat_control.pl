% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate_flat_control, []).

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
 *   constraint_id: shinbutsu_ontological_substrate_flat_control
 *   human_readable: Shinbutsu-shugo as Shared Ontological Substrate
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   Shinbutsu-shugo (kami-buddha amalgamation) as a shared ontological
 *   substrate claims that kami and buddhas constitute a single religious
 *   reality requiring simultaneous practice. This substrate claim underpinned
 *   Japanese religious life from Buddhism's arrival (6th-8th century) through
 *   the Tokugawa period (1603-1868), enabling practitioners to engage both
 *   traditions without experiencing ontological contradiction. The constraint
 *   coordinated institutional arrangements (temple-shrine complexes, shared
 *   ritual calendars, combined specialist roles) and practitioner behavior
 *   (lifecycle rituals distributed across both traditions). The Meiji
 *   government's 1868 shinbutsu bunri (separation edict) forcibly dissolved
 *   these arrangements, destroying thousands of syncretic sites and forcing
 *   institutional separation. The substrate's extractiveness shows a dramatic
 *   time-indexed spike during the Meiji separation — what had been
 *   low-extraction coordination for over a millennium became high-extraction
 *   disruption when the state weaponized its dissolution. This flat
 *   construction treats the substrate as a single constraint; the contested
 *   question of whether alternative framings (honji-suijaku hierarchical
 *   metaphysics, ryobu-shinto esoteric unification doctrines) represent
 *   distinct constraints or readings of the same substrate is routed to omega
 *   variables.
 *
 * KEY AGENTS:
 *   - Village Practitioners: Primary beneficiaries (powerless/constrained at local scope) — accessed multiple forms of sacred power without doctrinal contradiction; the substrate solved genuine coordination problems across the lifecycle
 *   - Temple-Shrine Networks: Primary beneficiaries (institutional/mobile at regional scope) — shared resources and infrastructure; the substrate enabled institutional survival in areas where separate maintenance would be unsustainable
 *   - Ritual Specialists: Primary beneficiaries (moderate/constrained at local scope) — could serve both traditions, expanding livelihood opportunities and community role
 *   - Doctrinal Purists: Victims (moderate/identity_locked at national scope, Meiji era) — experienced the substrate as contamination suppressing categorical distinctions their frameworks required
 *   - Meiji Separation Targets: Victims (powerless/trapped at local scope, 1868-1874) — experienced the substrate's forced dissolution as violent extraction; lost livelihoods, ritual infrastructure, and community coherence
 *   - Tokugawa Bakufu: Beneficiaries (institutional/arbitrage at national scope) — used temple-shrine networks as distributed governance infrastructure without enforcing unified state religion
 *   - Phenomenological Observer: Analytical perspective (analytical/analytical at universal scope) — risks naturalizing the substrate as universal religious pattern, potentially obscuring constructed institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate_flat_control, 0.28).
domain_priors:suppression_score(shinbutsu_ontological_substrate_flat_control, 0.32).
domain_priors:theater_ratio(shinbutsu_ontological_substrate_flat_control, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate_flat_control, extractiveness, 0.28).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate_flat_control, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate_flat_control, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate_flat_control, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate_flat_control, "Shinbutsu-shugo as Shared Ontological Substrate").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate_flat_control, "religious_studies/japanese_history/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate_flat_control, 'ee2f9d11-83ad-4495-9bc6-b3fc64fb1188').
narrative_ontology:cs_kernel_codification('ee2f9d11-83ad-4495-9bc6-b3fc64fb1188', distributed).
narrative_ontology:cs_authority_grounding('ee2f9d11-83ad-4495-9bc6-b3fc64fb1188', practice).
narrative_ontology:cs_interpretation_layer_present('ee2f9d11-83ad-4495-9bc6-b3fc64fb1188').
narrative_ontology:cs_created_at('ee2f9d11-83ad-4495-9bc6-b3fc64fb1188', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(shinbutsu_ontological_substrate_flat_control, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate_flat_control, village_practitioners).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate_flat_control, temple_shrine_networks).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate_flat_control, ritual_specialists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE PRACTITIONER (ROPE) — Experiences the substrate as functional coordination solving the genuine problem of how to access multiple forms of sacred power without ontological contradiction. Prays at both shrine and temple for different life events (birth at shrine, funeral at temple, harvest thanksgiving at both). The constraint enables rather than extracts — it legitimates combined practice that addresses the full range of human needs. Exit is constrained by community norms and limited doctrinal literacy, but extraction is minimal.
constraint_indexing:constraint_classification(shinbutsu_ontological_substrate_flat_control, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: TEMPLE-SHRINE NETWORK (ROPE) — Temple and shrine institutions coordinated through this substrate for centuries: shrines housed Buddhist statuary (shinbutsu), temples maintained shrine precincts (jingu-ji), ritual specialists served both. The arrangement solved resource-sharing problems and enabled institutional survival in rural areas where separate maintenance would be unsustainable. Low extraction — the coordination genuinely reduced overhead and expanded both institutions' reach. Mobile exit options (could theoretically separate) but chose coordination because it was mutually beneficial.
constraint_indexing:constraint_classification(shinbutsu_ontological_substrate_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: DOCTRINAL PURIST, MEIJI ERA (TANGLED ROPE) — From the standpoint of mid-19th-century nativist or Buddhist reform movements seeking categorical purity, the substrate becomes extractive. The purist sees genuine coordination (the substrate does solve practical problems) but also sees contamination: Shinto kami lose distinct indigenous character, Buddhist dharma gets diluted with kami-worship. This agent is identity-locked rather than structurally trapped — exit is possible (the Meiji shinbutsu bunri successfully separated the traditions) but requires abandoning the cosmopolitan scholarly identity that values both traditions. Experiences moderate extraction because the substrate suppresses the categorical distinctions the purist's framework requires.
constraint_indexing:constraint_classification(shinbutsu_ontological_substrate_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 4: TOKUGAWA BAKUFU (ROPE) — The shogunate benefited from the substrate as a coordination mechanism: the temple-shrine networks provided distributed governance infrastructure (population registration through temples, local ritual calendar coordination through shrines) without requiring active enforcement of a unified state religion. Arbitrage exit — the bakufu could have imposed separatism or exclusive Buddhism (as earlier regimes did) but chose coordination because it was administratively cheaper. Low extraction — the arrangement genuinely solved the regime's need for local social order without building separate bureaucratic apparatus.
constraint_indexing:constraint_classification(shinbutsu_ontological_substrate_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PHENOMENOLOGICAL OBSERVER (MOUNTAIN) — From a comparative religion standpoint emphasizing that religious experience precedes doctrinal categories, the substrate appears as an expression of a universal human pattern: sacred power is accessed through multiple channels, and ordinary practitioners experience no contradiction in combining them. This perspective sees the substrate as reflecting an underlying anthropological constant rather than a constructed arrangement. However, this is a candidate false summit — the naturalizing frame risks obscuring that the specific institutional arrangements (which deities share which precincts, which rituals belong to which specialists, how resources flow between temple and shrine) were historically negotiated and enforced, not inevitable.
constraint_indexing:constraint_classification(shinbutsu_ontological_substrate_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: MEIJI SEPARATION TARGET (SNARE) — During the 1868-1874 shinbutsu bunri, rural communities and lower-ranking priests who had organized their entire ritual lives and livelihoods around combined practice experienced the substrate's dissolution as violent extraction. Statues destroyed, temples closed, ritual specialists forced to choose Buddhism or Shinto and abandon half their practice. From this perspective at this moment, the substrate appears retroactively as a snare — the very coordination that had been functional now becomes a trap because it made practitioners vulnerable to state-mandated separation. This is time-indexed extraction: the substrate was rope during the Tokugawa period and became snare only when the Meiji state weaponized its dissolution.
constraint_indexing:constraint_classification(shinbutsu_ontological_substrate_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shinbutsu_ontological_substrate_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shinbutsu_ontological_substrate_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(shinbutsu_ontological_substrate_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate for most of the interval. The substrate genuinely solved coordination problems for practitioners and institutions. Village practitioners accessed both kami blessings and Buddhist merit without contradiction. Temple-shrine networks shared resources efficiently. Ritual specialists could serve both traditions. The extraction that exists comes from: (1) suppression of pre-Buddhist indigenous practices that didn't fit the substrate (the omega on conceptual capture addresses this), (2) institutional pressure on practitioners who might have preferred exclusive practice of one tradition, (3) late-Edo period rigidification as nativist movements arose. The dramatic spike to 0.75 during Meiji separation reflects time-indexed extraction — the constraint became extractive when weaponized by state dissolution, not because its internal operation was extractive. Base extractiveness is measured at the late Edo period before separation (0.28), reflecting moderate extraction from institutional rigidification and nativist critique. Suppression (0.32): Low-moderate. The substrate required some boundary maintenance (who decides when shrine vs temple ritual is appropriate, how resources are shared) but functioned primarily through custom and mutual benefit rather than coercion. Alternatives existed but were socially constrained rather than prohibited — exclusive Buddhism or exclusive kami worship were possible but marked as heterodox. Suppression increased over time as institutional arrangements calcified and nativist criticism mounted. Theater ratio (0.15): Low. The substrate was functionally operative — combined practice genuinely served practitioner needs and institutional coordination genuinely reduced overhead. Some performative elements existed (ritual specialists maintaining formal distinctions between kami and buddha ceremonies even when practitioners experienced them as continuous), but the core function was real.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates time-indexed perspectival variation: the same substrate appears as rope from the village practitioner (solves coordination problem), rope from temple-shrine networks (enables resource sharing), rope from the Tokugawa state (provides governance infrastructure), tangled rope from the Meiji-era doctrinal purist (genuine coordination contaminated with categorical confusion), mountain from the phenomenological observer (universal religious pattern), and snare from the Meiji separation target (coordination becomes trap when state weaponizes its dissolution). The mountain perspective is a candidate false summit — the analytical frame risks naturalizing historically contingent institutional arrangements as universal anthropological constants. The snare perspective is time-indexed: the substrate was not extractive during its operational period but became retroactively extractive when the Meiji state used practitioners' dependence on combined practice as a lever for forced separation. The purist's tangled-rope perspective reveals that even during the substrate's functional period, agents seeking categorical purity experienced extraction — their identity-lock (cosmopolitan scholarly identity valuing both traditions) prevented exit from a substrate that suppressed the distinctions their framework required.
 *
 * DIRECTIONALITY LOGIC:
 *   Village practitioners and temple-shrine networks are primary beneficiaries — the substrate enables their combined practice and resource-sharing with minimal extraction. These agents have low directionality values (approaching 0.0-0.2), experiencing the constraint as coordination rather than extraction. Ritual specialists are beneficiaries with moderate power and constrained exit, yielding low directionality (~0.15). The Tokugawa bakufu is an institutional beneficiary with arbitrage exit, yielding very low or negative directionality (the constraint subsidizes their governance needs). Doctrinal purists are victims with moderate power but identity-locked exit — they experience the substrate as contamination but cannot exit without abandoning their cosmopolitan scholarly identity. This yields moderate directionality (~0.4-0.5), producing the tangled-rope classification. Meiji separation targets are victims with powerless standing and trapped exit during the separation period, yielding high directionality (~0.8-0.9) and the snare classification. The analytical observer with mountain classification has analytical power and exit, yielding very low directionality — but this is a false summit candidate, as the beneficiary presence (village practitioners, temple-shrine networks) triggers FSM evaluation.
 *
 * MANDATROPHY ANALYSIS:
 *   The substrate resolves mandatrophy by demonstrating that a constraint can be simultaneously coordination (rope) from beneficiaries, mixed coordination-extraction (tangled rope) from agents seeking categorical purity, and pure extraction (snare) from targets of state-mandated dissolution — and that these are not competing claims about the constraint's 'true' nature but perspectival readings from different structural positions. The time-indexed extraction (the dramatic spike during Meiji separation) shows that extraction can be latent in a coordination arrangement and actualized only when an external actor weaponizes the arrangement's dissolution. The substrate was not 'really' a snare that looked like a rope; it was genuinely a rope for most participants for most of its history and became a snare for specific agents at a specific moment. The mandatrophy is resolved by recognizing that constraint type is not an invariant property but a function of the observation context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_vs_honji_suijaku,
    'Is the substrate claim (kami and buddhas as single reality) structurally distinct from honji-suijaku doctrine (buddhas as original essence, kami as local manifestations), or are they the same constraint under different framings?',
    'Historical analysis of when practitioners distinguished substrate-level claims from hierarchical honji-suijaku metaphysics; examination of whether non-honji-suijaku forms of shinbutsu-shugo existed and functioned differently.',
    'If distinct: the substrate is a more fundamental coordination claim than any specific theological doctrine, and honji-suijaku is one reading among several. If identical: the substrate claim collapses into honji-suijaku and loses its status as a separate constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_vs_honji_suijaku, conceptual, 'Whether substrate claim is distinct from honji-suijaku doctrine').

omega_variable(
    naturalness_vs_negotiated_arrangement,
    'Is the substrate a natural emergence from polytheistic religious psychology, or a constructed institutional arrangement that required active maintenance?',
    'Cross-cultural comparison of polytheistic systems that did vs did not develop similar substrates; examination of enforcement mechanisms (who adjudicated boundary disputes between temple and shrine, how resource-sharing was negotiated); analysis of whether the substrate persisted after Meiji separation in folk practice (suggesting natural) or collapsed (suggesting constructed).',
    'If natural: mountain classification strengthened; the substrate reflects an underlying constant in how humans engage multiple sacred categories. If constructed: the mountain perspective is a false summit; the substrate was a historically contingent coordination arrangement that benefited specific institutional actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalness_vs_negotiated_arrangement, empirical, 'Whether substrate is natural or requires active maintenance').

omega_variable(
    coordination_vs_conceptual_capture,
    'Did the substrate genuinely solve a practitioner coordination problem (how to access both kami and buddha benefits), or did it capture practitioners in a conceptual frame that obscured pre-Buddhist indigenous practices?',
    'Archaeological and textual evidence of pre-Buddhist kami worship practices; analysis of what changed after Buddhism''s arrival; examination of whether post-Meiji Shinto ''purification'' recovered older practices or invented new ones; comparison of substrate vs non-substrate polytheistic systems on metrics of practitioner autonomy and ritual diversity.',
    'If genuine coordination: rope classification confirmed across most perspectives. If conceptual capture: the substrate becomes a tangled rope or snare from more perspectives, especially for carriers of pre-Buddhist traditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_conceptual_capture, empirical, 'Whether substrate coordinates or captures practitioners').

omega_variable(
    meiji_separation_as_exogenous_shock,
    'Does the Meiji shinbutsu bunri represent an exogenous shock to an otherwise stable coordination arrangement, or does it reveal latent tensions that were always present in the substrate?',
    'Historical analysis of pre-Meiji conflicts over temple-shrine boundaries, resource allocation disputes, and doctrinal controversies; examination of whether nativist movements existed before Western contact; assessment of whether post-separation folk practice reverted to combined worship (suggesting the separation was purely top-down) or maintained separation (suggesting it aligned with latent preferences).',
    'If exogenous shock: the substrate''s time-indexed extraction (rope → snare during separation) reflects state violence, not substrate fragility. If latent tensions: the substrate was always a tangled rope, and the Meiji state merely revealed extraction that doctrinal purists had experienced all along.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_exogenous_shock, empirical, 'Whether Meiji separation reveals latent substrate tensions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate_flat_control, 0, 1268).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_flat_theater_nara, shinbutsu_ontological_substrate_flat_control, theater_ratio, 0, 0.1).
narrative_ontology:measurement(shinbutsu_flat_theater_kamakura, shinbutsu_ontological_substrate_flat_control, theater_ratio, 400, 0.12).
narrative_ontology:measurement(shinbutsu_flat_theater_muromachi, shinbutsu_ontological_substrate_flat_control, theater_ratio, 800, 0.15).
narrative_ontology:measurement(shinbutsu_flat_theater_edo, shinbutsu_ontological_substrate_flat_control, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(shinbutsu_flat_theater_late_edo, shinbutsu_ontological_substrate_flat_control, theater_ratio, 1200, 0.18).
narrative_ontology:measurement(shinbutsu_flat_theater_meiji_separation, shinbutsu_ontological_substrate_flat_control, theater_ratio, 1268, 0.25).

% Extraction over time
narrative_ontology:measurement(shinbutsu_flat_extractiveness_nara, shinbutsu_ontological_substrate_flat_control, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(shinbutsu_flat_extractiveness_kamakura, shinbutsu_ontological_substrate_flat_control, base_extractiveness, 400, 0.18).
narrative_ontology:measurement(shinbutsu_flat_extractiveness_muromachi, shinbutsu_ontological_substrate_flat_control, base_extractiveness, 800, 0.22).
narrative_ontology:measurement(shinbutsu_flat_extractiveness_edo, shinbutsu_ontological_substrate_flat_control, base_extractiveness, 1000, 0.28).
narrative_ontology:measurement(shinbutsu_flat_extractiveness_late_edo, shinbutsu_ontological_substrate_flat_control, base_extractiveness, 1200, 0.35).
narrative_ontology:measurement(shinbutsu_flat_extractiveness_meiji_separation, shinbutsu_ontological_substrate_flat_control, base_extractiveness, 1268, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_flat_suppression_nara, shinbutsu_ontological_substrate_flat_control, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(shinbutsu_flat_suppression_kamakura, shinbutsu_ontological_substrate_flat_control, suppression_requirement, 400, 0.22).
narrative_ontology:measurement(shinbutsu_flat_suppression_muromachi, shinbutsu_ontological_substrate_flat_control, suppression_requirement, 800, 0.28).
narrative_ontology:measurement(shinbutsu_flat_suppression_edo, shinbutsu_ontological_substrate_flat_control, suppression_requirement, 1000, 0.32).
narrative_ontology:measurement(shinbutsu_flat_suppression_late_edo, shinbutsu_ontological_substrate_flat_control, suppression_requirement, 1200, 0.38).
narrative_ontology:measurement(shinbutsu_flat_suppression_meiji_separation, shinbutsu_ontological_substrate_flat_control, suppression_requirement, 1268, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate_flat_control, identity_coordination).

% DUAL FORMULATION NOTE:
% This flat construction treats the substrate as a single constraint. The question of whether honji-suijaku doctrine, ryobu-shinto esoteric unification, or other specific theological frameworks represent separate constraints or readings of this substrate is addressed through omega variables rather than network decomposition. If subsequent analysis determines that these frameworks have sufficiently distinct epsilon values (different beneficiary/victim structures, different extraction profiles), they should be decomposed into separate constraint stories and linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
