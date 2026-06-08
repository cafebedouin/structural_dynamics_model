% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment_flat_control, []).

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
 *   constraint_id: temple_sacrifice_commitment_flat_control
 *   human_readable: Temple Sacrifice Law as Divinely Commanded Practice
 *   domain: religious_law/halakhic_tradition/commitment_system
 *
 * SUMMARY:
 *   The Temple sacrifice system represents a commitment-system constraint
 *   grounded in fixed textual authority (Torah) claiming divine command
 *   status, requiring specific material infrastructure (Temple building,
 *   hereditary priesthood, ritual purity protocols) for legitimate
 *   performance. The system operated as active religious law during Second
 *   Temple period (roughly -500 to +70 CE), extracting mandatory offerings
 *   and pilgrimage obligations from the broader Jewish population while
 *   concentrating religious authority and material benefit in the priestly
 *   class. The constraint's lifecycle demonstrates clear scaffold dynamics:
 *   Temple destruction in 70 CE functioned as the sunset event, after which
 *   rabbinic Judaism constructed alternative frameworks (prayer, study,
 *   ethical conduct) that replaced rather than merely suspended the sacrifice
 *   mechanism. Two millennia of Judaism without Temple sacrifice provides
 *   strong empirical evidence that the material conditions were not eternally
 *   binding divine requirements but historically contingent institutional
 *   arrangements. However, some Orthodox traditions maintain the suspension
 *   interpretation, holding sacrifice law remains binding but unperformable
 *   pending Temple restoration. This interpretive split is the core
 *   contestation.
 *
 * KEY AGENTS:
 *   - Non-priestly observant Jews: Primary victims (powerless/identity_locked pre-70 CE, moderate/mobile post-70 CE) — bore mandatory offering costs and pilgrimage burdens with no exit that preserved Jewish identity; trapped in system during its active phase
 *   - Priestly class (Kohanim): Primary beneficiaries (institutional/arbitrage) — hereditary administrators collecting material offerings, holding exclusive ritual authority, experiencing system as coordination of religious service with legitimate compensation
 *   - Temple administration: Institutional coordinator (institutional/constrained) — managed sacrifice infrastructure, enforced purity and offering requirements, extracted resources while providing communal religious function
 *   - Rabbinic movement: Organized agents building exit pathway (organized/constrained post-70 CE) — constructed prayer/study framework as replacement rather than stopgap, proving system's transitional nature
 *   - Contemporary diaspora Jews: Observing completed sunset (moderate/mobile) — two millennia post-Temple demonstrates material conditions were contingent, not eternal
 *   - Analytical observer: Measuring false summit risk (analytical/analytical) — immutable divine command framing may naturalize human institutional arrangement; substantial extraction and beneficiary structure contradict mountain metrics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment_flat_control, 0.35).
domain_priors:suppression_score(temple_sacrifice_commitment_flat_control, 0.65).
domain_priors:theater_ratio(temple_sacrifice_commitment_flat_control, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment_flat_control, extractiveness, 0.35).
narrative_ontology:constraint_metric(temple_sacrifice_commitment_flat_control, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_commitment_flat_control, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment_flat_control, scaffold).
narrative_ontology:human_readable(temple_sacrifice_commitment_flat_control, "Temple Sacrifice Law as Divinely Commanded Practice").
narrative_ontology:topic_domain(temple_sacrifice_commitment_flat_control, "religious_law/halakhic_tradition/commitment_system").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment_flat_control).
narrative_ontology:has_sunset_clause(temple_sacrifice_commitment_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment_flat_control, '4783b4b8-9c0c-47c2-823a-bead6577909b').
narrative_ontology:cs_kernel_codification('4783b4b8-9c0c-47c2-823a-bead6577909b', fixed_text).
narrative_ontology:cs_authority_grounding('4783b4b8-9c0c-47c2-823a-bead6577909b', lineage).
narrative_ontology:cs_interpretation_layer_present('4783b4b8-9c0c-47c2-823a-bead6577909b').
narrative_ontology:cs_created_at('4783b4b8-9c0c-47c2-823a-bead6577909b', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(temple_sacrifice_commitment_flat_control, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment_flat_control, priestly_class).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment_flat_control, temple_administration).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment_flat_control, religious_community_coordination).
narrative_ontology:constraint_victim(temple_sacrifice_commitment_flat_control, non_priestly_observant_jews).
narrative_ontology:constraint_victim(temple_sacrifice_commitment_flat_control, economic_burden_bearers).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment_flat_control, divine_command_legitimacy).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment_flat_control, covenantal_obligation_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The sacrifice system coordinates communal religious obligation fulfillment, providing a centralized mechanism for sin atonement, festival observance, and covenantal renewal rituals that the community understands as divinely mandated.
% TRANSFER_FUNCTION: Mandatory animal offerings, grain offerings, firstborn redemption payments, and Temple tax flow from non-priestly Jewish households to the Temple administration and priestly class, who hold exclusive authority to perform the rituals.
% ABSENT_VOICES: Non-priestly Jews who bear the economic burden have voice through pilgrimage participation but no governance role in the system's administration. Gentiles, women without independent household authority, and the poor who cannot afford mandated offerings are structurally excluded from full participation while still subject to obligation claims.
% DISAPPEARANCE_RATIONALE: Temple destruction in 70 CE did cause the world to rearrange: Judaism reconstructed around prayer, study, and ethical conduct rather than sacrifice. The two-millennium gap demonstrates that arrangements did depend on the constraint — when it became unperformable, the religious framework transformed rather than persisted unchanged. This is evidence of scaffold sunset, not mountain immutability.
% FOUNDING_PROBLEM: Provide a concrete mechanism for Israelite covenant community to fulfill what were understood as divine commandments for sin atonement, thanksgiving, and festival observance, coordinating individual religious obligation with centralized ritual performance.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic tradition post-70 CE explicitly addresses this through Yohanan ben Zakkai's question at Yavneh ('How shall we atone without Temple?') and the consensus answer that prayer, charity, and study provide atonement. Two thousand years of normative Jewish practice without sacrifice demonstrates cross-denominational acceptance that the founding problem has been solved by alternative means. Corroboration: Mishnah and Talmud codify the replacement framework; contemporary Jewish denominational statements across Orthodox, Conservative, and Reform movements either accept prayer as sufficient (Reform/Conservative) or hold sacrifice suspended pending messianic restoration (Orthodox), with none arguing the founding problem remains live and unsolved.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment_flat_control, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-PRIESTLY OBSERVANT JEW (SNARE) — Identity-locked within covenantal framework; cannot exit without abandoning Jewish identity. Bears economic burden of mandatory offerings and pilgrimages. Experiences the constraint as extraction: obligated to provide animals, grain, and resources to a system administered by hereditary class with no alternative pathway to fulfill divine commands. The material conditions (Temple location, priestly mediation, purity requirements) create dependency structure.
constraint_indexing:constraint_classification(temple_sacrifice_commitment_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: PRIESTLY CLASS (ROPE) — Primary beneficiaries of the system. Experiences constraint as coordination mechanism: the sacrifice law creates stable income stream, social status, and religious authority. Can exit through geographic mobility or profession change. From this seat, the system coordinates legitimate religious service with material support for those performing it. The extraction others experience runs toward this group.
constraint_indexing:constraint_classification(temple_sacrifice_commitment_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: RABBINIC MOVEMENT (SCAFFOLD) — Organized agents building alternative prayer/study framework after Temple destruction. Sees sacrifice system as temporary structure whose sunset has occurred: prayer replaces sacrifice, Torah study replaces altar service, synagogue replaces Temple. The material conditions were never permanent divine requirements but transitional mechanisms for a specific covenant phase. Constrained by tradition's weight but actively constructing exit pathway through reinterpretation.
constraint_indexing:constraint_classification(temple_sacrifice_commitment_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: TEMPLE ADMINISTRATION (TANGLED ROPE) — Coordinates genuine religious function (communal atonement, festival observance, covenantal renewal) while extracting substantial material resources. Requires active enforcement of pilgrimage obligations, purity standards, and offering requirements. Benefits from coordination role but also constrained by the system's material demands and vulnerability to political disruption. Mixed coordination and extraction.
constraint_indexing:constraint_classification(temple_sacrifice_commitment_flat_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: DIASPORA COMMUNITY (SCAFFOLD) — From contemporary perspective with mobile exit options, the sacrifice system is clearly transitional. Two millennia of Judaism without Temple sacrifice demonstrates the sunset was real. Prayer, charity, and ethical conduct have replaced animal offerings. The material conditions were historically contingent, not eternally binding. The constraint's function has been fulfilled and superseded.
constraint_indexing:constraint_classification(temple_sacrifice_commitment_flat_control, scaffold,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: IMMUTABLE DIVINE COMMAND VIEW (MOUNTAIN) — Some Orthodox traditions hold that sacrifice law remains eternally binding, merely suspended by force majeure (Temple destruction). From this view, the constraint is unchangeable divine will, not human institution. The material conditions are prerequisites, not negotiable elements. This classification faces false summit detection: the substantial extraction, active enforcement requirements, and clear beneficiary structure contradict mountain metrics. The 'eternal divine law' framing may naturalize what is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(temple_sacrifice_commitment_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temple_sacrifice_commitment_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temple_sacrifice_commitment_flat_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(temple_sacrifice_commitment_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35 base, declining sharply post-70 CE): Moderate during active phase. Non-priestly families faced mandatory animal offerings for sin/guilt atonements, festival sacrifices, firstborn redemption, and Temple tax. Archaeological evidence suggests 15-25% of household agricultural output flowed to Temple system for observant families. This is substantial but not maximal extraction — the system provided genuine communal religious functions (atonement ritual, festival coordination, covenantal renewal) alongside the extraction. Post-destruction, extractiveness collapses to near-zero as prayer replaces offerings. Suppression (0.65 base, declining post-70 CE): Moderate-high during active phase. No alternative pathway for sin atonement or fulfilling covenantal obligations — Temple sacrifice was presented as sole divinely-mandated mechanism. Exit required abandoning Jewish religious identity entirely (identity_locked exit for observant population). Purity requirements, pilgrimage obligations, and priestly mediation created high barriers. Post-destruction, suppression declines as rabbinic framework provides alternatives, though some traditions maintain the binding obligation (suppression persists for those identity-locked in restoration expectation). Theater ratio (0.15 base, spiking to 0.90+ post-70 CE): Low during active phase — sacrifices were functionally performed, blood ritually applied, offerings consumed. The system operated as claimed, not performatively. Post-destruction, theater ratio spikes dramatically: liturgical references to sacrifice become memorial rather than functional, Yom Kippur service describes rather than performs atonement ritual, priestly blessing recitation occurs without Temple context. The constraint persists almost entirely as commemorative performance with negligible functional content.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates clean scaffold dynamics from most perspectives: a coordination mechanism with genuine religious function that extracted from non-priestly population, benefited hereditary priestly class, and reached its sunset when Temple destruction made performance impossible. Rabbinic movement's construction of prayer/study replacement, sustained successfully for two millennia, proves the system was transitional rather than eternal. However, the analytical mountain perspective (immutable divine command view) creates diagnostic tension: some Orthodox traditions hold sacrifice law remains binding, merely suspended. This view faces false summit detection — the substantial extraction (mandatory offerings), active enforcement (pilgrimage requirements, purity protocols), clear beneficiary structure (priestly class), and successful replacement by alternative framework all contradict mountain metrics. The 'eternal divine command' framing naturalizes what empirical evidence shows was a contingent human institution that served its function and was superseded. The core perspectival gap is between those who see Temple destruction as the sunset event proving transitional nature (scaffold) versus those who see it as force majeure suspension of eternal requirement (mountain). The extractiveness and beneficiary data support scaffold interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-priestly observant Jews are victims with identity_locked exit during Second Temple period, producing high directionality toward full target (d approaching 1.0) and high effective extraction. They bear the offering costs and pilgrimage burdens with no alternative that preserves religious identity. Post-70 CE, their exit options improve to mobile as rabbinic framework provides alternatives, and their victim status diminishes as extraction mechanism dissolves. Priestly class are beneficiaries with arbitrage exit, producing low directionality (d approaching 0.0) and negative effective extraction — they experience net subsidy from the system. They collect offerings, hold exclusive ritual authority, and can exit geographically or occupationally if desired. Temple administration is beneficiary with constrained exit — they coordinate and extract but are also bound by system maintenance requirements. Rabbinic movement are organized agents building exit pathway, with moderate directionality reflecting their mixed position: constrained by tradition but actively constructing alternatives. Contemporary diaspora see completed scaffold with mobile exit and beneficiary relationship to the replacement framework (prayer/study). The analytical mountain view risks zero directionality (treating constraint as natural law affecting no one differentially), but structural data contradicts this — clear beneficiaries and victims existed, producing asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating how a system with genuine coordination function (communal atonement, festival observance, covenantal renewal) and substantial extraction (mandatory offerings to hereditary priestly class) reaches a clear sunset when material conditions become impossible. Scaffold classification is justified by: (1) declared transitional nature in some textual traditions (prophetic critiques of sacrifice, 'I desire mercy not sacrifice'); (2) successful replacement by alternative framework proving function was not dependent on material mechanism; (3) two-millennium sustainability of Judaism without Temple demonstrating the original system's contingency. The tangled_rope perspective (Temple administration) captures the mixed coordination/extraction during active phase. The snare perspective (trapped non-priestly population) captures the high-extraction experience for those bearing costs. The rope perspective (priestly beneficiaries) captures the coordination-as-experienced-by-collectors view. The mountain perspective (immutable divine command) is the false summit the framework exists to detect: substantial extraction and beneficiary concentration contradict natural law metrics, revealing theological framing as naturalization mechanism. The system's mandate (provide covenantal atonement mechanism) was fulfilled and superseded, not eternal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_command_vs_human_institution,
    'Is the sacrifice system a direct divine command (unchangeable natural law) or a human institutional arrangement legitimated by theological framing?',
    'Historical analysis of how the system evolved, adapted, and was ultimately replaced; cross-cultural comparison of sacrifice systems; textual evidence of human authorship vs divine dictation claims',
    'If divine command: mountain classification justified, extraction is illusion. If human institution: scaffold/tangled_rope classification confirmed, theological framing is naturalization mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_command_vs_human_institution, conceptual, 'Whether sacrifice law is divine command or human institution').

omega_variable(
    sunset_interpretation_contestation,
    'Does Temple destruction constitute the sunset event proving the system was transitional, or merely temporary suspension of an eternal requirement?',
    'Theological debate resolution; observation of whether Third Temple reconstruction attempts restore sacrifice or adopt alternative framework; consensus emergence in rabbinic tradition',
    'If sunset: scaffold classification confirmed across more perspectives. If suspension: mountain/rope classifications gain weight, and post-70 CE prayer framework is stopgap rather than replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_interpretation_contestation, conceptual, 'Whether Temple destruction was sunset or suspension').

omega_variable(
    economic_extraction_magnitude,
    'What proportion of household resources did mandatory offerings extract from non-priestly families in Second Temple period?',
    'Archaeological evidence of economic activity, household budgets, and offering requirements; historical records of pilgrimage costs and Temple tax burdens',
    'If extraction < 10% of household income: coordination function dominates, rope/scaffold from more perspectives. If extraction > 25%: snare/tangled_rope from more perspectives, system was substantially extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_extraction_magnitude, empirical, 'Magnitude of economic extraction on non-priestly households').

omega_variable(
    priestly_class_mobility,
    'Were Kohanim genuinely able to exit priestly service, or was hereditary status functionally inescapable?',
    'Historical evidence of Kohanim pursuing non-priestly occupations, geographic mobility patterns, social penalties for abandoning priestly role',
    'If mobile: arbitrage exit confirmed, rope classification for priestly perspective strengthened. If identity_locked: even beneficiaries were trapped in system, raising effective extraction across all agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_class_mobility, empirical, 'Whether priestly class could exit their hereditary role').

omega_variable(
    alternative_atonement_sufficiency,
    'Did rabbinic prayer/study framework genuinely replace sacrifice''s religious function, or merely provide stopgap pending restoration?',
    'Theological development tracking; community acceptance of prayer as sufficient atonement; persistence or decline of Temple restoration movements over two millennia',
    'If genuine replacement: scaffold sunset confirmed, sacrifice system was transitional. If stopgap: scaffold is aspirational rather than structural, and many contemporary Jews remain identity_locked in awaiting restoration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_atonement_sufficiency, empirical, 'Whether rabbinic framework replaced or merely suspended sacrifice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment_flat_control, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temple_sac_theater_second_temple_early, temple_sacrifice_commitment_flat_control, theater_ratio, 0, 0.1).
narrative_ontology:measurement(temple_sac_theater_second_temple_late, temple_sacrifice_commitment_flat_control, theater_ratio, 200, 0.15).
narrative_ontology:measurement(temple_sac_theater_post_destruction_early, temple_sacrifice_commitment_flat_control, theater_ratio, 400, 0.85).
narrative_ontology:measurement(temple_sac_theater_medieval, temple_sacrifice_commitment_flat_control, theater_ratio, 1000, 0.9).
narrative_ontology:measurement(temple_sac_theater_modern, temple_sacrifice_commitment_flat_control, theater_ratio, 1800, 0.92).
narrative_ontology:measurement(temple_sac_theater_contemporary, temple_sacrifice_commitment_flat_control, theater_ratio, 2000, 0.95).

% Extraction over time
narrative_ontology:measurement(temple_sac_extract_second_temple_early, temple_sacrifice_commitment_flat_control, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(temple_sac_extract_second_temple_late, temple_sacrifice_commitment_flat_control, base_extractiveness, 200, 0.45).
narrative_ontology:measurement(temple_sac_extract_post_destruction_early, temple_sacrifice_commitment_flat_control, base_extractiveness, 400, 0.15).
narrative_ontology:measurement(temple_sac_extract_medieval, temple_sacrifice_commitment_flat_control, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(temple_sac_extract_modern, temple_sacrifice_commitment_flat_control, base_extractiveness, 1800, 0.02).
narrative_ontology:measurement(temple_sac_extract_contemporary, temple_sacrifice_commitment_flat_control, base_extractiveness, 2000, 0.01).

% Suppression requirement over time
narrative_ontology:measurement(temple_sac_suppress_second_temple_early, temple_sacrifice_commitment_flat_control, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(temple_sac_suppress_second_temple_late, temple_sacrifice_commitment_flat_control, suppression_requirement, 200, 0.75).
narrative_ontology:measurement(temple_sac_suppress_post_destruction_early, temple_sacrifice_commitment_flat_control, suppression_requirement, 400, 0.4).
narrative_ontology:measurement(temple_sac_suppress_medieval, temple_sacrifice_commitment_flat_control, suppression_requirement, 1000, 0.2).
narrative_ontology:measurement(temple_sac_suppress_modern, temple_sacrifice_commitment_flat_control, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(temple_sac_suppress_contemporary, temple_sacrifice_commitment_flat_control, suppression_requirement, 2000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment_flat_control, identity_coordination).

% DUAL FORMULATION NOTE:
% This flat construction treats the entire Temple sacrifice system as a single constraint. Alternative framings could decompose into: (1) the theological claim of divine command vs. (2) the institutional enforcement mechanism vs. (3) the material extraction structure. The current framing bundles all three because the divine command claim is the legitimation mechanism for the institutional structure, making them a single commitment-system unit. If decomposition revealed substantially different epsilon values for 'theological claim' vs. 'enforcement mechanism', network decomposition would be warranted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
