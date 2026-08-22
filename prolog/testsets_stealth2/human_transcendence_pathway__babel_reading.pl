% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__babel_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__babel_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: human_transcendence_pathway__babel_reading
 *   human_readable: Babel Reading: Unified-Power Self-Sufficiency Project
 *   domain: political_theology/technology_ethics
 *
 * SUMMARY:
 *   A population attempts what its charter names plainly: to secure itself,
 *   by its own collected power, against fragmentation and need — one speech,
 *   one technical standard, one building program, and no standard of judgment
 *   above the project. This file instantiates the babel_reading of the
 *   human_transcendence_pathway kernel: the reading on which the
 *   arrangement's genuine coordination achievement (a mutually unintelligible
 *   population made able to act together at scale) is purchased through
 *   enforced homogenization, with gains concentrating in the architect seat
 *   and costs borne by the laborers and by every community whose tongue the
 *   standard displaces. The epsilon referent is the standing arrangement
 *   under contest — the unified-power project as it actually operates —
 *   assessed by this reading's own lights, which register coercive
 *   homogenization. Assumptions: the interval 0-30 is abstract build-cycle
 *   time, not calendar years, and the referent is the archetypal arrangement
 *   (imperial language standard, centralized megaproject, platform
 *   monoculture are its concrete instances). Per the epsilon-invariance
 *   principle this file does not fold in the sibling readings: the
 *   jerusalem_reading and the technocratic_vs_incarnational_reading are
 *   separate constraints over the same kernel, linked through
 *   network.affects_constraints, with the reading-contest structure carried
 *   in the omegas.
 *
 * KEY AGENTS:
 *   - - project_architects: agenda-setting seat (institutional/arbitrage) — sets the standard, administers enforcement, collects the concentration
 *   - - conscripted_laborers: primary target (powerless/trapped) — bears the labor transfer directly
 *   - - minority_language_communities: primary target (moderate/constrained) — bears the cultural and linguistic erasure
 *   - - standard_language_communities: secondary beneficiary (moderate/mobile) — gains mobility and standing at the standard's reach
 *   - - project_engineers_technocrats: captured dual seat (organized/identity_locked) — runs the system it is bound to
 *   - - transcendent_reference_custodians: excluded voice (organized/constrained) — barred by the charter's own premise
 *   - - post_collapse_witnesses: analytical observer (moderate/analytical) — sees the arrangement's full shape from outside its promise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__babel_reading, 0.78).
domain_priors:suppression_score(human_transcendence_pathway__babel_reading, 0.82).
domain_priors:theater_ratio(human_transcendence_pathway__babel_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__babel_reading, tangled_rope).
narrative_ontology:human_readable(human_transcendence_pathway__babel_reading, "Babel Reading: Unified-Power Self-Sufficiency Project").
narrative_ontology:topic_domain(human_transcendence_pathway__babel_reading, "political_theology/technology_ethics").

domain_priors:requires_active_enforcement(human_transcendence_pathway__babel_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__babel_reading, '4202461e-0562-4019-b3a3-fd9024f581c5').
narrative_ontology:cs_kernel_codification('4202461e-0562-4019-b3a3-fd9024f581c5', formalized).
narrative_ontology:cs_authority_grounding('4202461e-0562-4019-b3a3-fd9024f581c5', extraction).
narrative_ontology:cs_interpretation_layer_present('4202461e-0562-4019-b3a3-fd9024f581c5').
narrative_ontology:cs_reading_relation('4202461e-0562-4019-b3a3-fd9024f581c5', human_transcendence_pathway__jerusalem_reading, coexists_with).
narrative_ontology:cs_reading_relation('4202461e-0562-4019-b3a3-fd9024f581c5', human_transcendence_pathway__technocratic_vs_incarnational_reading, influences).
narrative_ontology:cs_axiom('4202461e-0562-4019-b3a3-fd9024f581c5', foundational, collective_power_self_sufficiency).
narrative_ontology:cs_axiom_status(collective_power_self_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('4202461e-0562-4019-b3a3-fd9024f581c5', collective_power_self_sufficiency, instrumental).
narrative_ontology:cs_axiom('4202461e-0562-4019-b3a3-fd9024f581c5', foundational, uniformity_precondition_of_coordination).
narrative_ontology:cs_axiom_status(uniformity_precondition_of_coordination, holdable).
narrative_ontology:cs_axiom_grounding('4202461e-0562-4019-b3a3-fd9024f581c5', uniformity_precondition_of_coordination, instrumental).
narrative_ontology:cs_reference_frame('4202461e-0562-4019-b3a3-fd9024f581c5', unified_project_charter).
narrative_ontology:cs_drift_state('4202461e-0562-4019-b3a3-fd9024f581c5', late_build_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4202461e-0562-4019-b3a3-fd9024f581c5', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__babel_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, project_architects).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, standard_language_communities).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, project_engineers_technocrats).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, conscripted_laborers).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, minority_language_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, project_engineers_technocrats).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__babel_reading, unified_standard_coordination_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the unified language standard and the shared technical system it enables. They set what counts as proper speech, approve the building programs, and assign labor quotas. They collect the project's concentrated rewards — command over resources and labor, the monument's renown, and the security the charter promises. Their exit is the easiest in the arrangement: they can reposition within the system they designed, and their standing survives the failure of any single work.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, project_architects, agenda_setter,
    institutional, generational, arbitrage, global).

% The populations whose speech became the project's standard. They trade, travel, and take administrative work without paying translation or assimilation costs, and their dialect carries official standing everywhere the system reaches. Their advantage depends on the center's continued reach; they can relocate freely within the system but not outside it without forfeiting that standing.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, standard_language_communities, beneficiary,
    moderate, biographical, mobile, regional).

% The technical and managerial class that keeps the unified system running — kiln schedules, material standards, logistics, records. The project gives them status, purpose, and the largest works anyone has attempted; in exchange it consumes their judgment (they execute quotas they did not set) and binds their reputations to the project's success. Leaving would mean abandoning the only scale of work at which their skills are now recognized.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, project_engineers_technocrats, beneficiary,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__babel_reading, project_engineers_technocrats, payer).

% Supply the physical work — digging, hauling, firing brick — under quotas set far above them. They eat from the project's stores and are housed in the works' shadow; refusing the quota means losing both. They have no voice in what is built or why, and no assembly through which to bargain.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, conscripted_laborers, payer,
    powerless, immediate, trapped, local).

% Communities whose tongues and lifeways fall outside the standard. Each generation must translate itself into the official speech to trade, litigate, or hold office; their own languages lose domains year by year — first administration, then markets, then the home. They keep enclaves and memory, but every practical exit runs through the standard they are being erased by.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, minority_language_communities, payer,
    moderate, generational, constrained, regional).

% The tradition-bearers — elders, priests, prophetic voices — who hold that the project must answer to a standard above itself and that its self-sufficiency claim is the oldest of human errors. The project's founding premise bars them from its deliberations: a charter that recognizes no transcendent reference has no seat for its custodians. They speak from the margins, and their critique is heard chiefly when it embarrasses the center.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, transcendent_reference_custodians, excluded,
    organized, generational, constrained, regional).

% Communities that lived through an earlier unified project's failure and carry its memory — the works abandoned half-built, the speech that stopped being mutually intelligible when the center's enforcement lapsed. They collect nothing from the current project and bear little of its direct cost; what they hold is pattern-recognition, the ability to see the whole arrangement's shape from outside its promise.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, post_collapse_witnesses, observer,
    moderate, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_transcendence_pathway__babel_reading, project_architects).
narrative_ontology:fixing_cost_class(human_transcendence_pathway__babel_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: One speech and one technical standard let a previously mutually unintelligible population pool labor, move materials over distance, and attempt works no local community could undertake alone — the coordination problem solved is large-scale cooperation across linguistic diversity.
% TRANSFER_FUNCTION: Moves labor, obedience, and linguistic and cultural particularity from the laborers and minority communities to the project center; the standard language is the medium through which the center collects both work and conformity, and the monument concentrates the renown.
% ABSENT_VOICES: The custodians of transcendent reference are barred by the charter's own premise — a project that acknowledges no standard above itself has no seat for those who hold one. The laborers have no assembly and no voice in what is built. The minority communities appear only as assimilation targets; the people whose tongues the standard erases would dissolve the project's self-description if they could speak in it.
% DISAPPEARANCE_RATIONALE: If the unified system and its enforcement vanished overnight, the megaprojects halt for lack of a medium of command, the center loses its reach and its revenue of conformity, trade and administration re-localize, and the minority tongues begin recovering their lost domains within a generation — the whole built order rearranges around the standard's absence.
% FOUNDING_PROBLEM: A dispersed, mutually unintelligible population cannot pool labor, secure its stores, or attempt common works — the founding problem is collective vulnerability to fragmentation and scattering.
% FOUNDING_PROBLEM_CORROBORATION: The minority-language communities and the post-collapse witnesses corroborate, from outside the benefiting parties, that the founding problem — fragmentation and vulnerability — is real and still live. The transcendent-reference custodians corroborate the problem's reality while testifying that the project's answer manufactures a worse form of the same vulnerability. No part of the status claim rests on the architects' own attestation.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__babel_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__babel_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__babel_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(human_transcendence_pathway__babel_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__babel_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__babel_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_transcendence_pathway__babel_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(human_transcendence_pathway__babel_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high and rising (0.55 to 0.78 across the interval) because the transfer decouples early: once the standard holds, the center's take of labor and conformity grows without any matching growth in what the many receive. Suppression is the arrangement's load-bearing wall (0.55 to 0.82): the standard's reach is enforced in administration, markets, and schooling, and the exits — keeping one's tongue, declining the quota — are actively closed. Theater rises (0.15 to 0.46) as the monumental self-naming function grows: an increasing share of activity defends the project's renown and its charter rather than performing coordination, the Goodhart drift this reading expects as the sufficiency promise strains; it stays below 0.5 because the coordination function is still substantially real. Accessibility_collapse is moderate (0.55): the alternatives — enclave tongues, local exchange — persist at the margins but are crowded out of every official domain, so they collapse in viability without disappearing. Resistance (0.6) is real: quota slowdowns, enclave persistence, and the custodians' standing critique. The victims' coalition potential is structurally suppressed: laborers and minority communities could combine, but the unified standard — the only medium through which a coalition at this scale could coordinate — is the constraint's own instrument, so the arrangement monopolizes the medium of its opposition. The measurement series share one time grid (0, 5, 10, 15, 20, 25, 30) across all tracked metrics; the drift is monotonic — an enforcement ratchet, not a cycle — so no cyclical machinery is invoked.
 *
 * PERSPECTIVAL GAP:
 *   The architect seat computes a different constraint from the laborer and minority seats. From the center, the arrangement is the coordination it built and the security it promised — the charter's own self-description. From the trapped and constrained target seats, the same structure is quota, erasure, and closed exit. The engineers sit between: they experience the arrangement as vocation (the largest works anyone has attempted) while their judgment is consumed by it — the identity lock that makes their exit unthinkable, and whose breaking would move their seat sharply toward the target end. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The architects sit near the beneficiary end: they set the rules, collect the concentration, and hold arbitrage-grade exit within the system they designed. The laborers and minority communities sit near the target end: both bear the transfer directly, and the laborer's trapped exit places that seat nearer full-target than the minority community's constrained exit. The standard-language communities derive low d from their beneficiary declaration, with the contingency carried in the standard_speaker_net_position omega. The engineers are dual-positioned: beneficiary by role, payer by capture — their identity_locked exit pushes their effective position toward the target end despite the beneficiary declaration, which is exactly the case the derivation chain handles through exit modulation rather than a hand-set override (none is used here; the structural data carries the distinction). The custodians are excluded rather than coordinated: the charter's premise is precisely their exclusion, so they sit outside the extraction flow while naming it. The witnesses hold the analytical seat. The arrangement's scope is continental-to-global; verification of the standard's fairness degrades with reach, and the engine's scope scaling registers that amplification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmentation, vulnerability to scattering — is live, so this is not a dead mandate kept alive by inertia; the mandatrophy question here is subtler. This reading's claim is that the arrangement now manufactures the vulnerability it was chartered to end: enforced uniformity creates a single point of failure — one medium of coordination, one center of command — whose collapse is precisely the scattering the charter feared. The constraint produces its own failure condition and then reads the need for repair as vindication of the charter. Keeping the coordination and extraction readings distinct prevents two opposite errors: reading the genuine coordination function (a mutually unintelligible population made able to act together) as pure extraction, and reading the extraction (homogenization, concentrated gain) as the necessary price of coordination. The classification holds both at once: the arrangement coordinates and extracts through the same structure, with active enforcement doing the holding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the babel_reading of the human_transcendence_pathway kernel; if the jerusalem_reading or the technocratic_vs_incarnational_reading were instantiated over the same standing arrangement, which structural facts would change, and where exactly do the readings disagree?',
    'Authoring the sibling readings as separate stories over the same referent and comparing per-seat classifications. The located disagreement: whether the unified-power arrangement''s coordination achievement constitutes human flourishing (technocratic side) or its counterfeit (this reading, with jerusalem and the incarnational side), and whether the victim set is those whose languages and cultures are erased (this reading) or all whose communion is foreclosed by the arrangement''s form (jerusalem_reading).',
    'Under the jerusalem_reading the same arrangement loses the coordination-function credit this reading grants it and gains an enlarged victim set; under the technocratic side the measured cost is re-read as the price of a genuine transcendence pathway. Epsilon, directionality, and type could all move — this file''s values are valid only for the babel_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings would change the victim set and the coordination evaluation.').

omega_variable(
    sufficiency_premise_falsifiability,
    'Does the unified system actually deliver the stability and self-sufficiency the charter promises, or does its coordination function degrade precisely when centralized power fails — making the arrangement power-dependent rather than self-sufficient?',
    'Observing coordination performance under stress: center succession, supply disruption, enforcement lapses. If mutual intelligibility of practice and command collapses with the center''s enforcement, the sufficiency premise is empirically falsified; if coordination survives center failure, the charter''s claim stands.',
    'If falsified, the arrangement''s coordination function is contingent on the very power structure doing the extracting, and classification drifts toward pure extraction with coordination as cover; if robust, a larger share of the measured cost is the genuine price of coordination at scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_premise_falsifiability, empirical, 'Whether the charter''s sufficiency promise is delivered or structurally self-undermining.').

omega_variable(
    homogenization_suppression_mechanism,
    'Is the suppression of linguistic and cultural diversity maintained structurally (administrative standard-setting, market and legal gating) or internalized (communities abandoning their own tongues through aspirational identification with the standard)?',
    'Post-enforcement trajectory: where the standard''s enforcement is removed or lapses, do minority-language domains recover (structural suppression) or stay abandoned (internalized suppression)?',
    'If internalized, effective suppression exceeds the structural measure and persists after enforcement ends — the erased communities carry the erasure with them, and the arrangement''s coercive overhead is understated by the scalar.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homogenization_suppression_mechanism, empirical, 'Structural vs internalized mechanism of diversity suppression.').

omega_variable(
    standard_speaker_net_position,
    'Are the standard-language communities structural net beneficiaries, or is their benefit derivative — contingent on complicity in the transfer and revocable by the center?',
    'Welfare comparison across a center-decline episode: if standard speakers'' mobility and standing evaporate with central power, their beneficiary position was derivative of the extraction flow rather than independent of it.',
    'If derivative, the genuine beneficiary set narrows toward the architect seat alone, the arrangement''s coordination dividend is thinner than measured, and the classification drifts toward pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(standard_speaker_net_position, empirical, 'Whether the broad beneficiary class is structural or derivative of the center''s power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__babel_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(babel_reading_tr_t0, human_transcendence_pathway__babel_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(babel_reading_tr_t5, human_transcendence_pathway__babel_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(babel_reading_tr_t10, human_transcendence_pathway__babel_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(babel_reading_tr_t15, human_transcendence_pathway__babel_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement(babel_reading_tr_t20, human_transcendence_pathway__babel_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(babel_reading_tr_t25, human_transcendence_pathway__babel_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(babel_reading_tr_t30, human_transcendence_pathway__babel_reading, theater_ratio, 30, 0.46).

% Extraction over time
narrative_ontology:measurement(babel_reading_be_t0, human_transcendence_pathway__babel_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(babel_reading_be_t5, human_transcendence_pathway__babel_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(babel_reading_be_t10, human_transcendence_pathway__babel_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(babel_reading_be_t15, human_transcendence_pathway__babel_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement(babel_reading_be_t20, human_transcendence_pathway__babel_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(babel_reading_be_t25, human_transcendence_pathway__babel_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(babel_reading_be_t30, human_transcendence_pathway__babel_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(babel_reading_su_t0, human_transcendence_pathway__babel_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(babel_reading_su_t5, human_transcendence_pathway__babel_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(babel_reading_su_t10, human_transcendence_pathway__babel_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(babel_reading_su_t15, human_transcendence_pathway__babel_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(babel_reading_su_t20, human_transcendence_pathway__babel_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(babel_reading_su_t25, human_transcendence_pathway__babel_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(babel_reading_su_t30, human_transcendence_pathway__babel_reading, suppression_requirement, 30, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__babel_reading, information_standard).
narrative_ontology:affects_constraint(human_transcendence_pathway__babel_reading, human_transcendence_pathway__jerusalem_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__babel_reading, human_transcendence_pathway__technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% The human_transcendence_pathway kernel decomposes into three readings per the epsilon-invariance principle: this babel_reading (unified power as coercive homogenization; high epsilon, tangled_rope), the jerusalem_reading (participatory communion under divine blessing; plurality integrated rather than erased), and the technocratic_vs_incarnational_reading (optimization without limits versus grace in vulnerability). The readings share the kernel's referent — the standing question of human self-sufficiency — and diverge on the victim set and on the evaluation of the coordination function. Each is a separate story with its own epsilon; they are linked here because the babel reading functions both as the template the technocratic claim re-instantiates and as the standing critique both siblings must answer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
