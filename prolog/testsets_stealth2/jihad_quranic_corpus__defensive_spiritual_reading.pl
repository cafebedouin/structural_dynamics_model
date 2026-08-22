% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__defensive_spiritual_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Defensive-Spiritual Jihad Framework: Nafs Primacy with Bounded, Authorized Defensive Force
 *   domain: religious/political-theological/legal
 *
 * SUMMARY:
 *   This story models the defensive-spiritual construal of the Quranic
 *   struggle corpus as a standing governance arrangement. Under it, striving
 *   is directed first inward — the discipline of the self (jihad al-nafs) is
 *   the primary register — and armed force is legitimate only as a response
 *   to aggression, bounded by proportionality and non-combatant immunity,
 *   authorized by recognized state authority, gated by a deliberately high
 *   threshold for declaration, and embedded in a coexistence framework that
 *   treats religious plurality as the normal condition rather than a
 *   temporary concession. The arrangement is administered by a scholarly
 *   establishment that issues the authorizing and restraining determinations,
 *   operated by states that hold the declaration monopoly, received by lay
 *   populations as a faith discipline, and extended as a protection shield
 *   over religious minorities living under Muslim-majority governance. The
 *   epsilon referent is this standing arrangement as this reading assesses
 *   it: protective flows dominate, and the residual extraction is
 *   concentrated in the authority gate. KEY AGENTS (by structural
 *   relationship): - mainline_juridical_establishment: agenda-setting
 *   interpreter (institutional / identity_locked) — administers the
 *   thresholds and polices the interpretive boundary -
 *   muslim_state_authorities: authorization gateholder (institutional /
 *   constrained) — holds the declaration monopoly, collects its legitimation
 *   gains - general_muslim_populations: primary beneficiary with payer
 *   overlay (organized / identity_locked) — receives the discipline and the
 *   protection, carries the waiting costs - non_muslim_minority_residents:
 *   protected beneficiary (moderate / constrained) — covered by immunity, did
 *   not negotiate the terms - aggrieved_occupied_populations: primary payer
 *   (powerless / trapped) — bears the threshold's delay under attack -
 *   dissident_pulpit_preachers: excluded voice (moderate / constrained) —
 *   barred from the councils that set the threshold -
 *   comparative_religion_law_scholars: analytical observer (institutional /
 *   analytical) — maps the framework against other just-war systems
 *
 * KEY AGENTS:
 *   - mainline_juridical_establishment: agenda-setting interpreter (institutional/identity_locked) — administers thresholds, trains state muftis, polices the authorized/unauthorized boundary
 *   - muslim_state_authorities: authorization gateholder (institutional/constrained) — holds the declaration monopoly; the arrangement's legitimation gains accrue here
 *   - general_muslim_populations: primary beneficiary, secondary payer (organized/identity_locked) — receives discipline and protection; carries waiting costs when attacked
 *   - non_muslim_minority_residents: protected beneficiary (moderate/constrained) — outside the lawful-target set unless they take up arms; did not negotiate the terms
 *   - aggrieved_occupied_populations: primary payer (powerless/trapped) — defense waits on authorization from institutions that may be distant, pressured, or aligned with attackers
 *   - dissident_pulpit_preachers: excluded (moderate/constrained) — argue the threshold locks defenseless populations in place; barred from official councils
 *   - comparative_religion_law_scholars: analytical observer (institutional/analytical) — maps the framework's conditions against other legal systems' rules of force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.26).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.42).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Defensive-Spiritual Jihad Framework: Nafs Primacy with Bounded, Authorized Defensive Force").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "religious/political-theological/legal").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__defensive_spiritual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '48baf1e8-0505-43c0-bc65-c9127d141074').
narrative_ontology:cs_kernel_codification('48baf1e8-0505-43c0-bc65-c9127d141074', fixed_text).
narrative_ontology:cs_authority_grounding('48baf1e8-0505-43c0-bc65-c9127d141074', lineage).
narrative_ontology:cs_interpretation_layer_present('48baf1e8-0505-43c0-bc65-c9127d141074').
narrative_ontology:cs_reading_relation('48baf1e8-0505-43c0-bc65-c9127d141074', jihad_quranic_corpus__expansionist_legalist_reading, forecloses).
narrative_ontology:cs_reading_relation('48baf1e8-0505-43c0-bc65-c9127d141074', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('48baf1e8-0505-43c0-bc65-c9127d141074', foundational, armed_force_defensive_only).
narrative_ontology:cs_axiom_status(armed_force_defensive_only, holdable).
narrative_ontology:cs_axiom_grounding('48baf1e8-0505-43c0-bc65-c9127d141074', armed_force_defensive_only, theological).
narrative_ontology:cs_axiom('48baf1e8-0505-43c0-bc65-c9127d141074', foundational, state_authority_gates_legitimate_force).
narrative_ontology:cs_axiom_status(state_authority_gates_legitimate_force, holdable).
narrative_ontology:cs_axiom_grounding('48baf1e8-0505-43c0-bc65-c9127d141074', state_authority_gates_legitimate_force, conventional).
narrative_ontology:cs_axiom('48baf1e8-0505-43c0-bc65-c9127d141074', secondary, non_combatant_immunity_universal).
narrative_ontology:cs_axiom_status(non_combatant_immunity_universal, holdable).
narrative_ontology:cs_axiom_grounding('48baf1e8-0505-43c0-bc65-c9127d141074', non_combatant_immunity_universal, deontological).
narrative_ontology:cs_reference_frame('48baf1e8-0505-43c0-bc65-c9127d141074', medinan_defensive_paradigm).
narrative_ontology:cs_drift_state('48baf1e8-0505-43c0-bc65-c9127d141074', contemporary_postcolonial_nationstate_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('48baf1e8-0505-43c0-bc65-c9127d141074', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, general_muslim_populations).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_minority_residents).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_state_authorities).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, aggrieved_occupied_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, general_muslim_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholarly bodies and their academies issue the determinations that specify when the defensive conditions are met, train the jurists who staff state mufti offices, and maintain the interpretive boundary between authorized and unauthorized force. Their standing rests on transmission credentials and continuity with the classical schools; stepping outside the framework they administer would dissolve the basis of their own authority, so exit is not a practical option for them as institutions. They are bound to the arrangement by what they are, not only by what they gain.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, mainline_juridical_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Governments hold the formal power to authorize armed response, decide when the defensive threshold is satisfied, and criminalize military mobilization they have not licensed. The arrangement hands them a monopoly over legitimate force that they use to manage both external defense and internal dissent, and the arrangement's operational gains — control over the most consequential authorization a polity can issue — accrue to this seat. Abandoning the framework would strip them of the gate and expose them to rivals claiming its mantle, so they operate inside it while shaping its application to their interests.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_state_authorities, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, muslim_state_authorities, beneficiary).

% Lay believers receive a discipline that directs striving first inward toward ethical self-governance and permits outward force only under strict conditions, sparing their communities cycles of unauthorized bloodshed and reprisal. They carry the arrangement as faith identity — leaving it carries apostasy-level social and legal cost — and when their communities are attacked they additionally carry the waiting cost of a high threshold before authorized response arrives. Most of what the arrangement produces flows to them; a slice of its burden does too.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, general_muslim_populations, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, general_muslim_populations, payer).

% Religious minorities living under Muslim-majority governance are covered by the framework's non-combatant immunity and treaty protections: the arrangement places them outside the set of lawful targets unless they take up arms. They did not negotiate these terms and their protection depends on the framework continuing to bind the community around them, but within it they hold enforceable claims to security of life, property, and worship.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_minority_residents, beneficiary,
    moderate, biographical, constrained, national).

% Communities under occupation or persecution bear the arrangement's sharpest running cost: the high declaration threshold and the state-authority gate mean their defense waits on authorization from institutions that may be distant, pressured, or aligned with their attackers. Private recourse is condemned and punished, so their realistic options are endurance, flight, or the inward discipline the framework prescribes while they wait. Nothing about their situation lets them exit the arrangement's costs; they can only await its benefits.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, aggrieved_occupied_populations, payer,
    powerless, biographical, trapped, regional).

% Preachers and activists who argue the threshold is being applied to lock defenseless populations in place are barred from official pulpits and authorization councils in many polities; their arguments circulate informally or from abroad. They would press the councils to lower the threshold for cases of ongoing occupation and to relax the state gate where the state is absent or compromised. Their exclusion from the bodies that set the threshold is maintained by the same gatekeeping machinery the arrangement runs on.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, dissident_pulpit_preachers, excluded,
    moderate, biographical, constrained, regional).

% Academic specialists in Islamic law and comparative just-war traditions map how the framework's conditions — defense, proportionality, immunity, authority — line up against other legal systems' rules of force, from international humanitarian law to historical Christian just-war doctrine. They take no side in authorization, collect nothing from the arrangement's operation, and their analyses feed courts, ministries, and interfaith bodies that handle its application.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, comparative_religion_law_scholars, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__defensive_spiritual_reading, muslim_state_authorities).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__defensive_spiritual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of violence in a religiously plural world: it fixes who may authorize force, against whom force may be directed, and under what limits it must stop — preventing unauthorized bloodshed, protecting non-combatant life across confessional lines, and giving the community a shared discipline connecting inner ethics to outer conduct.
% TRANSFER_FUNCTION: Moves interpretive authority and force-legitimation upward, concentrating them in the juridical establishment and the state gateholders; moves waiting costs onto aggrieved populations whose defense awaits authorization; and distributes immunity protections outward to non-combatants, including non-Muslim minorities who paid nothing into the framework.
% ABSENT_VOICES: Aggrieved populations under occupation were never consulted on the threshold's height; non-Muslim minorities live under immunity terms they did not negotiate; women are largely absent from the authorization councils; lay believers receive the framework as settled doctrine rather than as a negotiated settlement. Dissident preachers who contest the threshold exist but are kept outside the councils that set it.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, force-authorization within Muslim communities would reorganize immediately: either toward unrestrained private and factional recourse, or toward an unqualified state monopoly with no doctrinal limit short of state interest. Minority-protection guarantees would lose their doctrinal anchor and become matters of bare state discretion, and the coexistence settlements built on the immunity and treaty rules would need renegotiation from zero. Every named seat's situation depends on the arrangement's continuation.
% FOUNDING_PROBLEM: The early community faced expulsion, persecution, and armed aggression and needed rules distinguishing legitimate defense from vendetta and from imperial conquest: when may the community fight, whom may it kill, when must it stop, and who decides.
% FOUNDING_PROBLEM_CORROBORATION: The problem's persistence is corroborated from outside the benefiting parties: the UN Charter's self-defense provisions attest that authorization-and-limit questions remain live for all polities; international humanitarian law's distinction and proportionality principles attest the same limits were independently rediscovered as necessary; and secular historians of the early Islamic period attest the founding aggression context without reliance on the tradition's own testimony. What no outside source corroborates is this reading's particular answers — the specific threshold height and the state-gate requirement are attested mainly by the tradition's own juristic apparatus, and that asymmetry is itself signal.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).
:- end_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim is rope because the arrangement's dominant structure is genuine coordination: it solves the collective-action problem of violence (who may authorize force, against whom, under what limits), its participants are net beneficiaries, and its coercive overhead, while real, is secondary to its protective function. The metrics describe actual operation. Extractiveness is low but nonzero (0.26): the authority gate converts into legitimation rents for gateholding states, and the high threshold shifts waiting costs onto aggrieved populations, but no seat collects anything approaching the arrangement's total value flow — most of what it produces (immunity, restraint discipline, coexistence stability) is consumed broadly. Suppression (0.42) is a raw structural property, unscaled: roughly sixty percent structural (criminal statutes against unauthorized mobilization, pulpit and council gatekeeping) and forty percent internalized (believers' self-restraint as piety, doctrinal self-policing that persists where enforcement is absent). Theater ratio (0.24) reflects ceremonial invocations of the spiritual register in official pageancy that outrun substantive teaching, rising with state co-optation mid-interval and receding as independent scholarship re-substantivized the framework. Accessibility collapse is low (0.32): understanding this framework does not close off alternatives — other construals of the same corpus, secular just-war frameworks, and pacifism all remain live and reachable, which is itself evidence the arrangement is not suppressing its competitors. Resistance is moderate-to-high (0.52): dissident preachers contest the threshold's height, armed movements simply bypass the gate, and secular critics reject the religious framing of force altogether. The measurement series run on one shared grid (interval units approximate years since 1925): extraction, theater, and suppression all peaked mid-interval when post-colonial states nationalized religious institutions and turned the gate into an instrument of internal control, then partially receded as transnational scholarly consensus documents and independent media re-substantivized the framework. The trajectories are monotonic-with-a-peak rather than cyclical; the peak, not the oscillation, is the analytically salient feature.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the aggrieved_occupied_populations seat, the arrangement operates as abandonment dressed as discipline: the threshold is experienced as a wall placed between a population and its defense, and the state gate as a lock for which attackers hold the key. From the non_muslim_minority_residents seat, the same structure operates as a shield — the immunity rules are the difference between secure residence and exposure. From the general_muslim_populations seat, it is a faith discipline that spares their communities reprisal cycles. From the mainline_juridical_establishment seat, it is stewardship: the careful maintenance of conditions under which force stays rare and bounded. From the muslim_state_authorities seat, it is an instrument — a monopoly over the most consequential word a government can say. The engine derives these divergent per-seat classifications from the power, exit, and directional data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation. general_muslim_populations and non_muslim_minority_residents sit near the beneficiary end (low d): the arrangement subsidizes them with protection and discipline, and their costs are diffuse. muslim_state_authorities derive a low-to-moderate d from their beneficiary role, but their agenda-setter position and constrained exit keep them structurally engaged rather than passively subsidized — they are the seat the gains demonstrably accrue to, which is why gain_flow names them. aggrieved_occupied_populations derive a high d: they bear the arrangement's sharpest costs, and their trapped exit (no authorized recourse, private recourse condemned and punished, flight costly) pushes them toward the full-target end of the spectrum. The observer seat is analytical and feeds no extraction arithmetic. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rules distinguishing legitimate defense from vendetta and conquest for a community facing existential aggression — remains live: occupations, persecutions, and cross-border attacks continue, and the questions of authorization and limit are as operative as they were at the corpus's formation. No mandatrophy is declared. The classification discipline guards against two opposite errors. Reading the arrangement's coercive elements (the gate, the criminalization of unauthorized force) as pure extraction ignores the coordination function that protects minorities and spares communities reprisal cycles — that error would misclassify a working rope as a snare. Reading the protective rhetoric as proof of purity ignores the gate's documented capture episodes — that error would miss the drift the temporal series records. The measurements exist precisely to catch the second error if it matures: a sustained rise in base_extractiveness alongside rising theater would signal the transition the mid-interval peak prefigures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading (defensive_spiritual_reading) of the kernel jihad_quranic_corpus; the sibling readings expansionist_legalist_reading and revolutionary_vanguard_reading instantiate different constraints. Where exactly is the disagreement located, and what would sibling adoption change structurally?',
    'Comparative analysis across the three reading-files: the disagreement sits on three structural axes — the initiation rule (defensive-only vs. offensive mandate), the authority rule (state gate vs. individual obligation bypassing the state), and the status of the spiritual-primacy texts (literal vs. subordinate). Each axis changes the victim set and epsilon of whichever reading adopts it.',
    'Under the expansionist sibling, non-Muslim polities enter the target set and epsilon rises sharply; under the vanguard sibling, sitting rulers and rival Muslims enter the target set and the authority gate dissolves. This file''s low epsilon and narrow victim set hold only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one-of-three readings of a shared fixed text; disagreement located on initiation, authority, and spiritual-primacy axes.').

omega_variable(
    abrogation_direction_dispute,
    'Do the later martial verses (traditionally cited as 9:5, 9:29) abrogate the earlier restraint and defensive verses (2:190, 22:39-40), or do the defensive verses control the corpus''s operative meaning?',
    'Philological and jurisprudential analysis: hadith chronology, matn coherence with the Meccan/Medinan contexts, and the formation history of madhhab consensus on the abrogation ordering.',
    'If abrogation runs toward the sword verses, this reading''s foundational defensive axiom loses its textual anchor, non-Muslim polities enter the victim set, epsilon rises dramatically, and the classification trends toward enforced asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_direction_dispute, empirical, 'Textual-ordering question that determines whether the defensive axiom is anchored or overridden within the tradition''s own tools.').

omega_variable(
    greater_jihad_hadith_authenticity,
    'Is the hadith in which the returning combatant reports moving from the lesser jihad to the greater jihad of the self (jihad al-nafs) authentically transmitted, given that classical hadith critics graded its chain weak?',
    'Isnad reconstruction and matn analysis within the tradition''s own authentication sciences, plus survey of which juristic schools admitted it into operative doctrine despite grading disputes.',
    'If inauthentic, the spiritual-primacy pillar rests solely on the Qur''anic restraint verses and ethical material — thinner but not collapsed; the reading survives with a narrower foundation and somewhat higher vulnerability to the abrogation dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(greater_jihad_hadith_authenticity, empirical, 'Authenticity status of the textual anchor for the nafs-primacy pillar.').

omega_variable(
    state_gate_capture_vs_safeguard,
    'Does the state-authority gate operate as a safeguard against false declarations of defensive war, or as a capture mechanism by which rulers suppress legitimate defense of populations under attack?',
    'Comparative study of authorization fatwa patterns across regime types: whether declarations track aggression facts or ruler interests, and whether denial rates correlate with regime accountability rather than threat level.',
    'If capture dominates in a polity, effective extraction there rises well above the authored base, the payer seat''s burden intensifies, and the local classification trends toward hybrid coordination-extraction; if the safeguard reading holds, the gate is functioning coordination infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_gate_capture_vs_safeguard, empirical, 'Whether the authority gate is protective infrastructure or ruler-capture surface.').

omega_variable(
    immunity_scope_universal_vs_treaty_bound,
    'Does non-combatant immunity under this reading extend universally to all civilians, as this reading asserts, or does it remain bounded by classical fiqh categories (treaty peoples, protected residents, enemy subjects)?',
    'Doctrinal analysis of how contemporary fatwa councils and academic jurists apply the immunity rule to persons outside the classical legal categories, versus how classical manuals scope it.',
    'If immunity is category-bound, persons outside the recognized categories fall outside the protected set, widening the effective victim set and raising epsilon; if universalized, the coexistence framework''s protections hold as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunity_scope_universal_vs_treaty_bound, conceptual, 'Boundary of the protected class: universal civilian immunity vs. category-scoped classical immunity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jihad_defensive_spiritual_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jihad_defensive_spiritual_tr_t20, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(jihad_defensive_spiritual_tr_t40, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(jihad_defensive_spiritual_tr_t60, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(jihad_defensive_spiritual_tr_t80, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement(jihad_defensive_spiritual_tr_t100, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 100, 0.24).

% Extraction over time
narrative_ontology:measurement(jihad_defensive_spiritual_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(jihad_defensive_spiritual_be_t20, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(jihad_defensive_spiritual_be_t40, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(jihad_defensive_spiritual_be_t60, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(jihad_defensive_spiritual_be_t80, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 80, 0.31).
narrative_ontology:measurement(jihad_defensive_spiritual_be_t100, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 100, 0.26).

% Suppression requirement over time
narrative_ontology:measurement(jihad_defensive_spiritual_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(jihad_defensive_spiritual_su_t20, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(jihad_defensive_spiritual_su_t40, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(jihad_defensive_spiritual_su_t60, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(jihad_defensive_spiritual_su_t80, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 80, 0.49).
narrative_ontology:measurement(jihad_defensive_spiritual_su_t100, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'jihad' covers three structurally distinct claims — this defensive-spiritual reading (low extraction, narrow victim set, state-gated), the expansionist_legalist_reading (offensive mandate under jurisprudential conditions, non-Muslim polities in the target set), and the revolutionary_vanguard_reading (individual obligation bypassing state authority via takfir, rulers and rival Muslims in the target set). Each is a separate story with its own epsilon, stakeholders, and classification; all three cite the same fixed text with different abrogation orderings and authority theories, so the shared kernel anchors the family and each member links the others here. The upstream member by empirical confidence is this reading (operative in the majority of contemporary institutional Islam); the siblings are downstream contestations that cite the same verses against it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
