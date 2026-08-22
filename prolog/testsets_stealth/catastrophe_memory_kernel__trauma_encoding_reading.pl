% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Mourning-Ritual Trauma Transmission as Intergenerational Warning System (Trauma-Encoding Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A post-catastrophe minority community maintains an annual mourning
 *   calendar — fasts, memorials, liturgies, and school curricula — that
 *   transmits the catastrophe to each new cohort in embodied, affectively
 *   intense form. This story instantiates the trauma_encoding_reading of the
 *   catastrophe_memory_kernel: the practice is read as a mechanism that
 *   encodes intergenerational trauma and deploys it as a collective warning
 *   system. The beneficiary is the community's threat-vigilance capacity (an
 *   early-warning function administered through custodial institutions); the
 *   victim is the descendant generation, which inherits the psychological
 *   burden without having consented to it. The epsilon referent is the
 *   standing mourning-transmission arrangement as this reading assesses it —
 *   NOT the rights-respecting or therapeutically processed alternative this
 *   reading might prefer. The three sibling readings (symbol_continuity,
 *   survival_competence, boundary_maintenance) are separate constraints in
 *   separate files, linked through network.affects_constraints; their epsilon
 *   values differ and are not averaged here.
 *
 * KEY AGENTS:
 *   - communal_memory_leadership: Primary agenda-setter (institutional/identity_locked) — administers the transmission, collects authority and livelihood from it
 *   - diaspora_communal_institutions: Secondary beneficiary (organized/constrained) — receives cohesion, mobilization capacity, and funding from the commemorative cycle
 *   - descendant_generations: Primary target (moderate/identity_locked) — bears the inherited psychological burden; consent was structurally impossible
 *   - assimilation_oriented_members: Sanctioned interior dissent (moderate/mobile) — prefers lighter remembrance; their preference is delegitimized rather than argued
 *   - host_society_neighbors: Monitored outsider (institutional/arbitrage) — the object of the transmitted vigilance, not a party to it
 *   - trauma_research_clinicians: Analytical observer (institutional/analytical) — documents the burden side of the ledger from outside the practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.64).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.58).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Mourning-Ritual Trauma Transmission as Intergenerational Warning System (Trauma-Encoding Reading)").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, '05e3194d-e2bc-482f-8ab1-c119455ca62f').
narrative_ontology:cs_kernel_codification('05e3194d-e2bc-482f-8ab1-c119455ca62f', fixed_text).
narrative_ontology:cs_authority_grounding('05e3194d-e2bc-482f-8ab1-c119455ca62f', lineage).
narrative_ontology:cs_interpretation_layer_present('05e3194d-e2bc-482f-8ab1-c119455ca62f').
narrative_ontology:cs_reading_relation('05e3194d-e2bc-482f-8ab1-c119455ca62f', catastrophe_memory_kernel__symbol_continuity_reading, influences).
narrative_ontology:cs_reading_relation('05e3194d-e2bc-482f-8ab1-c119455ca62f', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('05e3194d-e2bc-482f-8ab1-c119455ca62f', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('05e3194d-e2bc-482f-8ab1-c119455ca62f', foundational, catastrophe_memory_imposes_transmission_duty).
narrative_ontology:cs_axiom_status(catastrophe_memory_imposes_transmission_duty, holdable).
narrative_ontology:cs_axiom_grounding('05e3194d-e2bc-482f-8ab1-c119455ca62f', catastrophe_memory_imposes_transmission_duty, deontological).
narrative_ontology:cs_axiom('05e3194d-e2bc-482f-8ab1-c119455ca62f', foundational, descendant_burden_justified_by_threat_persistence).
narrative_ontology:cs_axiom_status(descendant_burden_justified_by_threat_persistence, holdable).
narrative_ontology:cs_axiom_grounding('05e3194d-e2bc-482f-8ab1-c119455ca62f', descendant_burden_justified_by_threat_persistence, instrumental).
narrative_ontology:cs_reference_frame('05e3194d-e2bc-482f-8ab1-c119455ca62f', inherited_warning_obligation_framework).
narrative_ontology:cs_drift_state('05e3194d-e2bc-482f-8ab1-c119455ca62f', post_trauma_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('05e3194d-e2bc-482f-8ab1-c119455ca62f', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, communal_memory_leadership).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, diaspora_communal_institutions).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Clergy, educators, and lay historians who own the commemorative calendar: they set the annual cycle of fasts and memorials, write the curricula, and decide which catastrophes are taught and how vividly. Their standing, employment, and moral authority rest on being custodians of what the community must not forget. Stepping back from the custodial role would cost them the identity and livelihood built around it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, communal_memory_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Schools, congregations, and mutual-aid bodies that run on the commemorative cycle: it fills their halls, anchors their fundraising appeals, and gives members a reason to affiliate across generational lines. When threat-talk sharpens, attendance and donations follow. Abandoning the cycle would mean reinventing their offer to members from scratch.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, diaspora_communal_institutions, beneficiary,
    organized, generational, constrained, global).

% Children and grandchildren raised inside the calendar: they learn the catastrophe before they read fluently, inherit grief and watchfulness they did not choose, and carry anxiety scripts into adulthood. Opting out means disappointing parents, losing standing among peers, and in the strong case severing family ties — the practice is fused with who they understand themselves to be.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations, payer,
    moderate, biographical, identity_locked, global).

% Members drawn toward lighter remembrance — secular history, therapy, private family memory — who find their preference treated as ingratitude or drift. They rarely argue the point publicly; they quietly reduce attendance, and their absence is read as confirmation that vigilance must be tightened.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, assimilation_oriented_members, excluded,
    moderate, biographical, mobile, national).

% The majority society whose conduct the transmitted watchfulness monitors. They are not party to the calendar and are mostly unaware of the scripts it writes about them; if consulted, they would object to being inherited as a standing menace. Ignoring the entire apparatus costs them nothing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, host_society_neighbors, excluded,
    institutional, generational, arbitrage, national).

% Researchers and clinicians who study intergenerational transmission of trauma. They document elevated hypervigilance, anxiety, and identity constriction in descendant cohorts, and increasingly question whether the protective yield justifies the load. They observe and publish; they neither run nor attend the calendar.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, trauma_research_clinicians, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__trauma_encoding_reading, communal_memory_leadership).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__trauma_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a distributed, cross-generational early-warning capacity: catastrophe knowledge, threat cues, and mobilization templates are stored in ritual form and refreshed on an annual cycle, so each cohort inherits recognition of danger signs without needing direct experience of the catastrophe.
% TRANSFER_FUNCTION: Moves remembered threat and its emotional charge from elder generations and institutional custodians to children and new members — via calendar obligations, liturgy, and schooling — together with the psychological load (grief, fear, vigilance) attached to the memory. It also moves status and definitional authority to the custodians who administer the transmission.
% ABSENT_VOICES: Descendants too young or unborn to consent — the seat that bears the burden has no seat at the table where the transmission regime is designed and renewed. Assimilation-inclined members are present but silenced: their preference for lighter remembrance is delegitimized as betrayal before it is argued. Host-society neighbors, pre-classified by the transmitted scripts as latent threat, are entirely outside the conversation.
% DISAPPEARANCE_RATIONALE: If the transmission regime vanished overnight, detailed catastrophe memory would thin within a generation to history-book knowledge; communal mobilization templates and mutual-aid reflexes primed by the calendar would atrophy; custodial authority would lose its foundation; and descendants would be spared the inherited load. The community's arrangements — schooling, fundraising, identity formation, threat-recognition — visibly depend on the practice continuing.
% FOUNDING_PROBLEM: After catastrophe, ensure the group is never again caught unprepared: preserve actionable memory of how the disaster unfolded, which signals were missed, who ignored them, and how to move early next time.
% FOUNDING_PROBLEM_CORROBORATION: Custodial leadership attests the problem is live, citing recurrent hostility and new threat environments. Independent corroboration cuts both ways: historians document the deliberate post-catastrophe design of commemoration as instruction (confirming the founding problem was real), while trauma researchers and descendant testimony gathered outside custodial institutions attest that the threat environment has transformed and that the protective yield no longer obviously covers the imposed burden. No party outside the beneficiary set attests simple liveness.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.64 (moderate-to-high): the arrangement imposes real, lifelong psychological costs on unconsenting descendants, but the same structure delivers a genuine protective capacity, so the cost is not pure rent. Suppression is authored at 0.58 as a RAW structural property — it is deliberately NOT scaled by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation. Suppression here is roughly half structural (compulsory schooling in the memory, communal sanction, liturgical obligation) and half internalized (descendants experience forgetting as betrayal; the guilt persists after external barriers are removed — see the suppression_mechanism omega). Theater ratio is low (0.22): the transmission function is real under this reading; only a growing fringe of commemorative activity (anniversary spectacle, heritage performance) is performative rather than functional. Accessibility collapse is moderate-low (0.42): alternatives — therapy, secular history, private family memory, assimilation — remain available but each carries real social and identity cost. Resistance is moderate (0.48): secularization, therapy culture, intermarriage drift, and explicit younger-cohort critique meet the practice continuously without displacing it. The temporal series run on ONE shared seven-point grid (every tracked metric authored at every examined time point). The suppression_requirement series is included because the story specifically tracks enforcement-capacity change: in the founding generation, living testimony made transmission self-enforcing (low enforcement requirement); as living memory faded, the community had to build curricula, obligations, and sanction machinery to hold the transmission steady — a rising enforcement ratchet, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   The custodial seat and the descendant seat should compute differently, and the divergence is structural, not error. From the leadership seat, the arrangement is a sacred duty it administers and a survival infrastructure it built — coordination-dominant, with the burden borne by others. From the descendant seat, the same structure is an unchosen inheritance that fuses identity with grief — extraction-dominant, with the coordinating voice located elsewhere. The clinician observer seat adds a third computation: net-harm accounting in which the protective yield is an open empirical question. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (communal_memory_leadership, diaspora_communal_institutions) derive low directionality — the arrangement subsidizes them: leadership collects authority and livelihood; institutions collect cohesion, attendance, and funding. Declared victims (descendant_generations) derive high directionality, pushed toward the full-target end by their identity_locked exit: the burden is fused with self-concept, so exit is not merely costly but self-dissolving. Diaspora institutions sit slightly above pure-beneficiary because they also fund and staff the transmission, but they remain net collectors. Assimilation-oriented members are sanctioned payers with mobile exit — their mobility is precisely why their dissent stays quiet. Host-society neighbors sit outside the structure entirely (arbitrage): the vigilance monitors them, but the arrangement extracts nothing from them. The vindicated proposition (threat_vigilance_early_warning_doctrine) is listed separately and collects no rents — it is the doctrine the practice's operation vindicates, not a beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters most at the two mislabeling edges. Calling this a snare would erase the genuine coordination function: the early-warning capacity is real, the founding problem was real, and communities with dense commemorative regimes demonstrably retain mobilization templates. Calling it a rope would erase the asymmetric extraction: the seat that bears the cost never consented and cannot exit without self-loss, while the seats that set the agenda collect authority. Tangled rope holds both truths. On the genealogy interview: founding_problem_status is authored 'contested' and disappearance_verdict 'world_rearranges', so the dead-problem-plus-rearrangement mismatch flag does NOT fire — correctly, because the founding problem's liveness is genuinely disputed (live per custodians, superseded per clinicians and descendant testimony) rather than settled-dead. The arrangement is not a piton candidate: theater_ratio is low, the transmission function still operates, and a concentrated seat (leadership) demonstrably captures the gains — piton's no-concentrated-beneficiary test fails.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (trauma_encoding) of the catastrophe_memory_kernel; would instantiating a sibling reading restructure the beneficiary/victim sets and epsilon?',
    'Author the three sibling stories (symbol_continuity, survival_competence, boundary_maintenance) and compare engine classifications across the kernel family; locate the disagreement in what is transmitted (wound vs. form vs. skill vs. boundary).',
    'Under survival_competence_reading the transmitted content is adaptive skill, the victim set thins, and epsilon falls rope-ward; under boundary_maintenance_reading the targets are prospective defectors rather than all descendants; under symbol_continuity_reading the burden is incidental to form-preservation. Each sibling is a different constraint, not a measurement parameter on this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is one of four readings of the catastrophe-memory kernel; sibling readings are separate constraints.').

omega_variable(
    vigilance_efficacy_decay,
    'Does the transmitted vigilance still deliver measurable early-warning value, or has the protective yield decayed while the transmission persists?',
    'Compare threat-response latency, mobilization speed, and mutual-aid activation across communities with dense versus light commemorative regimes, controlling for material security and host-country threat levels.',
    'If the yield has decayed, the arrangement''s costs approach pure unconsented burden and classification drifts snare-ward; if the yield is high, much of the measured extraction is better read as an insurance premium paid by descendants for collective protection, pulling rope-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vigilance_efficacy_decay, empirical, 'Whether the warning function still earns its psychological cost.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (compulsory curricula, communal sanction, liturgical obligation) or internalized (descendants experience forgetting as betrayal and carry the guilt after leaving)?',
    'Post-exit suppression trajectory: track descendants who leave the community — if vigilance, guilt, and catastrophe-anchored anxiety persist after the enforcement mechanism is removed, a substantial share is internalized.',
    'If largely internalized, effective suppression exceeds the structural measure and the burden travels with leavers — widening the victim set beyond current members; if largely structural, remediation via institutional reform becomes tractable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of the enforcement holding the transmission in place.').

omega_variable(
    burden_distribution_concentration,
    'Is the psychological burden uniform across descendant generations, or concentrated in survivor-lineage families and high-observance subgroups?',
    'Epidemiological study of hypervigilance, anxiety, and identity-constriction markers stratified by survivor lineage and observance intensity.',
    'Concentrated burden identifies a narrower, more heavily targeted victim seat and raises per-seat extraction estimates; diffuse burden supports the collective-good framing and moderates the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_distribution_concentration, empirical, 'Distribution of the transmitted burden across the descendant population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t12, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(cata_tr_t36, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 36, 0.18).
narrative_ontology:measurement(cata_tr_t48, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 48, 0.2).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement(cata_tr_t72, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 72, 0.22).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cata_be_t12, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(cata_be_t36, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 36, 0.59).
narrative_ontology:measurement(cata_be_t48, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 48, 0.62).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(cata_be_t72, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 72, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cata_su_t12, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(cata_su_t36, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 36, 0.51).
narrative_ontology:measurement(cata_su_t48, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 48, 0.55).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 60, 0.57).
narrative_ontology:measurement(cata_su_t72, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 72, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'ritual keeps catastrophe memory alive' covers four structurally distinct claims, authored as four stories sharing the catastrophe_memory_kernel. This file authors the trauma-encoding claim (transmitted content = wound-as-alarm; victim = descendants; epsilon 0.64). The upstream, most-established member is symbol_continuity_reading (form-preservation is empirically uncontested); the trauma-encoding reading is downstream and more contested because trauma science directly audits its cost ledger. Edges here record this reading's structural pressure on its siblings: the trauma framing burdens the continuity claim (preserving the vessel preserves the wound) and coexists with the competence and boundary readings as rival-or-complementary accounts held by different factions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
