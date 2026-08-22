% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Sacral Fidelity Reading of the Lycurgan Settlement
 *   domain: political philosophy/constitutional theory/commitment systems
 *
 * SUMMARY:
 *   This file instantiates the sacral_fidelity_reading of the lycurgan_laws
 *   kernel: the Spartan founding settlement as a Delphically ratified,
 *   changeless divine ordinance commanding absolute adherence, with zero
 *   revision capacity treated as a perfection rather than a defect. The
 *   referent of every authored value is the standing arrangement under
 *   contest — the Lycurgan settlement as it actually operated from the
 *   traditional promulgation to the Roman absorption — never the arrangement
 *   any reading would put in its place. The claim and the metrics are
 *   independent authored facts: the claimed mountain is what this reading
 *   asserts (divine ordinance, natural-law-like immutability), while the
 *   metrics describe the arrangement's operation as this reading's own
 *   tradition can observe it, including the decay its own great transmitter
 *   (Plutarch) laments. Declared beneficiaries are intentional false-summit
 *   authoring: the domain presents the immutability as natural law while
 *   identifiable guardians (the council of elders, the royal houses) collect
 *   authority rents from its unalterability, and the omega
 *   'natural_law_vs_constructed_ordinance' documents the required naturalness
 *   ambiguity. Calendar mapping for the interval is approximate: t=0 is the
 *   traditional promulgation (c. 800 BC), t=120 the consolidated subjugation
 *   of Messenia (c. 680 BC), t=240 the rise of Spartan hegemony (c. 560 BC),
 *   t=360 the aftermath of the great earthquake and helot revolt (c. 440 BC),
 *   t=480 the post-Leuctra collapse (c. 320 BC), t=600 the Roman-dominated
 *   ritual city (c. 200 BC).
 *
 * KEY AGENTS:
 *   - - gerousia_elders: Agenda-setting beneficiary (institutional/identity_locked) — administers, interprets, and insures the ordinances; lifetime tenure protected by immutability
 *   - - dual_kingship: Beneficiary (institutional/identity_locked) — collects sacral and military authority from the divine warrant
 *   - - spartiate_homoioi: Dual-positioned beneficiary-payer (organized/trapped) — receives the surplus, pays in total autonomy
 *   - - helot_population: Primary target (powerless/trapped) — bears the productive burden and the enforcement terror
 *   - - ephorate: Enforcement arm (institutional/immediate) — runs day-to-day compulsion on a one-year leash
 *   - - spartan_women: Secondary beneficiary-payer (moderate/constrained) — estate managers and reproducers of the order
 *   - - perioikoi_free_inhabitants: Excluded voice (moderate/constrained) — serves and pays, never deliberates
 *   - - ancient_political_commentators: Analytical observer — sees the full structure from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.3).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.2).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Sacral Fidelity Reading of the Lycurgan Settlement").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political philosophy/constitutional theory/commitment systems").

domain_priors:requires_active_enforcement(lycurgan_laws__sacral_fidelity_reading).
domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, 'a874c252-f108-47b4-808d-85fbe23596a0').
narrative_ontology:cs_kernel_codification('a874c252-f108-47b4-808d-85fbe23596a0', fixed_text).
narrative_ontology:cs_authority_grounding('a874c252-f108-47b4-808d-85fbe23596a0', lineage).
narrative_ontology:cs_interpretation_layer_present('a874c252-f108-47b4-808d-85fbe23596a0').
narrative_ontology:cs_reading_relation('a874c252-f108-47b4-808d-85fbe23596a0', lycurgan_laws__adaptive_fiction_reading, forecloses).
narrative_ontology:cs_reading_relation('a874c252-f108-47b4-808d-85fbe23596a0', lycurgan_laws__demographic_trap_reading, coexists_with).
narrative_ontology:cs_axiom('a874c252-f108-47b4-808d-85fbe23596a0', foundational, rhetra_divinely_ratified_and_changeless).
narrative_ontology:cs_axiom_status(rhetra_divinely_ratified_and_changeless, holdable).
narrative_ontology:cs_axiom_grounding('a874c252-f108-47b4-808d-85fbe23596a0', rhetra_divinely_ratified_and_changeless, theological).
narrative_ontology:cs_axiom('a874c252-f108-47b4-808d-85fbe23596a0', foundational, absolute_adherence_supreme_civic_duty).
narrative_ontology:cs_axiom_status(absolute_adherence_supreme_civic_duty, holdable).
narrative_ontology:cs_axiom_grounding('a874c252-f108-47b4-808d-85fbe23596a0', absolute_adherence_supreme_civic_duty, deontological).
narrative_ontology:cs_reference_frame('a874c252-f108-47b4-808d-85fbe23596a0', delphic_ratified_immutable_rhetra).
narrative_ontology:cs_drift_state('a874c252-f108-47b4-808d-85fbe23596a0', post_classical_spartan_decline, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a874c252-f108-47b4-808d-85fbe23596a0', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, gerousia_elders).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, dual_kingship).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartiate_homoioi).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, helot_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartan_women).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, spartiate_homoioi).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, spartan_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Twenty-eight men over sixty, chosen for life, who prepare business for the assembly, judge capital cases, and guard the founder's ordinances against alteration. Membership ends only at death; a man elevated to the council has spent his whole adult life forming others in the customs he now guards, and turning against them would unmake his own life's work. When circumstances raise new questions, they decide what the ordinances already require.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, gerousia_elders, agenda_setter,
    institutional, generational, identity_locked, national).

% Two hereditary royal houses whose authority rests on descent from the founder's line and on religious office: they lead the army, preside over the chief sacrifices, hold seats in the council, and receive places of honor at the messes. Their position is fixed at birth, renouncing it is not a live option, and their legitimacy depends wholly on the founder's settlement remaining unaltered.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, dual_kingship, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, dual_kingship, agenda_setter).

% Full citizens who eat at common messes financed by allotments worked for them by an unfree population, train together from boyhood, and hold equal rank with one another. They contribute fixed produce quotas, serve in the army from youth to old age, submit to communal discipline, and may not travel abroad freely or accumulate displayable wealth. Falling out of the mess or missing contribution strips citizenship; leaving the order means losing everything that defines them.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartiate_homoioi, beneficiary,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, spartiate_homoioi, payer).

% Unfree farm workers tied to Spartan allotments in Laconia and Messenia who surrender roughly half of each harvest to their masters and perform nearly all productive labor. They are subject to annual declared war, summary killing, staged humiliation of their elders to terrify the young, and expulsion by lot whenever the magistrates deem the numbers inconvenient. Flight offers only slave-catchers beyond the border; their children inherit the same station.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, helot_population, payer,
    powerless, generational, trapped, national).

% Five annual magistrates elected from the citizen body who oversee the kings, can convene proceedings against them, run the periodic sweeps against foreigners and suspect residents, and preside over the assembly. They serve a single year and answer for it afterward; their office exists to keep the settlement running exactly as received, day to day.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, ephorate, agenda_setter,
    institutional, immediate, constrained, national).

% Wives and daughters of citizens who manage farms and households while the men live in barracks and messes, own and inherit land to a degree unusual in Greece, and rear the next generation of soldiers under intense pressure to produce sons who survive the inspection of infancy. They hold no vote and no office and carry the settlement's reproductive demands.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_women, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, spartan_women, payer).

% Free inhabitants of the towns scattered through Laconia and Messenia who trade, manufacture, pay dues, follow Spartan foreign policy, and serve as hoplites in the army, yet stand outside the citizen body, the messes, and every deliberative chamber. They would contest their permanent second-class standing if they had anywhere to press the case.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, perioikoi_free_inhabitants, excluded,
    moderate, biographical, constrained, national).

% Writers from Xenophon and Aristotle through Plutarch who examine the Spartan settlement from outside its institutions: praising its stability and cohesion, cataloguing its costs, and transmitting the founder tradition in forms later ages argue over. They hold no stake in the arrangement and can set it beside every other Greek constitution.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, ancient_political_commentators, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__sacral_fidelity_reading, spartiate_homoioi).
narrative_ontology:fixing_cost_class(lycurgan_laws__sacral_fidelity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a small citizen body's cohesion and military readiness against a vastly larger subject population and against internal faction: common messes, uniform education, fixed land allotments, and lifetime offices give every male citizen an identically formed life and remove the levers of private ambition.
% TRANSFER_FUNCTION: Moves the agricultural product of the land, worked by unfree laborers, to the citizen messes; moves decision authority upward to the council of elders, the magistrates, and the kings; moves individual autonomy from every citizen into the common discipline.
% ABSENT_VOICES: The unfree farm workers of Messenia and Laconia have no assembly, no voice, and no standing to object; the free townspeople of Laconia fight in the army but sit outside every deliberative body; dissenting citizens exit only into silence or exile; and unborn generations are bound by a constitution they have no power to amend.
% DISAPPEARANCE_RATIONALE: Without the requirement of absolute adherence to the founder's ordinances, the common messes lose their compulsory basis, allotments fragment through inheritance and sale, the citizen levy dissolves into an ordinary farmer militia, and the minority citizen body loses its grip on the subject population — the entire Lacedaemonian order rearranges around ordinary Greek civic life.
% FOUNDING_PROBLEM: Civil strife among the Spartans in the generations before the traditional lawgiving, joined to the problem of how a tiny citizen minority could hold and govern a much larger conquered population without either tyranny or collapse.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting seats: Tyrtaeus's elegies, a contemporary poetic witness, attest the civil-strife setting and the conquest settlement; Aristotle (Politics II) attests both the design intent and its costs while writing from outside the beneficiary set; Plutarch preserves the tradition while recording variant accounts. No attesting source stands wholly outside the tradition's reach, but Tyrtaeus predates the sacral codification and supplies the strongest independent corroboration.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__sacral_fidelity_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored low (0.30 at interval end) because this reading assesses the arrangement as gift rather than taking: adherence is piety, the burdens are divinely ordered duties. Even within the frame, the operated arrangement drifts upward as land concentrates and wealth creeps in — the reading's own reformers (Agis, Cleomenes) premised their programs on exactly that decay. Theater rises monotonically from 0.10 to 0.48: early rites are lived worship; by the Hellenistic and Roman phases the Lycurgan repertoire persists as heritage performance for philhellenic visitors, a hollowness the reading's own literature confesses. Suppression_requirement is authored because this story specifically tracks enforcement-capacity change, and its arc is non-monotonic by design: a ratchet from 0.45 to 0.70 as the machinery hardens after the 464 BC earthquake and the third Messenian revolt (expanded crypteia, systematic expulsions), then decay to 0.20 as Messenia is lost, the citizen body evaporates, and the enforcement offices empty. All three series share one six-point grid; no metric borrows another's end-state. Accessibility_collapse is high (0.85) because inside the sacral frame no alternative is conceivable — questioning the ordinances is impiety, not policy preference — and resistance is low (0.15) because open dissent exits the frame as vice or madness; the helot revolts that did occur were read as brigandage, not as objection. Suppression is authored as a raw structural property and is deliberately unscaled; only extractiveness is scaled by directionality and spatial scope in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the council of elders and the royal houses the arrangement is a sacred trust they have spent their lives administering — mountain-like from those chairs, with identity-locked exit making the frame constitutive of self rather than a chosen position. From the helot seat the same structure is pure imposition with no exit and no voice. The citizen body sits between: genuine recipient of the surplus and genuine bearer of lifelong discipline, which is why its seat is dual-positioned rather than cleanly either. The ephors' one-year horizon makes them enforcers rather than believers — they administer a frame they will shortly leave, unlike the elders who die inside it. The commentators see the whole shape and can compare it against every other Greek constitution, which no participant seat can. The helot revolts are the standing reminder that the powerless seat retained coalition potential — the enforcement ratchet exists precisely because that coalition twice nearly broke the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations place the elders, the royal houses, and the citizen body near the subsidized end of the directionality range; the victim declaration places the helot population near the full-target end, amplified by trapped exit and generational entanglement. The material surplus demonstrably lands on the citizen messes, so the receipt surface names spartiate_homoioi rather than diffuse; the authority rents land separately on the elders and kings, which is a benefit-flow fact the receipt field does not need to duplicate. Women are genuinely dual-positioned (estate management gained, reproductive demand borne) and the derivation reads them through their primary declaration. No directionality overrides are authored: the beneficiary/victim data plus exit options already produce the right relationships, and the override mechanism keys on power atoms too coarse to separate the three institutional seats (elders, kings, ephors) whose differences the per-seat computation should surface on its own.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification apparatus guards against two opposite errors. Accepting the sacral claim uncritically would certify the arrangement as immune from analysis — a mountain needs no justification and admits no victims — which is exactly what the immutability doctrine was for; the declared beneficiaries route the story through false-summit evaluation instead. Dismissing the coordination content entirely would mislabel a working identity-coordination machine (common messes, uniform formation, boundary maintenance of citizen status) as pure taking; the identity_coordination typing keeps the genuine function on the books while the conservative floor ensures the relational framing cannot excuse the asymmetric burden. On obsolescence: the reading declares the founding problem (civil strife, minority rule over a subject majority) permanently live, since human nature does not expire; the mismatch consumer watches that declaration against the world_rearranges verdict and against the theater trajectory, which shows the maintenance becoming performance long before the polis itself ends. No mandatrophy_resolved flag is authored — the mandate and the arrangement decay together rather than one outliving the other cleanly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ordinance,
    'Is the requirement of absolute adherence to unalterable ordinances a genuine natural or divine limit that would bind regardless of enforcement, or a constructed constitutional rule whose immutability serves identifiable guardians?',
    'Comparative analysis: whether comparable settlements persist without enforcement machinery or benefiting administrators, and whether the Delphic warrant withstands source criticism.',
    'If constructed, the mountain claim fails and the arrangement routes through false-summit evaluation toward the hybrid or extraction categories; if genuinely natural-law-like, the beneficiary declarations are incidental to its persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ordinance, conceptual, 'Whether the immutability is natural law or a constructed rule serving identifiable guardians.').

omega_variable(
    kernel_reading_commitment_structure,
    'This file instantiates only the sacral_fidelity_reading of the lycurgan_laws kernel; the adaptive_fiction_reading and demographic_trap_reading siblings instantiate different constraints over the same referent with different epsilon, beneficiary, and victim structures — which reading''s structural profile matches the arrangement''s actual operation?',
    'Cross-file comparison of the three sibling stories'' epsilon and structural declarations over the identical referent, adjudicated by the engine''s per-seat computations rather than within this file.',
    'Under the adaptive fiction sibling the immutability becomes maintained deception with a hybrid/extraction profile; under the demographic trap sibling the unrevisability becomes the causal variable in collapse; this file''s mountain claim and low reading-indexed epsilon hold only within the sacral frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling deltas routed here rather than folded into this constraint.').

omega_variable(
    decline_attribution_dispute,
    'Was the collapse of the citizen body from roughly eight thousand to a few hundred the product of the settlement''s unrevisability as design, or of citizen vice, land greed, and external shocks as this reading maintains?',
    'Demographic and prosopographic reconstruction of citizen numbers; comparison against comparably rigid and flexible constitutions; analysis of inheritance and land-concentration patterns in the late classical and Hellenistic periods.',
    'If design-caused, the reading''s exoneration of the settlement fails, unrevisability registers as a cost rather than a perfection, and the founding-problem status shifts toward dead-or-mismatched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decline_attribution_dispute, empirical, 'Design versus vice attribution of the Spartan decline.').

omega_variable(
    delphic_warrant_authenticity,
    'Did the Delphic oracle actually ratify the Rhetra at the traditional founding, or was the divine warrant retrojected onto the settlement by later generations needing sacral cover?',
    'Textual criticism of the Rhetra''s transmission (Tyrtaeus fragments against Plutarch''s account), dating of the amendment episode, and archaeological study of early Spartan cult practice.',
    'If retrojected, the sacral grounding is post-hoc legitimation and the false-summit path strengthens; if authentic, the theological grounding of the axioms stands on its reported warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delphic_warrant_authenticity, empirical, 'Historicity of the oracle''s ratification of the founding instrument.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurgan_sacral_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lycurgan_sacral_tr_t120, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 120, 0.11).
narrative_ontology:measurement(lycurgan_sacral_tr_t240, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 240, 0.13).
narrative_ontology:measurement(lycurgan_sacral_tr_t360, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 360, 0.18).
narrative_ontology:measurement(lycurgan_sacral_tr_t480, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 480, 0.32).
narrative_ontology:measurement(lycurgan_sacral_tr_t600, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 600, 0.48).

% Extraction over time
narrative_ontology:measurement(lycurgan_sacral_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(lycurgan_sacral_be_t120, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 120, 0.16).
narrative_ontology:measurement(lycurgan_sacral_be_t240, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 240, 0.17).
narrative_ontology:measurement(lycurgan_sacral_be_t360, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 360, 0.19).
narrative_ontology:measurement(lycurgan_sacral_be_t480, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 480, 0.24).
narrative_ontology:measurement(lycurgan_sacral_be_t600, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 600, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(lycurgan_sacral_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(lycurgan_sacral_su_t120, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 120, 0.55).
narrative_ontology:measurement(lycurgan_sacral_su_t240, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 240, 0.58).
narrative_ontology:measurement(lycurgan_sacral_su_t360, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 360, 0.7).
narrative_ontology:measurement(lycurgan_sacral_su_t480, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 480, 0.4).
narrative_ontology:measurement(lycurgan_sacral_su_t600, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 600, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__sacral_fidelity_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, adaptive_fiction_reading).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, demographic_trap_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Lycurgan constitution' decomposes into three structurally distinct constraints over one referent: sacral fidelity (this file, mountain claim, reading-indexed low epsilon), adaptive fiction (immutability as maintained deception, hybrid/extraction profile), and demographic trap (brittle design, high epsilon with the citizen body itself among the harmed). Each sibling file links back here via network.affects_constraints; epsilon differs across the family because each reading assesses the same standing arrangement by its own lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
