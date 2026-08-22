% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_performance, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: kodashim_obligation__study_as_performance
 *   human_readable: Study of Kodashim as Cosmic-Efficacious Performance
 *   domain: religious/legal/textual
 *
 * SUMMARY:
 *   This story instantiates one reading (study_as_performance) of the
 *   kodashim_obligation kernel: the practice of studying the Talmudic
 *   tractates of Kodashim (sacrificial law) after the Temple's destruction.
 *   On this reading, study does not merely preserve knowledge or maintain
 *   identity — it IS the sacrifice, structurally and cosmically complete in
 *   itself, such that the Temple's physical absence carries no deficit
 *   requiring remedy. This is the coordination-heavy, near-zero-extraction
 *   end of the kernel's reading space: no victim set, no enforcement
 *   apparatus, no restorationist urgency. The sibling readings
 *   (study_as_preparation, study_as_archive) are separate constraints, not
 *   alternative measurements of this one — each has its own ε and its own
 *   file.
 *
 * KEY AGENTS:
 *   - study_house_participants: beneficiary/agenda_setter (moderate/mobile) — perform the study that this reading holds fully efficacious
 *   - cosmic_order: non-agent beneficiary (analytical/universal) — the abstract entity the reading names as receiving the benefit
 *   - rabbinic_interpretive_tradition: agenda_setter (institutional/constrained) — articulates and transmits the doctrine
 *   - diaspora_religious_communities: beneficiary (organized/mobile) — receive a self-sufficient practice requiring no Temple
 *   - study_as_preparation_advocates / study_as_archive_advocates: excluded — hold coexisting sibling readings, not refuted, simply not this reading's framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.03).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.08).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.03).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Study of Kodashim as Cosmic-Efficacious Performance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious/legal/textual").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, 'd6459a93-e693-473a-b06a-a82c7d84ee11').
narrative_ontology:cs_kernel_codification('d6459a93-e693-473a-b06a-a82c7d84ee11', fixed_text).
narrative_ontology:cs_authority_grounding('d6459a93-e693-473a-b06a-a82c7d84ee11', lineage).
narrative_ontology:cs_interpretation_layer_present('d6459a93-e693-473a-b06a-a82c7d84ee11').
narrative_ontology:cs_reading_relation('d6459a93-e693-473a-b06a-a82c7d84ee11', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_reading_relation('d6459a93-e693-473a-b06a-a82c7d84ee11', kodashim_obligation__study_as_archive, influences).
narrative_ontology:cs_axiom('d6459a93-e693-473a-b06a-a82c7d84ee11', foundational, study_fully_substitutes_for_sacrifice).
narrative_ontology:cs_axiom_status(study_fully_substitutes_for_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('d6459a93-e693-473a-b06a-a82c7d84ee11', study_fully_substitutes_for_sacrifice, theological).
narrative_ontology:cs_axiom('d6459a93-e693-473a-b06a-a82c7d84ee11', secondary, temple_absence_is_spiritually_immaterial).
narrative_ontology:cs_axiom_status(temple_absence_is_spiritually_immaterial, holdable).
narrative_ontology:cs_axiom_grounding('d6459a93-e693-473a-b06a-a82c7d84ee11', temple_absence_is_spiritually_immaterial, theological).
narrative_ontology:cs_reference_frame('d6459a93-e693-473a-b06a-a82c7d84ee11', talmudic_substitution_doctrine).
narrative_ontology:cs_drift_state('d6459a93-e693-473a-b06a-a82c7d84ee11', contemporary_diaspora_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d6459a93-e693-473a-b06a-a82c7d84ee11', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, cosmic_order).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, study_house_participants).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, diaspora_religious_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in daily or cyclical study of the tractates of Kodashim (Zevachim, Menachot, and related sacrificial law) as an act held to substitute for and complete the sacrificial rite itself. They set the daily study agenda (e.g. daf yomi cycles) and receive, on this reading, the same spiritual benefit the sacrifice would have conferred. Exit from this specific practice is available (one can choose not to study Kodashim) without loss of standing in most communities, though within study-centered communities the practice is central to communal identity.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, study_house_participants, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_performance, study_house_participants, agenda_setter).

% The abstract cosmic/metaphysical order that, on this reading, is sustained or repaired by the recitation and study of sacrificial law in lieu of its physical performance. Not an actor; included for completeness because the reading names it as the primary beneficiary of the constraint's operation, displacing any human party from that seat.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, cosmic_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_performance, cosmic_order).

% Communities without Temple access who receive, under this reading, a fully sufficient religious practice: study substitutes completely, so the absence of a Temple is not experienced as a deficit requiring remedy. This removes the pressure toward messianic-restorationist anxiety that a preparation-reading would generate.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, diaspora_religious_communities, beneficiary,
    organized, generational, mobile, global).

% The chain of halakhic authorities (from Talmudic sources onward, e.g. the reading of Hosea 14:3 'let our lips substitute for bullocks' and Talmudic statements that studying sacrificial law is as if the sacrifice were performed) who articulate and transmit the study-as-performance doctrine. They administer which texts count as efficacious study and how it is to be conducted, but do not extract material benefit from the doctrine's operation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, rabbinic_interpretive_tradition, agenda_setter,
    institutional, generational, constrained, global).

% Hold the sibling reading that sacrificial law remains binding-but-unperformable and study serves to preserve technical competence for eventual Temple restoration. Under this reading's framework they are not refuted, merely occupying a different commitment about what the study accomplishes; they would object that treating study as fully efficacious removes urgency from restorationist practice and prayer for the Temple's rebuilding.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, study_as_preparation_advocates, excluded,
    organized, generational, mobile, global).

% Hold the sibling reading that Kodashim documents a defunct system and study is historical/identity preservation without legal-obligation or cosmic-function force. They would object that assigning cosmic efficacy to study over-claims metaphysical warrant for what is, on their view, a preservationist and cultural practice.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, study_as_archive_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed, Temple-less community around a shared practice (structured daily/cyclical study of sacrificial law) that supplies continuity of religious performance and communal identity without requiring the physical infrastructure, priesthood, or geography the original sacrificial system depended on.
% TRANSFER_FUNCTION: No material transfer occurs between human parties under this reading. What moves is time and attention from the practitioner into study, and (on this reading's own terms) spiritual efficacy flows from the act of study to the maintenance of cosmic/covenantal order — a transfer with no human payer or human recipient other than the cosmic order itself.
% ABSENT_VOICES: Advocates of the study_as_preparation and study_as_archive readings would object that this reading either over-claims metaphysical efficacy (archive critique) or removes the theological urgency of restoration (preparation critique). Neither is present as an adjudicating party within this reading's own framework; each holds a separate, coexisting commitment.
% DISAPPEARANCE_RATIONALE: Practitioners within this reading hold that the practice's disappearance would leave a cosmic function unperformed, which they take to be consequential even though undetectable by ordinary means; from an external observer's standpoint, the disappearance of Kodashim study would leave daily religious life, communal calendars, and identity practices materially rearranged, but would not obviously perturb anything checkable outside the tradition's own commitments. Whether the world 'rearranges' therefore depends on whether one credits the reading's own cosmological claim.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), sacrificial worship could no longer be physically performed, threatening the covenantal relationship's continuity and the felt efficacy of atonement and cosmic maintenance that sacrifice had provided.
% FOUNDING_PROBLEM_CORROBORATION: Within this reading, the rabbinic sources themselves (e.g. b. Menachot 110a, and the Hosea verse cited above) attest that study substitutes fully for sacrifice — but this corroboration comes from the same tradition that benefits from the doctrine's continuation. Comparative religion scholars observing the phenomenon from outside the tradition (studying analogous substitution mechanisms in other post-cultic religious traditions) corroborate that such doctrines function to sustain practice-continuity after ritual infrastructure loss, without adjudicating the truth of the cosmic-efficacy claim itself. No corroboration exists from a party who would independently verify the metaphysical claim; that absence is itself part of what the omega below tracks.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_performance, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_performance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_performance, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_performance_tests).
:- end_tests(kodashim_obligation__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03) because this reading structurally locates no human payer: no material, temporal, or status cost is transferred from any human party to another. The near-total absence of victims is not an oversight but the reading's defining structural feature (per the expected delta: 'zero extractiveness, no victim set'). Suppression is low (0.08) because no coercive apparatus enforces participation in Kodashim study specifically — communities that emphasize it do so through prestige and practice-culture, not compulsion. Theater ratio is low but nonzero and slowly rising (0.05→0.10) reflecting that some performative/ceremonial framing (e.g. public daf yomi completion celebrations) accretes around the practice over centuries without displacing its substantive study content. Accessibility collapse is moderate (0.35): once a practitioner is inside a study-centered community the alternative readings become harder to hold seriously in practice, though they remain intellectually available. Resistance is low (0.15): little active pushback against the doctrine from within Orthodox study communities, though the sibling readings represent quieter, non-antagonistic dissent from outside that framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Because this reading locates no victim and treats the beneficiary as either the diffuse practicing community or the non-agent 'cosmic order,' directionality across stakeholders clusters near the beneficiary end for every human party. Study-house participants and diaspora communities are declared beneficiaries with mobile exit (participation is optional, not trapped), which the derivation chain reads as low d. The rabbinic interpretive tradition sits as agenda_setter with institutional power and constrained exit (a rabbi who wished to reject the doctrine faces real reputational and communal cost) — this is the only seat with directionality tension, since setting doctrine and being bound by its transmission expectations are both present. No override was needed: the derived d values already track the reading's own claim that no one pays.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/disappearance_verdict pairing is intentionally contested rather than resolved: within the reading's own terms the founding problem (loss of the ability to perform sacrifice) is permanently and fully solved by study, so the arrangement is not a zombie mandate — it does exactly what it was built for, forever, without needing the Temple to return. This is precisely what prevents mislabeling this reading as extractive theater: unlike a scaffold whose justification is a transition it never completes, this reading claims the transition is already complete and permanently so. Mandatrophy would only bite if it could be shown that even by the tradition's own lights the doctrine's cosmic-efficacy claim had gone dead or had converted into pure status-signaling detached from any believed function — the low, slowly-rising theater_ratio is the variable to watch for that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_performance_metaphysical_warrant,
    'Is the claim that study of sacrificial law fully substitutes for and completes the sacrifice''s cosmic function a genuine metaphysical fact this reading tracks, or a doctrinally load-bearing claim whose function is to resolve the psychological/communal crisis of Temple loss regardless of its truth?',
    'No empirical resolution mechanism exists for the metaphysical claim itself; the question can be partially triangulated by comparative-religion analysis of structurally analogous substitution doctrines in other post-cultic traditions (e.g. post-Second-Temple-adjacent movements, or other ritual systems that lost their material infrastructure) to see whether such doctrines correlate with specific historical pressures rather than independent theological discovery.',
    'If the doctrine is best explained as crisis-adaptive rather than independently warranted, the reading''s claimed zero-extraction, no-victim structure remains internally coherent but the cosmic-order beneficiary seat becomes better described as a proposition the tradition needed vindicated rather than a genuine recipient of benefit — this would not change the classification but would sharpen the vindicated_propositions framing already used here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_as_performance_metaphysical_warrant, conceptual, 'Whether cosmic efficacy is a tracked metaphysical fact or a crisis-adaptive doctrinal construction').

omega_variable(
    kernel_reading_selection_pressure,
    'Given that all three readings of kodashim_obligation coexist within the broader tradition, what determines which reading a given community or individual practitioner adopts, and does that selection correlate with structural position (e.g. Temple-restorationist political movements favor study_as_preparation; secularizing or academic Jewish communities favor study_as_archive)?',
    'Sociological survey of self-reported reading commitments across denominational and political lines, cross-referenced with attitudes toward Temple restoration activism and archaeological/political engagement with the Temple Mount.',
    'If reading selection tracks political-theological commitments (e.g. restorationist activism) rather than independent theological reasoning, this reading''s ''Temple restoration not structurally necessary'' delta becomes a contested boundary condition rather than a settled feature — some communities holding this reading might still support restorationist politics for unrelated reasons, complicating the clean decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, empirical, 'Whether reading adoption correlates with restorationist political commitments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(koda_tr_t400, kodashim_obligation__study_as_performance, theater_ratio, 400, 0.07).
narrative_ontology:measurement(koda_tr_t900, kodashim_obligation__study_as_performance, theater_ratio, 900, 0.08).
narrative_ontology:measurement(koda_tr_t1400, kodashim_obligation__study_as_performance, theater_ratio, 1400, 0.09).
narrative_ontology:measurement(koda_tr_t1950, kodashim_obligation__study_as_performance, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_performance, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(koda_be_t400, kodashim_obligation__study_as_performance, base_extractiveness, 400, 0.02).
narrative_ontology:measurement(koda_be_t900, kodashim_obligation__study_as_performance, base_extractiveness, 900, 0.03).
narrative_ontology:measurement(koda_be_t1400, kodashim_obligation__study_as_performance, base_extractiveness, 1400, 0.03).
narrative_ontology:measurement(koda_be_t1950, kodashim_obligation__study_as_performance, base_extractiveness, 1950, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_performance, 0.08).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_preparation).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kodashim_obligation kernel decomposed per the epsilon-invariance principle: study_as_performance (this file, near-zero epsilon, cosmic beneficiary, no victims, founding problem fully and permanently resolved), study_as_preparation (binding-but-unperformable obligation preserved for messianic restoration — expected higher structural tension between present unperformability and future completion, founding problem status likely 'live'), and study_as_archive (historical/identity preservation without legal-obligation or cosmic-function force — expected lowest metaphysical stakes and most secularized coordination framing). Each carries its own epsilon and stakeholder structure; do not average across them. This reading exerts an 'influences' relation on study_as_archive because a community's adoption of fully efficacious study tends to reduce the social space and resourcing available for a purely archival/secular framing of the same texts, without logically foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
