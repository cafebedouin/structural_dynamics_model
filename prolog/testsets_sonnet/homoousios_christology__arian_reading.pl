% ============================================================================
% CONSTRAINT STORY: homoousios_christology__arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__arian_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: homoousios_christology__arian_reading
 *   human_readable: Arian Christology: Subordinationist Reading of Christ's Nature
 *   domain: historical_theology/ecclesiastical_politics
 *
 * SUMMARY:
 *   This constraint models the Arian reading of the fourth-century
 *   christological controversy: the claim that Christ, the Son, was created
 *   by the Father and is therefore subordinate and not of identical substance
 *   (ousia). Following the Council of Nicaea (325 CE) condemned this
 *   position, it nonetheless retained substantial institutional life for the
 *   following half-century through the sympathy of successive Eastern Roman
 *   emperors (notably Constantius II and, later, Valens), who used state
 *   power to depose Nicene bishops and install Arian or semi-Arian clergy
 *   across contested sees, including Alexandria and Constantinople. The
 *   measured extraction and suppression rise and fall with imperial patronage
 *   cycles rather than moving monotonically — a genuine oscillation, not
 *   drift, driven by which emperor held the throne and whether that emperor
 *   favored subordinationist Christology. The pattern reverses permanently
 *   after Theodosius I's Edict of Thessalonica (380 CE) established Nicene
 *   Christianity as the sole legitimate imperial religion, after which the
 *   Arian reading lost imperial backing across the Roman world (though it
 *   persisted for centuries among some Germanic tribes converted during the
 *   Arian ascendancy, a separate downstream constraint not modeled here).
 *
 * KEY AGENTS:
 *   - arius_and_successors: originating theological voice, organized/constrained
 *   - non_nicene_bishops: institutional beneficiaries whose authority depends on imperial backing
 *   - nicene_clergy_exiled: primary victims, trapped exit, cyclically deposed and restored
 *   - lay_congregations_caught_in_schism: powerless bystanders bearing social costs of factional turnover
 *   - arian_imperial_enforcers: state apparatus wielding theological uniformity as a tool of political cohesion
 *   - later_church_historians: analytical observers reconstructing the pattern from largely Nicene-shaped sources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__arian_reading, 0.58).
domain_priors:suppression_score(homoousios_christology__arian_reading, 0.72).
domain_priors:theater_ratio(homoousios_christology__arian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__arian_reading, "Arian Christology: Subordinationist Reading of Christ's Nature").
narrative_ontology:topic_domain(homoousios_christology__arian_reading, "historical_theology/ecclesiastical_politics").

domain_priors:requires_active_enforcement(homoousios_christology__arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__arian_reading, 'cb60dae8-dbaa-4cd8-891e-183794cd559e').
narrative_ontology:cs_kernel_codification('cb60dae8-dbaa-4cd8-891e-183794cd559e', distributed).
narrative_ontology:cs_authority_grounding('cb60dae8-dbaa-4cd8-891e-183794cd559e', distributed).
narrative_ontology:cs_reading_relation('cb60dae8-dbaa-4cd8-891e-183794cd559e', homoousios_christology__pro_nicene_reading, forecloses).
narrative_ontology:cs_reading_relation('cb60dae8-dbaa-4cd8-891e-183794cd559e', homoousios_christology__semi_arian_reading, influences).
narrative_ontology:cs_axiom('cb60dae8-dbaa-4cd8-891e-183794cd559e', foundational, son_is_created_being).
narrative_ontology:cs_axiom_status(son_is_created_being, holdable).
narrative_ontology:cs_axiom_grounding('cb60dae8-dbaa-4cd8-891e-183794cd559e', son_is_created_being, deontological).
narrative_ontology:cs_axiom('cb60dae8-dbaa-4cd8-891e-183794cd559e', foundational, strict_numerical_monotheism_requires_ontological_subordination).
narrative_ontology:cs_axiom_status(strict_numerical_monotheism_requires_ontological_subordination, overridden).
narrative_ontology:cs_axiom_grounding('cb60dae8-dbaa-4cd8-891e-183794cd559e', strict_numerical_monotheism_requires_ontological_subordination, conventional).
narrative_ontology:cs_reference_frame('cb60dae8-dbaa-4cd8-891e-183794cd559e', subordinationist_pre_nicene_theology).
narrative_ontology:cs_drift_state('cb60dae8-dbaa-4cd8-891e-183794cd559e', post_theodosian_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('cb60dae8-dbaa-4cd8-891e-183794cd559e', '').
narrative_ontology:cs_kernel_id(homoousios_christology__arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, non_nicene_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, arian_clergy_networks).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, eastern_court_theologians).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, nicene_clergy_exiled).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, lay_congregations_caught_in_schism).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, alexandrian_church_faction).
narrative_ontology:constraint_vindicates(homoousios_christology__arian_reading, strict_monotheism_preservation).
narrative_ontology:constraint_vindicates(homoousios_christology__arian_reading, divine_transcendence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulates and defends the position that the Son was created by the Father and is therefore ontologically subordinate, not co-eternal or of identical substance. Builds theological arguments from scriptural exegesis (e.g., 'the Father is greater than I') and philosophical concerns about compromising divine unity. Faces condemnation and exile cycles depending on which emperor holds power.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, arius_and_successors, agenda_setter,
    organized, generational, constrained, regional).

% Hold sees across the Eastern empire and gain or retain ecclesiastical authority, imperial favor, and doctrinal legitimacy when Arian or semi-Arian emperors (Constantius II, Valens) back their reading. Their institutional power waxes and wanes with imperial patronage rather than settled doctrine, making their position durable but never fully secure.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, non_nicene_bishops, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, non_nicene_bishops, agenda_setter).

% Advise emperors sympathetic to subordinationist Christology, shaping court religious policy and gaining influence, appointments, and access. Can shift allegiance between theological factions as imperial winds change, giving them more genuine exit than clergy tied to specific sees.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, eastern_court_theologians, beneficiary,
    powerful, biographical, mobile, regional).

% Bishops and clergy (Athanasius among the most prominent) who hold the homoousios position are repeatedly deposed, exiled, or physically driven from their sees when Arian-sympathetic emperors enforce subordinationist doctrine as imperial policy. Their exit options are essentially nil: flight, exile, or recantation are the only paths, and none restores their position without a change in imperial patronage.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, nicene_clergy_exiled, payer,
    moderate, biographical, trapped, continental).

% Ordinary believers in contested sees experience alternating installations of Arian and Nicene bishops, competing liturgies, and periodic violence between factions (as in Alexandria and Constantinople). They have no voice in the doctrinal dispute and bear the social and sometimes physical cost of factional turnover imposed from above.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, lay_congregations_caught_in_schism, payer,
    powerless, immediate, trapped, local).

% The Alexandrian see, historically Nicene-aligned, experiences repeated imperial intervention installing Arian bishops (e.g., George of Cappadocia) over local objection, provoking riots and prolonged local resistance. Their theological tradition and local authority are directly targeted by enforcement actions originating in imperial courts.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, alexandrian_church_faction, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, alexandrian_church_faction, excluded).

% Emperors and their administrative apparatus (particularly under Constantius II and Valens) enforce subordinationist doctrine as state policy, using exile, deposition, and military force to install compliant bishops and suppress Nicene resistance. They treat theological uniformity as a tool of imperial cohesion, shifting policy whenever political calculation favors it.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, arian_imperial_enforcers, agenda_setter,
    institutional, biographical, arbitrage, continental).

% Holds that this entire framing is heretical, that homoousios is the only faithful reading, and that the Arian reading's structural durability rests entirely on imperial coercion rather than doctrinal merit. Excluded from legitimate voice whenever an Arian-sympathetic emperor holds power; their objections are recorded mainly in polemical histories written after Nicene ascendancy was restored.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, pro_nicene_faction, excluded,
    organized, generational, trapped, continental).

% Assess the fourth-century controversy from later vantage points, generally through sources shaped by the eventual Nicene-Chalcedonian consensus, and reconstruct how much of the Arian reading's persistence depended on genuine theological conviction versus imperial power struggles.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent theological account of divine unity and transcendence: by holding the Son as created and subordinate, the arrangement resolves the philosophical worry that treating Father and Son as of identical substance compromises strict monotheism and produces two co-equal gods.
% TRANSFER_FUNCTION: Moves ecclesiastical authority, imperial patronage, and control of sees from Nicene-aligned clergy to non-Nicene bishops whenever an Arian-sympathetic emperor enforces the reading as state policy; moves social and physical stability away from lay congregations in contested sees toward whichever faction currently holds imperial backing.
% ABSENT_VOICES: Lay congregations in contested sees have no formal voice in the christological debate despite bearing its costs directly (rioting, alternating clergy, social division). The Alexandrian faction's sustained local resistance is documented mainly by its opponents. Ordinary believers' theological views, as opposed to elite factional positions, are almost entirely unrecorded.
% DISAPPEARANCE_RATIONALE: If the Arian reading and its imperial enforcement apparatus disappeared, non-Nicene bishops would lose their sees and patronage networks, exiled Nicene clergy would be restored, contested congregations would stabilize under a single doctrinal authority, and imperial religious policy would no longer function as a lever for controlling ecclesiastical appointments across the Eastern empire — which is substantially what happened after Theodosius I's Nicene enforcement from 380 CE onward.
% FOUNDING_PROBLEM: How to reconcile Christian monotheism with the divine status attributed to Christ in scripture and worship, without either compromising the oneness of God (the Arian worry) or demoting Christ to a mere creature unable to fully save or reveal God (the Nicene worry). The founding problem was a genuine, unresolved philosophical-theological tension in early Christian doctrine.
% FOUNDING_PROBLEM_CORROBORATION: The Arian side (Arius, Eusebius of Nicomedia, and successive Eastern courts) attests the problem is live and that homoousios language is the actual departure from scriptural monotheism. Nicene sources (Athanasius, the Cappadocians, later conciliar tradition) attest the problem was resolved at Nicaea and that Arian persistence after 325 CE was sustained primarily by imperial politics rather than unresolved theology. No source fully outside both benefiting factions survives from the period itself; modern historians of late antiquity (outside either theological tradition) generally corroborate that imperial political calculation, not settled doctrine, drove the alternating enforcement pattern across the fourth century.
narrative_ontology:disappearance_verdict(homoousios_christology__arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__arian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__arian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__arian_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__arian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__arian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction and suppression are authored as moderate-to-high and oscillating rather than monotonic, reflecting the historical record: subordinationist doctrine's institutional durability tracked imperial patronage, rising sharply under Constantius II (350s CE), receding somewhat under Julian's religious pluralism, and rising again under Valens (360s-370s), before collapsing after Theodosius I. Theater ratio is moderate (~0.28 at interval end) because doctrinal disputes were substantively contested (not merely performative) even as imperial power politics shaped which side could enforce its reading at any given moment. Suppression is authored higher than extraction because the mechanism of persistence was coercive — exile, deposition, and occasionally violence — rather than voluntary theological consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the non-Nicene bishops' seat, this is legitimate theological coordination around a coherent and scripturally-grounded position, temporarily vindicated by orthodox imperial policy. From the exiled Nicene clergy's seat, the same structure is enforced heresy sustained only by transient state coercion. The engine should compute these as structurally different experiences of one constraint, not as two constraints — the divergence is exactly what the seat-level classification is meant to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-Nicene bishops and eastern court theologians sit near the beneficiary end: they gain sees, patronage, and doctrinal legitimacy when imperial policy favors them, and their exit options (mobile for court theologians, constrained for bishops) reflect genuine but bounded agency. Nicene clergy and lay congregations sit near the target end: exiled clergy have essentially trapped exit (flight or recantation only), and lay congregations bear costs imposed from above with zero voice in the dispute. The Alexandrian faction sits as an organized payer with some collective resistance capacity but ultimately constrained exit, since the see's fate depends on imperial appointment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling Christian monotheism with Christ's scriptural divine status — was a genuine, unresolved theological tension at the controversy's outset, which is why this constraint should not be read as pure extraction dressed as coordination: the coordination function (a coherent account of divine unity) was real for those who held it. What makes this tangled rather than a pure rope is the asymmetric extraction riding on top: once imperial power entered the picture, the doctrinal dispute became a mechanism for redistributing ecclesiastical authority and imposing real costs (exile, riot, instability) on parties who did not choose the imperial side that happened to hold power in a given decade. The founding_problem_status is authored as contested rather than dead precisely because both sides plausibly still hold their positions as live theological convictions even where the corpus also shows the dispute functioning as a vehicle for imperial control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_conviction_vs_imperial_instrument,
    'Was the persistence of the Arian reading after Nicaea driven primarily by genuine, widely-held theological conviction among Eastern clergy, or primarily by imperial political calculation using doctrinal uniformity as a tool of state cohesion?',
    'Comparative analysis of surviving Arian theological writings independent of court context, cross-referenced against the timing correlation between doctrinal enforcement shifts and changes in imperial religious policy; assessment of whether Arian doctrinal positions shifted in ways that tracked political convenience versus internal theological development.',
    'If primarily political instrument, the constraint is more purely extractive (tangled_rope tilting toward snare) with theological content as cover. If primarily genuine conviction with imperial politics as an amplifying but independent factor, the coordination function is more substantial and the tangled_rope classification with real coordination content is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_conviction_vs_imperial_instrument, empirical, 'Whether Arian persistence reflects genuine theological conviction or instrumentalized imperial politics.').

omega_variable(
    kernel_reading_sibling_disagreement_locus,
    'Where exactly does the disagreement between this reading and its siblings (pro_nicene, semi_arian) locate structurally — is it a dispute over the meaning of a shared term (ousia/substance), over which scriptural passages carry doctrinal weight, or over a prior philosophical commitment about the nature of divine unity that precedes any specific textual dispute?',
    'Textual-philosophical analysis of the earliest surviving statements from each faction (Arius''s letters, the Nicene creed''s anathemas, Basil of Ancyra''s semi-Arian formulations) to identify whether the dispute is primarily semantic, exegetical, or metaphysical in origin.',
    'If the disagreement is primarily philosophical/metaphysical (a prior commitment about divine unity), the three readings are more genuinely coexisting incommensurable frameworks. If primarily semantic (disagreement over what a Greek philosophical term entails), later reconciliation attempts (like the semi-Arian homoiousios compromise) make more structural sense as genuine bridge positions rather than mere political expedients.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_sibling_disagreement_locus, conceptual, 'Whether the kernel dispute is semantic, exegetical, or metaphysical in origin, and what that implies about the coherence of a middle reading.').

omega_variable(
    natural_theology_vs_constructed_faction,
    'Is the Arian reading better understood as a natural extension of pre-existing subordinationist tendencies already present in earlier Christian theology (Origen, Justin Martyr), or as a constructed faction that crystallized specifically in response to Nicaea''s homoousios formula and gained beneficiaries only after the fact?',
    'Genealogical tracing of subordinationist language in pre-Nicene theological writing, compared against the specific timing and composition of the post-Nicene Arian coalition''s beneficiary structure.',
    'If a natural extension, the reading''s persistence has stronger independent theological legitimacy claims. If a constructed post-Nicene faction, the beneficiary structure (non_nicene_bishops, eastern_court_theologians) more plausibly drove doctrinal formulation rather than the reverse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_theology_vs_constructed_faction, conceptual, 'Whether Arian subordinationism predates and independently motivates the post-Nicene faction, or whether the faction''s interests shaped the doctrine''s post-Nicene articulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__arian_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_christology__arian_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(homo_tr_t0, observed).
narrative_ontology:measurement(homo_tr_t10, homoousios_christology__arian_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(homo_tr_t10, observed).
narrative_ontology:measurement(homo_tr_t20, homoousios_christology__arian_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(homo_tr_t20, observed).
narrative_ontology:measurement(homo_tr_t30, homoousios_christology__arian_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(homo_tr_t30, observed).
narrative_ontology:measurement(homo_tr_t40, homoousios_christology__arian_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement_basis(homo_tr_t40, observed).
narrative_ontology:measurement(homo_tr_t50, homoousios_christology__arian_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement_basis(homo_tr_t50, observed).
narrative_ontology:measurement(homo_tr_t60, homoousios_christology__arian_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(homo_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_christology__arian_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(homo_be_t0, observed).
narrative_ontology:measurement(homo_be_t10, homoousios_christology__arian_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(homo_be_t10, observed).
narrative_ontology:measurement(homo_be_t20, homoousios_christology__arian_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(homo_be_t20, observed).
narrative_ontology:measurement(homo_be_t30, homoousios_christology__arian_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement_basis(homo_be_t30, observed).
narrative_ontology:measurement(homo_be_t40, homoousios_christology__arian_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement_basis(homo_be_t40, observed).
narrative_ontology:measurement(homo_be_t50, homoousios_christology__arian_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement_basis(homo_be_t50, observed).
narrative_ontology:measurement(homo_be_t60, homoousios_christology__arian_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(homo_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_christology__arian_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(homo_su_t0, observed).
narrative_ontology:measurement(homo_su_t10, homoousios_christology__arian_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(homo_su_t10, observed).
narrative_ontology:measurement(homo_su_t20, homoousios_christology__arian_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(homo_su_t20, observed).
narrative_ontology:measurement(homo_su_t30, homoousios_christology__arian_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(homo_su_t30, observed).
narrative_ontology:measurement(homo_su_t40, homoousios_christology__arian_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(homo_su_t40, observed).
narrative_ontology:measurement(homo_su_t50, homoousios_christology__arian_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement_basis(homo_su_t50, observed).
narrative_ontology:measurement(homo_su_t60, homoousios_christology__arian_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(homo_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__arian_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, semi_arian_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'the christological controversy' (or 'the Arian controversy') into structurally distinct kernel readings, per the ε-invariance principle: arian_reading (this file), pro_nicene_reading, and semi_arian_reading. Each reading has its own beneficiary/victim structure and its own extraction profile depending on which faction held imperial backing at a given moment; they are linked here rather than merged because measuring 'the controversy' from the Arian institutional vantage versus the Nicene institutional vantage yields different ε values for who bears the enforcement cost. The pro_nicene_reading is the eventual conciliar victor (325 CE and definitively 381 CE) and functions as the network's upstream anchor; this reading and semi_arian_reading are downstream contestants whose institutional fortunes are more volatile and more directly coupled to imperial political cycles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_christology__arian_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
