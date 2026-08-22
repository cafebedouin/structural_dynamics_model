% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: classical_latin_standard__continuity_reading
 *   human_readable: Classical Latin Standard: Continuity Reading
 *   domain: historical_linguistics/commitment_systems
 *
 * SUMMARY:
 *   The continuity reading of the Classical Latin standard asserts that
 *   correct Latin is the living form transmitted through unbroken
 *   institutional practice from late antiquity through the medieval period,
 *   incorporating natural linguistic drift as legitimate development. This
 *   reading is held primarily by medieval institutional grammarians, Church
 *   scribes, and ecclesiastical authorities who need to defend their actual
 *   practice (which includes medieval neologisms, ecclesiastical-technical
 *   vocabulary, and linguistic change) as continuous with and legitimate
 *   within the Classical tradition. The reading legitimates gatekeeping
 *   through institutional transmission — one's Latin is correct if it
 *   descends from authorized channels — while explicitly permitting the
 *   linguistic evolution that distinguishes medieval Latin from Cicero. This
 *   reading coexists with the reconstruction reading (later humanist claim
 *   that correct Latin requires archaeological recovery of Classical norms
 *   and rejection of medieval developments) and with the hybrid reading (both
 *   Classical fidelity and post-Classical developments are legitimate in
 *   their respective domains).
 *
 * KEY AGENTS:
 *   - Church administrative apparatus: institutional agenda-setter, controls scribal training and legitimation
 *   - Institutional scribes and clerics: beneficiaries, defend their evolving practice as continuous descent
 *   - Scholastic grammarians: beneficiaries and educators, reconcile texts with living usage
 *   - Classical reconstructionists: excluded voices (future humanist scholars not yet present)
 *   - Vernacular speakers: powerless, excluded, their language delegitimized as barbarism
 *   - Heretical communities: suppressed, their linguistic variants delegitimized via theology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__continuity_reading, 0.38).
domain_priors:suppression_score(classical_latin_standard__continuity_reading, 0.22).
domain_priors:theater_ratio(classical_latin_standard__continuity_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__continuity_reading, rope).
narrative_ontology:human_readable(classical_latin_standard__continuity_reading, "Classical Latin Standard: Continuity Reading").
narrative_ontology:topic_domain(classical_latin_standard__continuity_reading, "historical_linguistics/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__continuity_reading, '0e73d16c-17b6-4aed-a9c2-d5cbac860ce2').
narrative_ontology:cs_kernel_codification('0e73d16c-17b6-4aed-a9c2-d5cbac860ce2', distributed).
narrative_ontology:cs_authority_grounding('0e73d16c-17b6-4aed-a9c2-d5cbac860ce2', lineage).
narrative_ontology:cs_interpretation_layer_present('0e73d16c-17b6-4aed-a9c2-d5cbac860ce2').
narrative_ontology:cs_reading_relation('0e73d16c-17b6-4aed-a9c2-d5cbac860ce2', classical_latin_standard__reconstruction_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e73d16c-17b6-4aed-a9c2-d5cbac860ce2', classical_latin_standard__hybrid_reading, influences).
narrative_ontology:cs_axiom('0e73d16c-17b6-4aed-a9c2-d5cbac860ce2', foundational, linguistic_continuity_permits_development).
narrative_ontology:cs_axiom_status(linguistic_continuity_permits_development, holdable).
narrative_ontology:cs_axiom_grounding('0e73d16c-17b6-4aed-a9c2-d5cbac860ce2', linguistic_continuity_permits_development, conventional).
narrative_ontology:cs_axiom('0e73d16c-17b6-4aed-a9c2-d5cbac860ce2', secondary, institutional_transmission_grounds_legitimacy).
narrative_ontology:cs_axiom_status(institutional_transmission_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0e73d16c-17b6-4aed-a9c2-d5cbac860ce2', institutional_transmission_grounds_legitimacy, deontological).
narrative_ontology:cs_reference_frame('0e73d16c-17b6-4aed-a9c2-d5cbac860ce2', apostolic_unbroken_transmission).
narrative_ontology:cs_drift_state('0e73d16c-17b6-4aed-a9c2-d5cbac860ce2', high_medieval_systematization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0e73d16c-17b6-4aed-a9c2-d5cbac860ce2', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__continuity_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, institutional_scribes_and_clerics).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, church_administrative_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, scholastic_grammarians).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, ecclesiastical_councils_and_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain Latin literacy through monastic and cathedral schools by transmitting a living, evolving Latin standard that accommodates ecclesiastical and administrative Latin developments. They benefit from the continuity reading's legitimation of their actual practice — the Latin they speak, write, and teach is defensible as correct because it descends unbroken from Classical sources, even as it incorporates medieval neologisms and linguistic drift.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, institutional_scribes_and_clerics, beneficiary,
    organized, generational, constrained, continental).

% Sets and enforces the standard through scriptoria, educational institutions, and theological authority. The continuity reading legitimates Church Latin as the authentic heir to Classical standards, which grounds the Church's own authority (apostolic succession doctrine parallels linguistic continuity from the Apostles onward). Administers the standard by training clerics, maintaining scribal tradition, and excluding or correcting 'barbarisms' — departures that break the continuity chain rather than extend it legitimately.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, church_administrative_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Teach grammar from authoritative texts and living practice, using the continuity reading to reconcile apparent contradictions between what Classical texts prescribe and what living Latin usage embodies. They benefit from the reading's framework because it allows them to defend their students' actual speech as legitimate development rather than error, while still grounding correctness in unbroken transmission from authority.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, scholastic_grammarians, beneficiary,
    organized, generational, mobile, continental).

% Later humanist scholars and Renaissance philologists who would argue that 'correct Latin' requires textual archaeology and recovery of Classical norms, not accommodation of medieval drift. They are not present in the medieval institutional framework but their potential arguments are structurally excluded by the continuity reading's framework — medieval practice is defended as legitimate development, not as corruption requiring reconstruction.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, classical_reconstructionists, excluded,
    powerful, biographical, mobile, continental).

% Are outside the Latin-reading institutional circle entirely. Their vernacular developments are treated as barbarian corruptions or degradation, not as legitimate linguistic drift. The constraint's accessibility collapse for them is near-total: no path to literacy or institutional standing except through subordination to the Latin standard.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, vernacular_speakers, excluded,
    powerless, biographical, trapped, local).

% Develop distinctive Latin usage in their own ecclesiastical contexts (e.g., Arian, Nestorian communities). Their Latin variants are delegitimized not on linguistic grounds but on doctrinal grounds — their language is read as corruption because their theology is deemed false. The constraint suppresses their linguistic alternatives alongside their theological alternatives.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, heretical_or_schismatic_communities, excluded,
    moderate, biographical, constrained, regional).

% Define and defend linguistic norms through synods, papal decrees, and theological pronouncements. The continuity reading's framework — that correct Latin descends unbroken from apostolic times — legitimates the councils' own authority as guardians of unbroken tradition. They benefit from the reading because it makes linguistic purity synonymous with doctrinal orthodoxy.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, ecclesiastical_councils_and_authorities, agenda_setter,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__continuity_reading, ecclesiastical_councils_and_authorities, beneficiary).

% Examines how the constraint operates across centuries and institutional domains, attending to how beneficiaries experience it differently than excluded populations, and how the continuity reading's framework suppresses visibility of its own gatekeeping function.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, analytic_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__continuity_reading, church_administrative_apparatus).
narrative_ontology:fixing_cost_class(classical_latin_standard__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared lingua franca across a fragmented, non-literate medieval Europe, enabling ecclesiastical communication, theological discourse, legal documentation, and administrative continuity. A single linguistic standard — however evolving — solves the collective-action problem of linguistic fragmentation more efficiently than allowing each region's Latin to diverge into mutual unintelligibility.
% TRANSFER_FUNCTION: Moves authority and legitimacy from alternative linguistic communities (vernacular speakers, heretical communities, peripheral regional scribal traditions) to the Church's institutional scribal monopoly. The constraint transfers linguistic standing: one's speech is 'correct' if and only if it descends from the institutional chain of transmission, which is controlled by the Church.
% ABSENT_VOICES: Vernacular speakers whose languages are degraded as barbarisms and have no path to institutional standing. Heretical communities whose linguistic variants are delegitimized alongside their theology. Regional scribal traditions that predate or run parallel to the Church's standard but are suppressed as 'uncorrected' or 'vulgar.' Renaissance philologists whose reconstructionist critique of medieval linguistic drift is not yet articulated within the medieval framework itself.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, the Church would lose its primary mechanism for maintaining unified administrative and theological communication across Europe. Regional Latin variants would diverge more rapidly into what eventually became the Romance languages. The Church's authority to define doctrinal orthodoxy would fragment alongside linguistic standards — heresies would spread more easily across linguistic boundaries, and the Church would lose one of its most effective control mechanisms (conflating linguistic corruption with theological error).
% FOUNDING_PROBLEM: Post-Roman linguistic fragmentation: as Roman administrative structures collapsed, Latin literacy became concentrated in the Church; regional variants proliferated; the risk was mutual unintelligibility within the clergy itself, breaking theological and administrative continuity.
% FOUNDING_PROBLEM_CORROBORATION: Medieval grammarians and Church authorities attest the ongoing necessity of linguistic standards for ecclesiastical communication. The problem remains live because without institutional Latin teaching, regional corruptions would continue. However, independent philologists and historians observe that by the high medieval period, the founding problem (genuine risk of unintelligibility) has substantially been solved — the constraint persists as gatekeeping and legitimation, not as a direct response to fragmentation pressure.
narrative_ontology:disappearance_verdict(classical_latin_standard__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__continuity_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__continuity_reading_tests).
:- end_tests(classical_latin_standard__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint operates gatekeeping (institutional monopoly on literacy, control of standards) but does not systematically delegitimize alternatives through falsehood — medieval developments ARE recognized as legitimate drift within the continuity framework. Suppression is relatively low (0.22) because the reading explicitly permits linguistic change, creating accessibility for legitimate innovation; what IS suppressed is the claim that alternatives (vernacular, heretical variants, reconstructionist purism) are equally valid. Theater is low (0.18) because the teaching function is genuine — grammarians actually do reconcile texts and practice — though an increasing share of enforcement activity defends institutional monopoly over meaning-making. The measurement series shows gentle rise in extractiveness and suppression from T0 to T60, then plateau: as the medieval period solidifies and high medieval systematization increases, gatekeeping intensifies slightly, but the core legitimation strategy (permitting drift as development) remains stable.
 *
 * PERSPECTIVAL GAP:
 *   The reading creates asymmetric accessibility. Institutional insiders benefit from permissiveness (drift is legitimate development); institutional outsiders find the constraint nearly impermeable (vernacular is barbarism, heretical variants are corruption). The constraint permits innovation for authorized speakers while suppressing it for excluded populations — the same linguistic change is legitimate development when performed by a trained scribe, barbarism when performed by a vernacular speaker.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional scribes and Church apparatus benefit from the reading because it legitimates their control over standards while permitting the practice change they require. Their directionality is low (d near 0.1–0.2: they collect prestige, authority, institutional standing from the constraint). Scholastic grammarians benefit from being able to defend their teaching (their actual practice is correct by definition of continuity); they have moderate directionality (d near 0.3–0.4: they gain professional standing but are subordinate to Church authority). Vernacular speakers and heretical communities have high directionality (d near 0.7–0.85: they bear the cost of being excluded and delegitimized); their alternatives are suppressed even though the constraint's own framework permits drift. Ecclesiastical councils occupy both agenda-setter and beneficiary roles: they set the standard and benefit from the reading because it legitimates their authority as guardians of unbroken tradition. No explicit directionality overrides are needed; the structural data (beneficiary vs. excluded role, power atom, exit options) derive the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing linguistic unintelligibility in fragmented post-Roman Europe) is live but substantially solved by the high medieval period. The constraint persists because it performs a second function — legitimating Church authority as guardian of continuous tradition — which has become the primary justification even as the original coordination problem has receded. This is mandatrophy-adjacent but not classical mandatrophy: the constraint's primary function HAS shifted (from preventing fragmentation to legitimating authority), but the reading explicitly embraces this shift as development, not degradation. The reading's claim that drift is legitimate development allows it to absorb functional change without admitting obsolescence. A reconstructionist critique would argue the constraint is now functionally dead (medieval Latin is no longer Classical) and persists only as performance; the continuity reading defends against that critique by redefining development as legitimate continuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drift_vs_corruption_boundary,
    'What marks the boundary between legitimate linguistic development and barbaric corruption within the continuity framework?',
    'Close reading of medieval grammarians'' own criteria for correcting texts vs. accepting variants. Comparison of which innovations grammarians defended as development vs. rejected as error, and what explicit criteria guided those judgments.',
    'If the boundary is explicitly theorized and consistently applied, the reading has internal coherence and lower extractiveness (gatekeeping is transparent). If the boundary is implicit or applied inconsistently (defending Church neologisms as development while rejecting regional variants as corruption), extractiveness and suppression are higher than authored and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_vs_corruption_boundary, empirical, 'Whether the continuity reading has explicit or implicit standards for distinguishing development from corruption.').

omega_variable(
    institutional_monopoly_vs_open_transmission,
    'Is the gatekeeping function of institutional transmission necessary to the continuity reading, or could drift be legitimate within non-institutional contexts as well?',
    'Counterfactual: what would the continuity reading claim about non-institutional Latin traditions (e.g., regional scribal schools outside Church control, merchant communities, vernacular-Latin code-switching)? Are they outside the continuity chain, or do they participate in legitimate development?',
    'If institutional monopoly is necessary, suppression rises and extractiveness is higher than authored (the reading defends institutional gatekeeping). If non-institutional transmission can be legitimate, suppression and extractiveness fall and the reading is closer to pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_monopoly_vs_open_transmission, conceptual, 'Whether the continuity reading necessarily binds legitimate development to institutional transmission.').

omega_variable(
    continuity_vs_reconstruction_foreclosure,
    'Does the continuity reading logically foreclose the reconstruction reading, or are they genuinely coexisting alternatives held by different parties?',
    'Test whether a party could hold both readings simultaneously without internal contradiction. If a medieval grammarian could accept ''continuity permits legitimate development'' AND ''we should recover Classical norms by rejecting medieval developments,'' the readings coexist. If accepting one logically commits you to denying the other, they foreclose.',
    'If foreclosure is true, the readings are incompatible instantiations of the kernel and must be treated as one-or-the-other. If coexistence is true (as expected for this triple), the readings are stable alternatives held by different institutional positions — the engine models them as three separate constraints linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_vs_reconstruction_foreclosure, conceptual, 'Whether continuity_reading and reconstruction_reading logically foreclose each other or coexist.').

omega_variable(
    heretical_suppression_mechanism,
    'Is heretical Latin suppressed as linguistic error (part of the constraint''s gatekeeping function) or as theological error (part of doctrinal enforcement, separate from the linguistic constraint)?',
    'Analysis of how Church authorities write about heretical language: do they critique the language structure itself, or do they treat language as a symptom of theological corruption? Do they attempt to ''correct'' heretical Latin, or forbid it entirely?',
    'If heretical suppression is primarily theological (not linguistic), the constraint''s suppression metric should not include heretical communities — they are suppressed by a separate doctrinal constraint, not by the Latin standard itself. If linguistic, suppression rises and the constraint''s gatekeeping function is tighter than authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(heretical_suppression_mechanism, empirical, 'Whether heretical language variants are suppressed as linguistic or theological deviance.').

omega_variable(
    reading_vs_kernel_distinction,
    'Is this constraint (continuity_reading) really ONE instantiation of a kernel (classical_latin_standard), or is it itself the kernel, with other readings as distinct constraints?',
    'Test: is there a stable COMMITMENT or TEXT that all three readings take as their reference point, and do they disagree ABOUT that reference, not about whether it exists? If yes, it is a kernel (the commitment is the kernel; the readings are different interpretations of it). If each reading has a different reference point (continuity uses living practice, reconstruction uses texts, hybrid uses both selectively), then there is no kernel — there are three independent constraints that linguists have accidentally given related names.',
    'If this is truly a kernel reading, the cs_structure and reading_relations fields are correct and the network.affects_constraints should link all three readings as interdependent. If there is no real kernel (the readings are just semantically similar but structurally independent), the reading_relations are not the core structure — instead, each reading is a separate constraint that happens to address related topics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_kernel_distinction, conceptual, 'Whether classical_latin_standard is a genuine shared kernel or three independent constraints that share semantic ground.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__continuity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(clas_tr_t0, projected).
narrative_ontology:measurement(clas_tr_t20, classical_latin_standard__continuity_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(clas_tr_t20, observed).
narrative_ontology:measurement(clas_tr_t40, classical_latin_standard__continuity_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(clas_tr_t40, observed).
narrative_ontology:measurement(clas_tr_t60, classical_latin_standard__continuity_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(clas_tr_t60, observed).
narrative_ontology:measurement(clas_tr_t80, classical_latin_standard__continuity_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement_basis(clas_tr_t80, observed).
narrative_ontology:measurement(clas_tr_t100, classical_latin_standard__continuity_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement_basis(clas_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__continuity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(clas_be_t0, projected).
narrative_ontology:measurement(clas_be_t20, classical_latin_standard__continuity_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement_basis(clas_be_t20, observed).
narrative_ontology:measurement(clas_be_t40, classical_latin_standard__continuity_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement_basis(clas_be_t40, observed).
narrative_ontology:measurement(clas_be_t60, classical_latin_standard__continuity_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement_basis(clas_be_t60, observed).
narrative_ontology:measurement(clas_be_t80, classical_latin_standard__continuity_reading, base_extractiveness, 80, 0.39).
narrative_ontology:measurement_basis(clas_be_t80, observed).
narrative_ontology:measurement(clas_be_t100, classical_latin_standard__continuity_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement_basis(clas_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t0, classical_latin_standard__continuity_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(clas_su_t0, projected).
narrative_ontology:measurement(clas_su_t20, classical_latin_standard__continuity_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement_basis(clas_su_t20, observed).
narrative_ontology:measurement(clas_su_t40, classical_latin_standard__continuity_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement_basis(clas_su_t40, observed).
narrative_ontology:measurement(clas_su_t60, classical_latin_standard__continuity_reading, suppression_requirement, 60, 0.22).
narrative_ontology:measurement_basis(clas_su_t60, observed).
narrative_ontology:measurement(clas_su_t80, classical_latin_standard__continuity_reading, suppression_requirement, 80, 0.23).
narrative_ontology:measurement_basis(clas_su_t80, observed).
narrative_ontology:measurement(clas_su_t100, classical_latin_standard__continuity_reading, suppression_requirement, 100, 0.22).
narrative_ontology:measurement_basis(clas_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__continuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__continuity_reading, 0.08).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__reconstruction_reading).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% The classical_latin_standard kernel instantiates three structurally distinct constraints, each representing a different reading of what 'correct Latin' means. The continuity_reading (this constraint) claims correct Latin is the living form transmitted through unbroken practice, incorporating drift as legitimate development. The reconstruction_reading claims correct Latin is the Classical form recoverable only through textual archaeology, requiring discontinuous recovery and rejection of medieval developments. The hybrid_reading claims correct Latin requires both Classical fidelity to authoritative texts AND recognition of legitimate post-Classical developments in technical/ecclesiastical domains. These are not observations of the same constraint from different angles; they are readings of a contested kernel that generate different epsilon values, different beneficiary structures, and different suppression mechanisms. The continuity_reading influences the hybrid_reading (builds on its framework of legitimate development) and coexists with the reconstruction_reading (held by different institutional communities with incompatible epistemologies). The three readings together form a constraint family linked through network.affects_constraints; each story carries its own epistemology and terminal type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
