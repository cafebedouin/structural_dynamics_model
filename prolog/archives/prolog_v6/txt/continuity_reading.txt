% ============================================================================
% CONSTRAINT STORY: continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_continuity_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: continuity_reading
 *   human_readable: Correct Latin as Living Continuity: Medieval Institutional Coordination
 *   domain: historical_linguistics/philology/institutional_practice
 *
 * SUMMARY:
 *   The continuity reading of 'correct Latin' asserts that medieval
 *   institutional Latin is a living continuation of Classical Latin, evolving
 *   naturally through phonological and morphological drift while maintaining
 *   intelligibility and function across generations. This reading frames
 *   Latin as a living language, not a frozen standard, and sees the medieval
 *   Church not as degrading or corrupting an ancient form but as maintaining
 *   and gradually adapting it through practice. The constraint solves a
 *   genuine coordination problem: monasteries, dioceses, and church offices
 *   scattered across Europe needed a written standard that would enable
 *   document retention, inter-institutional communication, and liturgical
 *   consistency across centuries. The continuity reading treats this as pure
 *   coordination with low extractive overhead — the standard emerges from
 *   practice, evolves with practice, and benefits its users by enabling
 *   communication. The competing reconstructionist reading claims instead
 *   that medieval Latin is a degraded approximation of a fixed Classical
 *   standard, reframing the same linguistic evolution as corruption of an
 *   ideal. The competing symbolic_reoccupation reading treats the entire
 *   kernel claim as performative legitimation — the Church claims continuity
 *   with antiquity not because it linguistically preserved anything but
 *   because the claim itself confers authority. This JSON instantiates ONLY
 *   the continuity reading.
 *
 * KEY AGENTS:
 *   - Medieval Institutional Users (institutional/arbitrage): Monks, scribes, church administrators who use Latin daily for administrative, liturgical, and scholarly purposes. Primary beneficiaries — the standard coordinates their communication. Not trapped because they can adopt regional dialects or simplified forms when needed, but they remain within Latin through arbitrage logic (administrative efficiency exceeds cost of learning variation).
 *   - Monastic Scriptoria (institutional/constrained): Scribal communities that learn, transmit, and enforce the standard through copying, correcting, and training. Secondary beneficiaries who also bear some suppression cost (manuscript rejection for 'incorrect' form). Constrained by career path dependence — leaving the scriptoria means losing scribal identity.
 *   - The Church Administrative Apparatus (institutional/arbitrage): Ecclesiastical authorities whose continued operation depends on document retention and inter-diocesan communication in Latin. Beneficiaries with maximum arbitrage freedom — they can change standards if administrative benefit justifies it, but they maintain Latin because it works.
 *   - The Learned Clergy (powerful/constrained): Bishops, theologians, educated priests who shape standards through scholastic commentary and corrective practice. Beneficiaries with some enforcement power — they define what 'correct' means and can reward or punish deviation. Constrained by the need to maintain consensus across regions and generations.
 *   - The Non-Lettered Laity (powerless/trapped): Those excluded from written culture, unable to learn Latin without institutional patronage. Victims bearing costs of exclusion and status asymmetry while receiving minimal coordination benefit. Trapped — no exit option without abandoning their communities.
 *   - Later Reconstructionist Scholars (analytical/arbitrage): Modern Latinists and Renaissance humanists who may judge medieval Latin against Classical standards. Not embedded in the continuity reading's institutional logic — they stand outside and can choose to accept or reject the continuity thesis based on linguistic evidence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(continuity_reading, 0.28).
domain_priors:suppression_score(continuity_reading, 0.35).
domain_priors:theater_ratio(continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(continuity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(continuity_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(continuity_reading, rope).
narrative_ontology:human_readable(continuity_reading, "Correct Latin as Living Continuity: Medieval Institutional Coordination").
narrative_ontology:topic_domain(continuity_reading, "historical_linguistics/philology/institutional_practice").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(continuity_reading, medieval_institutional_users).
narrative_ontology:constraint_beneficiary(continuity_reading, monastic_scriptoria).
narrative_ontology:constraint_beneficiary(continuity_reading, church_administrative_apparatus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL INSTITUTIONAL USER (ROPE) — For monastery scribes and church administrators, 'correct Latin' is a practical coordination standard: a living language that evolves naturally with use, maintaining intelligibility across generations while accommodating phonetic and morphological drift. The constraint solves the problem of communication without requiring artificial archaism. Benefits from the standard while also shaping it through practice. Exit option is arbitrage — users can adopt local dialects or simplified forms when the administrative cost justifies it, but they remain within the Latin system.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE SCRIBE LEARNING THE STANDARD (ROPE) — For a novice scribe, 'correct Latin' is an evolving standard transmitted through practice and correction. The constraint coordinates knowledge transfer: elders model the current acceptable form; younger scribes internalize it and slightly modify it. Suppression is moderate (punishment for egregious errors, manuscript rejection) but not total — the standard is intentionally flexible enough to accommodate regional and individual variation. Exit is constrained by career path dependence but not trapped — scribes can learn alternative writing systems or dialects if they accept reduced status.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: THE NON-LETTERED LAITY (TANGLED ROPE) — The constraint simultaneously coordinates church communication (laity can hear Mass in a form ancestral to their vernacular) and extracts status asymmetry (literacy itself becomes a gatekeeping mechanism; the non-lettered cannot verify, contest, or participate in written administration). The laity receives some benefit (intelligible sermons, prayer texts) but bears the cost of exclusion from written culture. This is not snare (they are not targets of pure extraction) but tangled rope: genuine coordination function exists alongside asymmetric extraction. Exit is trapped — they cannot learn Latin without institutional access and patronage.
constraint_indexing:constraint_classification(continuity_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: THE RECONSTRUCTIONIST CLASSICIST / EXTERNAL OBSERVER (PITON) — From the perspective of a later classically-trained scholar (e.g., a Renaissance humanist, a modern Latinist), the medieval continuity reading appears as a degraded piton: the medieval Church claims to maintain 'correct Latin' (the original kernel claim), but what they actually maintain is a corrupted, simplified version that has lost Classical grammar and adopted vulgar phonetics. The performance (preserving the name 'Latin') exceeds the function (maintaining Classical standards). Theater ratio is high from this external perspective — the medieval standard is maintained performatively as 'continuity with antiquity' rather than functionally as actual Classical Latin. However, this perspective is NOT the continuity reading itself; it is the perspective of someone rejecting the continuity reading in favor of reconstruction logic.
constraint_indexing:constraint_classification(continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / CONTINUITY THESIS (ROPE) — From a linguistic-historical perspective that accepts the continuity reading, Classical Latin did not 'die' — it evolved. The medieval institutional Latin of the 8th-12th centuries is a living continuation of earlier forms, showing expected phonological and morphological changes consistent with language drift. 'Correct Latin' is the current form of an evolving standard, not a degraded copy of an older form. This perspective sees the constraint as pure coordination: institutions collectively maintain a standard that enables communication and document retention across generations. No extraction mechanism is present — the constraint solves a genuine collective action problem without asymmetric benefit.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(continuity_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(continuity_reading, TR),
    TR >= 0.70.

:- end_tests(continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low. The continuity reading treats the standard as emerging from genuine institutional practice with low overhead for coordination. Medieval users benefit from the ability to communicate across time and space; the standard enables rather than restricts their primary function. Suppression exists (manuscript rejection, social penalty for egregious error) but is moderate (0.35), not severe — the standard is flexible enough to accommodate variation while maintaining intelligibility. Theater ratio (0.42): Moderate-low. The constraint has some performative content (claims of continuity with antiquity carry legitimacy value), but the performance is not the primary function. The standard genuinely coordinates communication; the claim of continuity is secondary to the practical coordination it enables. Over the interval (0-600 years, roughly 400-1000 CE), theater_ratio increases slightly (0.35 to 0.48) as the historical distance to Classical antiquity grows and the claim of continuity becomes more performative relative to the living practice. Extractiveness also increases slightly (0.22 to 0.32) as the standard ossifies slightly and gap between claim ('continuity with Classical standards') and practice ('evolved medieval form') widens — but even the endpoint remains well within Rope territory. The measurements capture the normal degradation pattern of any living standard: as institutions grow and distance increases, the theater increases and some extractiveness emerges, but the core coordination function persists.
 *
 * PERSPECTIVAL GAP:
 *   The institutional user and analytical observer both see Rope — the standard genuinely coordinates communication. The scribe sees Rope with some suppression burden — learning the standard requires discipline, but the learning is functional, not arbitrary. The non-lettered laity see Tangled Rope — they benefit from intelligible liturgy but are excluded from written participation and bear the status cost of illiteracy. The reconstructionist observer (from outside this reading) sees Piton — the medieval claim to maintain Classical Latin is performative; the actual form is degraded. But the reconstructionist perspective is NOT internal to the continuity reading; it instantiates a different reading of the kernel. This JSON contains only the continuity reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The continuity reading assigns low directionality values (d near 0.2-0.4) because beneficiaries are the primary institutional users themselves — the Church and its scribes use the standard they maintain. Exit options are arbitrage (institutional) or constrained (scribal community), both relatively high mobility compared to trapped or identity_locked. There are no powerful external extractors; the constraint is endogenous to the institutions that maintain it. The non-lettered laity represent asymmetric extraction (high d, trapped exit), which elevates tangled_rope classification at the powerless perspective. But from the beneficiary and analytical perspectives, d remains low because the constraint's primary function is coordination of users who maintain it.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading resolves mandatrophy by avoiding the false choice between 'pure coordination' (Rope) and 'corruption of standards' (Tangled Rope or Snare). Both framings attempt to classify the same linguistic phenomenon (Latin evolution) using external reference points (Classical purity, original standard). The continuity reading steps outside this choice by treating medieval Latin not as an attempted copy of Classical Latin but as the living form of Latin in that era. From this position, the constraint is pure Rope with some spillover extraction (the laity are excluded from participation). The mandatrophy is resolved by recognizing that 'correct Latin' has no stable external referent — it is always 'the form that currently enables institutional communication.' By this definition, medieval Latin is correct, Renaissance Latin is correct, and modern ecclesiastical Latin is correct — each is correct for its era. The constraint type (Rope) is determined by whether the standard enables coordination (yes) and whether it extracts asymmetrically (minimal at institutional level, moderate at laity level), not by how well it matches an ancient ideal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_reconstruction_kernel,
    'What is the kernel claim about ''correct Latin''? Is it the living form that evolved naturally from Classical Latin (continuity reading), or is it the reconstructed Classical standard that medieval users imperfectly approximated (reconstruction reading)?',
    'This is the contested kernel. The continuity reading asserts that medieval Latin IS continuous with Classical Latin through normal language drift. The reconstruction reading asserts that medieval Latin is a degraded approximation of a fixed Classical standard. The sibling symbolic_reoccupation_reading treats ''correct Latin'' as a performative claim of legitimacy with no stable referent. Evidence: comparative analysis of phonological and morphological changes between Classical and medieval texts; identification of whether changes follow expected drift patterns or represent random corruption.',
    'Continuity reading: extractiveness ≈ 0.28, type = Rope. Reconstruction reading: extractiveness ≈ 0.55, type = Tangled Rope (beneficiaries are Classical scholars/ecclesiastical authorities claiming to preserve antiquity; victims are medieval users judged inadequate). Symbolic_reoccupation reading: extractiveness ≈ 0.62, type = Snare (the kernel claim is performative legitimation with no functional referent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_vs_reconstruction_kernel, conceptual, 'Whether medieval Latin is natural continuation or degraded copy of Classical standard').

omega_variable(
    drift_naturalness_threshold,
    'At what rate and degree of phonological/morphological change does ''natural language drift'' become ''corruption of the standard''?',
    'Comparative historical linguistics: identification of drift rates in other documented language communities (Romance languages post-Latin, Old to Middle English, Classical to Koine Greek). Determination of whether medieval Latin drift falls within expected parameters for a living language or represents anomalous degradation.',
    'If medieval Latin drift is within normal parameters: continuity reading is correct (Rope). If degradation exceeds normal drift: reconstruction reading is more accurate (Tangled Rope). The threshold is inherently subject to disciplinary convention — different schools of linguistics may draw this differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_naturalness_threshold, empirical, 'Rate and degree of change distinguishing natural drift from corruption').

omega_variable(
    institutional_claim_vs_linguistic_reality,
    'Does the medieval institutional claim to maintain ''correct Latin continuity'' genuinely describe the linguistic practice, or is it a legitimation narrative that obscures actual change?',
    'Discourse analysis of medieval metalinguistic commentary (grammarians'' statements about correctness, scribal notations about variant forms) vs. actual practice documented in manuscripts. Identification of the gap between what medieval scholars claimed they were doing and what they actually did.',
    'Small gap: the institutional claim is largely accurate — continuity reading is descriptively sound. Large gap: the institutional claim is performative (legitimation) rather than descriptive — symbolic_reoccupation reading becomes more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_claim_vs_linguistic_reality, empirical, 'Gap between medieval metalinguistic claims and actual practice').

omega_variable(
    beneficiary_identification_ambiguity,
    'Who actually benefits from the ''correct Latin continuity'' standard? Institutional users who gain coordination efficiency (rope beneficiary), or ecclesiastical authorities who gain legitimacy through claim of continuity with antiquity (potential false beneficiary)?',
    'Historical analysis of who shaped the standard, who enforced it, and who captured the prestige value. Determination of whether the standard evolved organically from institutional practice or was imposed top-down as a legitimation strategy.',
    'If organic evolution from practice: beneficiaries are authentic (medieval users), extractiveness remains low, Rope classification is correct. If top-down imposition with legitimacy-capture: beneficiaries may be primarily ecclesiastical authorities claiming continuity, extractiveness rises, Tangled Rope becomes more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Whether beneficiaries are genuine institutional users or legitimacy-capturing authorities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(continuity_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cont_tr_t0, continuity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cont_tr_t300, continuity_reading, theater_ratio, 300, 0.42).
narrative_ontology:measurement(cont_tr_t600, continuity_reading, theater_ratio, 600, 0.48).

% Extraction over time
narrative_ontology:measurement(cont_be_t0, continuity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cont_be_t300, continuity_reading, base_extractiveness, 300, 0.28).
narrative_ontology:measurement(cont_be_t600, continuity_reading, base_extractiveness, 600, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(continuity_reading, information_standard).
narrative_ontology:affects_constraint(continuity_reading, reconstruction_reading).
narrative_ontology:affects_constraint(continuity_reading, symbolic_reoccupation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the contested kernel 'correct_latin'. The continuity_reading asserts natural linguistic evolution (ε=0.28, Rope). The reconstruction_reading asserts degraded approximation of fixed Classical standard (ε≈0.55, Tangled Rope). The symbolic_reoccupation_reading asserts performative legitimation with no stable referent (ε≈0.62, Snare). All three read the same observable linguistic data (medieval Latin texts) but emit different classifications because they adopt different interpretive frames for the kernel claim. The decomposition is required by ε-invariance: if the same observable produces different ε values under different readings, the readings are structurally distinct constraints that must be modeled separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
