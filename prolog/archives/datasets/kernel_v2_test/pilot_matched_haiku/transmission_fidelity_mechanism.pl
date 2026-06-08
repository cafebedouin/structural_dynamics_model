% ============================================================================
% CONSTRAINT STORY: transmission_fidelity_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transmission_fidelity_mechanism, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: transmission_fidelity_mechanism
 *   human_readable: Transmission Fidelity Mechanism in Halakhic Authority
 *   domain: religious_law/commitment_systems/institutional_authority
 *
 * SUMMARY:
 *   The transmission fidelity mechanism in halakhic authority represents a
 *   constraint that preserves minute procedural details of Temple sacrifice
 *   law across 1,900 years with zero enactment. The mechanism operates
 *   through yeshiva curriculum structure, manuscript transmission practices,
 *   and rabbinic adjudication on non-performable questions. This constraint
 *   exhibits the full range of DR classification from different perspectives,
 *   making it a diagnostic exemplar for how commitment systems maintain
 *   authority structures across historical discontinuity. The same structural
 *   phenomenon — the preservation of sacrifice law knowledge without
 *   performance — appears as an immutable natural law of institutional memory
 *   (mountain), a genuine coordination mechanism for preserving religious
 *   knowledge (rope), a mixed coordination-extraction hybrid (tangled rope),
 *   a temporary infrastructure maintaining readiness for messianic
 *   restoration (scaffold), a degraded ritual maintained through inertia
 *   (piton), or pure extraction through impossible obligation (snare),
 *   depending on the observer's structural position and reading of the
 *   contested kernel. The constraint's theater ratio (0.78) reflects that
 *   halakhic adjudication on non-performable questions is substantially
 *   performative: the procedures are studied and debated with intellectual
 *   rigor, but the performance never occurs. The extractiveness (0.35) is
 *   moderate because the mechanism does solve a genuine coordination problem
 *   (preserving knowledge and authority across diaspora) while also
 *   benefiting rabbinic authority through interpretive monopoly. The
 *   suppression (0.48) reflects that participation in the transmission
 *   mechanism is enforced through community ties, identity fusion, and
 *   institutional authority, but not through coercive mechanisms.
 *
 * KEY AGENTS:
 *   - Literal Obligation Bearer (powerless/identity_locked): Bears the cost of an unfulfilled command; identity fused with the halakhic obligation; structurally mobile but cannot exit without abandoning identity frame
 *   - Yeshiva Student (moderate/constrained): Participates in genuine coordination (knowledge transmission) while bearing extraction through time investment and opportunity cost; constrained by community ties and career path dependence
 *   - Rabbinic Interpretive Authority (institutional/arbitrage): Benefits from the transmission mechanism through institutional continuity and interpretive monopoly; has arbitrage options (can redefine obligation modality, authorize suspension, shift interpretation)
 *   - Halakhic Adjudication System (institutional/arbitrage): Maintains elaborate procedures for non-performable questions; sees own function as degraded; maintained through institutional inertia and theatrical performance
 *   - Messianic Restoration Coalition (organized/mobile): Sees transmission mechanism as temporary infrastructure maintaining readiness for restoration; has agency and sees exit path (restoration); experiences constraint as scaffold
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent institutional arrangements as immutable features of institutional memory; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transmission_fidelity_mechanism, 0.35).
domain_priors:suppression_score(transmission_fidelity_mechanism, 0.48).
domain_priors:theater_ratio(transmission_fidelity_mechanism, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transmission_fidelity_mechanism, extractiveness, 0.35).
narrative_ontology:constraint_metric(transmission_fidelity_mechanism, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(transmission_fidelity_mechanism, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transmission_fidelity_mechanism, piton).
narrative_ontology:human_readable(transmission_fidelity_mechanism, "Transmission Fidelity Mechanism in Halakhic Authority").
narrative_ontology:topic_domain(transmission_fidelity_mechanism, "religious_law/commitment_systems/institutional_authority").

domain_priors:requires_active_enforcement(transmission_fidelity_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transmission_fidelity_mechanism, '2ee7d71a-3a12-47e1-86aa-eec2762f1671').
narrative_ontology:cs_kernel_codification('2ee7d71a-3a12-47e1-86aa-eec2762f1671', fixed_text).
narrative_ontology:cs_authority_grounding('2ee7d71a-3a12-47e1-86aa-eec2762f1671', lineage).
narrative_ontology:cs_interpretation_layer_present('2ee7d71a-3a12-47e1-86aa-eec2762f1671').
narrative_ontology:cs_reading_relation('2ee7d71a-3a12-47e1-86aa-eec2762f1671', transmission_fidelity_mechanism__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('2ee7d71a-3a12-47e1-86aa-eec2762f1671', transmission_fidelity_mechanism__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ee7d71a-3a12-47e1-86aa-eec2762f1671', transmission_fidelity_mechanism__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('2ee7d71a-3a12-47e1-86aa-eec2762f1671', foundational, rabbinic_authority_can_redefine_mitzvah_modality).
narrative_ontology:cs_axiom_status(rabbinic_authority_can_redefine_mitzvah_modality, holdable).
narrative_ontology:cs_axiom_grounding('2ee7d71a-3a12-47e1-86aa-eec2762f1671', rabbinic_authority_can_redefine_mitzvah_modality, deontological).
narrative_ontology:cs_axiom('2ee7d71a-3a12-47e1-86aa-eec2762f1671', foundational, study_constitutes_genuine_exercise_of_obligation).
narrative_ontology:cs_axiom_status(study_constitutes_genuine_exercise_of_obligation, holdable).
narrative_ontology:cs_axiom_grounding('2ee7d71a-3a12-47e1-86aa-eec2762f1671', study_constitutes_genuine_exercise_of_obligation, deontological).
narrative_ontology:cs_axiom('2ee7d71a-3a12-47e1-86aa-eec2762f1671', secondary, obligation_is_fulfilled_through_intellectual_engagement).
narrative_ontology:cs_axiom_status(obligation_is_fulfilled_through_intellectual_engagement, holdable).
narrative_ontology:cs_axiom_grounding('2ee7d71a-3a12-47e1-86aa-eec2762f1671', obligation_is_fulfilled_through_intellectual_engagement, instrumental).
narrative_ontology:cs_reference_frame('2ee7d71a-3a12-47e1-86aa-eec2762f1671', study_as_exercise_fulfillment).
narrative_ontology:cs_drift_state('2ee7d71a-3a12-47e1-86aa-eec2762f1671', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2ee7d71a-3a12-47e1-86aa-eec2762f1671', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transmission_fidelity_mechanism, rabbinic_interpretive_authority).
narrative_ontology:constraint_victim(transmission_fidelity_mechanism, literal_obligation_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transmission_fidelity_mechanism, yeshiva_student).
narrative_ontology:constraint_beneficiary(transmission_fidelity_mechanism, messianic_restoration_coalition).
narrative_ontology:constraint_victim(transmission_fidelity_mechanism, literal_obligation_bearer).
narrative_ontology:constraint_victim(transmission_fidelity_mechanism, yeshiva_student).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the halakhic obligation to perform Temple sacrifice as a binding command. Under the performance-only reading, this obligation is unfulfilled and unfulfillable. The obligation is constitutive of Jewish religious identity — exiting the tradition means abandoning the identity frame that makes the obligation meaningful. Bears the cost of an impossible command with no authorized exit mechanism under this reading.
narrative_ontology:constraint_stakeholder(transmission_fidelity_mechanism, literal_obligation_bearer, payer,
    powerless, biographical, identity_locked, global).

% Participates in yeshiva study of halakhic procedures for Temple sacrifice. Invests significant time and intellectual effort in learning procedures that will never be applied. Benefits from genuine intellectual engagement and community belonging. Constrained by family expectations, community ties, and career path dependence within the tradition. Can exit but at significant social and economic cost.
narrative_ontology:constraint_stakeholder(transmission_fidelity_mechanism, yeshiva_student, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(transmission_fidelity_mechanism, yeshiva_student, beneficiary).

% Maintains and enforces the transmission mechanism through yeshiva curriculum, manuscript copying, and halakhic adjudication. Benefits from institutional continuity and interpretive monopoly on what counts as fulfillment of the obligation. Has arbitrage options: can redefine the obligation's modality (study-as-exercise reading), authorize suspension (messianic-suspension reading), or shift interpretation. Controls the mechanism and experiences it as enabling rather than extractive.
narrative_ontology:constraint_stakeholder(transmission_fidelity_mechanism, rabbinic_interpretive_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transmission_fidelity_mechanism, rabbinic_interpretive_authority, beneficiary).

% Maintains elaborate procedures for adjudicating questions about non-performable sacrifices (which birds are valid, how to calculate priestly portions, proper slaughter technique). The procedures are studied and debated with intellectual rigor but never applied. The system sees its own function as degraded — maintained through institutional inertia rather than operative function. Theater ratio is high because the entire apparatus is performative.
narrative_ontology:constraint_stakeholder(transmission_fidelity_mechanism, halakhic_adjudication_system, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Organized agents (Kabbalists, Hasidic movements, Temple restoration advocates) see the transmission mechanism as temporary infrastructure maintaining readiness for messianic restoration. The obligation is in abeyance, not violated. Study is instrumental preparation. Has agency and sees an exit path (restoration), making the constraint feel temporary rather than permanent. Benefits from the mechanism's preservation of knowledge for future restoration.
narrative_ontology:constraint_stakeholder(transmission_fidelity_mechanism, messianic_restoration_coalition, beneficiary,
    organized, generational, mobile, global).

% The Jewish collective benefits from the transmission mechanism's preservation of religious knowledge and cultural continuity across diaspora. The mechanism maintains Jewish identity and historical continuity. However, the collective is not an agent — it is a non-agent entity kept for narrative completeness. It does not collect rents from the mechanism and does not feed into directionality computation.
narrative_ontology:constraint_stakeholder(transmission_fidelity_mechanism, jewish_collective_identity, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(transmission_fidelity_mechanism, jewish_collective_identity).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserve halakhic knowledge and rabbinic authority structure across diaspora and historical discontinuity. Maintain intellectual engagement with religious law. Prepare for potential messianic restoration of Temple service.
% TRANSFER_FUNCTION: The mechanism transfers time, intellectual effort, and community participation from yeshiva students and obligation bearers to rabbinic authority (institutional continuity and interpretive monopoly). It transfers knowledge from one generation to the next through manuscript copying and curriculum transmission. Under the performance-only reading, it transfers the cost of an unfulfilled obligation to all obligation bearers.
% ABSENT_VOICES: Those who have left the tradition (apostates, secular Jews, Reform Jews) would object to the performance-only reading and the claim that the obligation remains binding. Those who hold the symbolic-archive reading (secular scholars, cultural Jews) would object to the claim that the mechanism fulfills a binding halakhic obligation. Those who hold the messianic-suspension reading would object to the performance-only reading's claim that the obligation is currently violated.
% DISAPPEARANCE_RATIONALE: Under the study-as-exercise reading, if the transmission mechanism disappeared, the obligation would be violated (study would cease to fulfill the mitzvah) and the world would rearrange itself (rabbinic authority would lose its interpretive monopoly, Jewish identity would be disrupted). Under the performance-only reading, if the mechanism disappeared, the obligation would remain unfulfilled (nothing would change in the world's arrangement because the obligation is already unfulfilled). Under the messianic-suspension reading, if the mechanism disappeared, the readiness for restoration would be lost (the world would rearrange itself when the Temple is restored and the knowledge is no longer available). Under the symbolic-archive reading, if the mechanism disappeared, cultural continuity would be disrupted but no binding obligation would be violated.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the halakhic obligation to perform Temple sacrifice became impossible to fulfill. The founding problem was: how to preserve the knowledge of sacrifice law and maintain the authority structure of rabbinic Judaism across diaspora and historical discontinuity, given that the primary function (performing sacrifice) is no longer possible?
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authority attests that the founding problem (preserving knowledge and authority across diaspora) remains live and that the transmission mechanism successfully addresses it. Messianic restoration advocates attest that the founding problem (maintaining readiness for restoration) remains live. Secular scholars attest that the founding problem (preserving cultural continuity) remains live. However, those who hold the performance-only reading attest that the founding problem (the unfulfilled obligation) remains unresolved and that the transmission mechanism does not address it. Those who hold the symbolic-archive reading attest that the founding problem (cultural preservation) is addressed but that the mechanism does not fulfill a binding obligation.
narrative_ontology:disappearance_verdict(transmission_fidelity_mechanism, contested).
narrative_ontology:founding_problem_status(transmission_fidelity_mechanism, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITERAL OBLIGATION BEARER (SNARE) — Identity fused with the halakhic obligation to perform Temple sacrifice. Structurally mobile (could leave the tradition) but identity-locked: the obligation is constitutive of Jewish religious identity. Cannot exit without abandoning the identity frame that makes the obligation meaningful. Bears the cost of an unfulfilled command with no authorized exit mechanism under this reading.
constraint_indexing:constraint_classification(transmission_fidelity_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: YESHIVA STUDENT (TANGLED ROPE) — Participates in genuine coordination (transmission of halakhic knowledge across generations) while bearing extraction through time investment and opportunity cost. Study of sacrifice law is both meaningful intellectual engagement and performative maintenance of a system that benefits rabbinic authority. Exit is constrained by community ties, family expectations, and career path dependence within the tradition.
constraint_indexing:constraint_classification(transmission_fidelity_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RABBINIC INTERPRETIVE AUTHORITY (ROPE) — Benefits from the transmission mechanism through institutional continuity and interpretive monopoly. The constraint solves a genuine coordination problem: how to preserve halakhic knowledge and authority structure across diaspora and historical discontinuity. Rabbinic authority has arbitrage options (can redefine the obligation's modality, can authorize suspension, can shift interpretation) and experiences the constraint as enabling rather than extractive.
constraint_indexing:constraint_classification(transmission_fidelity_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HALAKHIC ADJUDICATION SYSTEM (PITON) — The system maintains elaborate procedures for adjudicating questions about non-performable sacrifices (e.g., which birds are valid for which offerings, how to calculate priestly portions, proper slaughter technique) with zero functional enactment. The procedural detail is preserved through institutional inertia and theatrical maintenance. The system sees its own function as degraded — the procedures are studied and debated but never applied. Theater ratio is high because the entire apparatus is performative: the questions are real, the answers are rigorous, but the performance never occurs.
constraint_indexing:constraint_classification(transmission_fidelity_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MESSIANIC RESTORATION COALITION (SCAFFOLD) — Organized agents (Kabbalists, Hasidic movements, contemporary Temple restoration advocates) see the transmission mechanism as temporary infrastructure maintaining readiness for messianic restoration. The obligation is in abeyance, not violated. Study is instrumental preparation. This perspective has a sunset: when the Temple is restored, the transmission mechanism's function shifts from preservation to operational readiness. The coalition has agency and sees an exit path (restoration), making the constraint feel temporary rather than permanent.
constraint_indexing:constraint_classification(transmission_fidelity_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the transmission mechanism appears as an immutable feature of how religious traditions preserve knowledge across discontinuity. The fidelity of transmission across 1,900 years without enactment appears as a natural law of institutional memory. However, the structural data reveals this as a false summit: the transmission is maintained through active institutional work (yeshiva curriculum, manuscript copying, rabbinic authority enforcement), not through natural necessity. The constraint is contingent, not inevitable.
constraint_indexing:constraint_classification(transmission_fidelity_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transmission_fidelity_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transmission_fidelity_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transmission_fidelity_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(transmission_fidelity_mechanism, TR),
    TR >= 0.70.

:- end_tests(transmission_fidelity_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The transmission mechanism does solve a genuine coordination problem — preserving halakhic knowledge and rabbinic authority across diaspora and historical discontinuity. However, it also benefits rabbinic authority through interpretive monopoly and institutional continuity. The extractiveness is not as high as a pure snare (0.70+) because the mechanism genuinely coordinates knowledge transmission and enables intellectual engagement. The extractiveness is not as low as pure rope (0.10) because rabbinic authority benefits from maintaining the mechanism and has incentive to enforce participation. The moderate value reflects the mixed nature of the constraint. Suppression (0.48): Moderate. Participation in the transmission mechanism is enforced through community ties, family expectations, institutional authority, and identity fusion, but not through coercive legal mechanisms. The suppression has decreased over the interval (from 0.65 to 0.48) as alternative Jewish frameworks (Reform, secular, cultural Judaism) have emerged, reducing the monopoly of rabbinic authority. Theater ratio (0.78): High and increasing. Halakhic adjudication on non-performable questions is substantially performative — the procedures are studied with intellectual rigor but never applied. The theater ratio has increased over the interval (from 0.55 to 0.78) as the functional gap between knowledge and performance has widened. The increasing theater ratio is diagnostic of piton classification: the function has atrophied but the performance is maintained through institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single set of base properties. The literal obligation bearer sees snare — an unfulfilled command with no authorized exit mechanism (under the performance-only reading). The yeshiva student sees tangled rope — genuine intellectual coordination mixed with extraction through time investment and opportunity cost. Rabbinic authority sees rope — a coordination mechanism that solves the problem of preserving knowledge and authority across diaspora. The halakhic adjudication system sees piton — its own procedures are degraded, maintained through inertia rather than function. The messianic restoration coalition sees scaffold — a temporary infrastructure with a sunset (restoration). The analytical observer risks seeing mountain — naturalizing the transmission mechanism as an immutable feature of institutional memory. The perspectival gap is driven by the contested kernel: different readings of the sacrifice obligation produce different classifications. Under the study-as-exercise reading, extractiveness drops and the constraint becomes rope. Under the performance-only reading, extractiveness rises and the constraint becomes snare. Under the messianic-suspension reading, the constraint becomes scaffold. Under the symbolic-archive reading, the constraint becomes rope (voluntary cultural practice). The engine's classification will depend on which reading is adopted.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position relative to the transmission mechanism. The literal obligation bearer (powerless/identity_locked) experiences maximum extraction because they bear the cost of an unfulfilled obligation with no authorized exit mechanism under the performance-only reading. The yeshiva student (moderate/constrained) experiences moderate extraction because they participate in genuine coordination (knowledge transmission) while bearing opportunity costs. Rabbinic authority (institutional/arbitrage) experiences low or negative extraction because they benefit from the mechanism through interpretive monopoly and institutional continuity. The messianic restoration coalition (organized/mobile) experiences low extraction because they have agency and see an exit path (restoration). The directionality derivation is complicated by the contested kernel: under different readings, the same agent's directionality changes. Under the study-as-exercise reading, the literal obligation bearer's directionality drops because the obligation is fulfilled through study. Under the performance-only reading, the literal obligation bearer's directionality rises because the obligation remains unfulfilled. The engine's directionality computation will depend on which reading is adopted and how the beneficiary/victim declarations are interpreted.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The transmission mechanism's mandate was to preserve halakhic knowledge and authority structure across diaspora and historical discontinuity. This mandate has been fulfilled — the knowledge has been preserved with remarkable fidelity across 1,900 years. However, the mechanism's original function (maintaining readiness for Temple sacrifice performance) has become impossible due to historical discontinuity (Temple destruction, diaspora, loss of Temple service). The mechanism persists not because its original function is still operative, but because it serves secondary functions: preserving Jewish identity, maintaining rabbinic authority, enabling intellectual engagement, and (under the messianic-suspension reading) maintaining readiness for restoration. The piton classification reflects this mandatrophy: the mechanism's primary function has atrophied, but the performance is maintained through institutional inertia. The theater ratio (0.78) is diagnostic of this mandatrophy — the procedures are studied with intellectual rigor but never applied. The increasing theater ratio over the interval (from 0.55 to 0.78) reflects the widening gap between knowledge and performance. The constraint resolves the mandatrophy by showing that the mechanism serves multiple functions simultaneously: coordination (knowledge transmission), identity preservation, authority maintenance, and (under some readings) preparation for restoration. The mandatrophy is not a failure of the mechanism but a structural feature of how religious traditions adapt to historical discontinuity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    obligation_modality_transformation,
    'Can rabbinic authority legitimately transform the modality of a Torah-commanded mitzvah from physical performance to intellectual study, or does such transformation constitute unauthorized reinterpretation?',
    'Textual analysis of Talmudic sources (Menachot 110a and related passages) establishing the scope of rabbinic authority; comparison with other transformed mitzvot (e.g., Sukkot observance in diaspora); examination of whether the transformation is presented as temporary accommodation or permanent redefining of the obligation',
    'If transformation is legitimate: the study-as-exercise reading is structurally sound, extractiveness drops to near-zero, and the constraint becomes rope (coordination) rather than snare (extraction). If transformation exceeds rabbinic authority: the obligation remains unfulfilled, extractiveness remains high, and the constraint is snare (extraction masked as coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(obligation_modality_transformation, conceptual, 'Whether rabbinic authority can legitimately transform mitzvah modality from performance to study').

omega_variable(
    suspension_versus_violation,
    'Is the 1,900-year absence of Temple sacrifice a divinely authorized suspension of the obligation, or an ongoing violation of an active command?',
    'Theological analysis of implicit divine accommodation (would God command the impossible?); examination of whether suspension is presented as temporary or permanent; comparison with other suspended mitzvot (e.g., Temple service, priestly service); analysis of whether the obligation is described as ''in abeyance'' or ''unfulfilled''',
    'If suspension is authorized: the obligation is not violated, extractiveness is low, and the constraint is rope or scaffold (coordination/preparation). If suspension is not authorized: the obligation is violated, extractiveness is high, and the constraint is snare (extraction through impossible command).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_versus_violation, conceptual, 'Whether the absence of sacrifice is authorized suspension or ongoing violation').

omega_variable(
    beneficiary_identification_ambiguity,
    'Who benefits from the transmission mechanism: rabbinic authority (institutional monopoly on interpretation), the Jewish collective (preservation of identity and knowledge), or neither (the mechanism is purely performative with no real beneficiary)?',
    'Analysis of institutional power flows: does rabbinic authority gain authority/legitimacy/resources from maintaining the transmission mechanism? Does the Jewish collective gain identity/continuity/meaning? Or is the mechanism maintained purely through institutional inertia with no real beneficiary?',
    'If rabbinic authority benefits: the constraint is tangled rope or snare (extraction masked as coordination). If the collective benefits: the constraint is rope (genuine coordination). If neither benefits: the constraint is piton (pure theater with no functional beneficiary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Who benefits from maintaining the transmission mechanism').

omega_variable(
    theater_ratio_measurement_validity,
    'Does the high theater ratio (0.78) accurately capture the performative nature of halakhic adjudication on non-performable questions, or does it underestimate the genuine intellectual and spiritual value of the study itself?',
    'Ethnographic analysis of yeshiva study: what proportion of time is spent on questions with no possible application vs. questions with potential application? What proportion of student motivation is intellectual engagement vs. obligation fulfillment vs. institutional participation? Comparison with other scholarly traditions (e.g., medieval Christian theology on non-applicable questions).',
    'If theater ratio is accurate: the constraint is piton (degraded function maintained through inertia). If theater ratio is too high: the constraint may be rope (genuine intellectual coordination) or tangled rope (mixed coordination and extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_measurement_validity, empirical, 'Whether theater ratio accurately captures performative nature of halakhic study').

omega_variable(
    identity_lock_mechanism_specificity,
    'Is the identity-lock binding the literal obligation bearer primarily through religious identity fusion, or through social/community identity, or through both equally?',
    'Analysis of exit narratives: when individuals leave the tradition, what identity elements do they report losing? Do they describe the obligation as intrinsic to their religious identity, or as embedded in community belonging, or both? Comparison of exit costs for those with strong religious identity vs. strong community identity.',
    'If primarily religious identity: the identity-lock is cognitive/theological and may shift if the reading changes (e.g., if study-as-exercise reading becomes dominant). If primarily community identity: the identity-lock is social and may persist even if the reading changes. If both: the lock is overdetermined and highly resistant to change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_specificity, empirical, 'Specificity of identity-lock mechanism binding obligation bearer').

omega_variable(
    false_summit_natural_law_claim,
    'Is the transmission mechanism''s 1,900-year fidelity a natural law of institutional memory, or a contingent institutional achievement maintained through active work?',
    'Historical analysis: what would happen if rabbinic authority ceased enforcing the transmission mechanism? Would the knowledge persist through other channels, or would it degrade? Comparison with other religious traditions'' transmission mechanisms and their failure modes. Analysis of the resources (yeshivas, manuscript copying, rabbinic authority) required to maintain the mechanism.',
    'If natural law: the mountain classification is correct, and the constraint is immutable. If contingent: the mountain classification is a false summit, and the constraint is maintained through active institutional work (tangled rope or piton). The engine''s false summit detector will reclassify based on the beneficiary declaration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, empirical, 'Whether transmission fidelity is natural law or contingent institutional achievement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transmission_fidelity_mechanism, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(transfid_theater_t0, transmission_fidelity_mechanism, theater_ratio, 0, 0.55).
narrative_ontology:measurement(transfid_theater_t500, transmission_fidelity_mechanism, theater_ratio, 500, 0.68).
narrative_ontology:measurement(transfid_theater_t1000, transmission_fidelity_mechanism, theater_ratio, 1000, 0.75).
narrative_ontology:measurement(transfid_theater_t1500, transmission_fidelity_mechanism, theater_ratio, 1500, 0.78).

% Extraction over time
narrative_ontology:measurement(transfid_extract_t0, transmission_fidelity_mechanism, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(transfid_extract_t500, transmission_fidelity_mechanism, base_extractiveness, 500, 0.38).
narrative_ontology:measurement(transfid_extract_t1000, transmission_fidelity_mechanism, base_extractiveness, 1000, 0.36).
narrative_ontology:measurement(transfid_extract_t1500, transmission_fidelity_mechanism, base_extractiveness, 1500, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(transfid_suppress_t0, transmission_fidelity_mechanism, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(transfid_suppress_t500, transmission_fidelity_mechanism, suppression_requirement, 500, 0.58).
narrative_ontology:measurement(transfid_suppress_t1000, transmission_fidelity_mechanism, suppression_requirement, 1000, 0.52).
narrative_ontology:measurement(transfid_suppress_t1500, transmission_fidelity_mechanism, suppression_requirement, 1500, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transmission_fidelity_mechanism, identity_coordination).
narrative_ontology:boltzmann_floor_override(transmission_fidelity_mechanism, 0.12).
narrative_ontology:affects_constraint(transmission_fidelity_mechanism, rabbinic_authority_legitimacy).
narrative_ontology:affects_constraint(transmission_fidelity_mechanism, jewish_identity_continuity).
narrative_ontology:affects_constraint(transmission_fidelity_mechanism, temple_restoration_readiness).

% DUAL FORMULATION NOTE:
% The transmission fidelity mechanism is downstream of the contested kernel (sacrifice obligation) but represents a distinct structural constraint. The upstream kernel has multiple readings with different extractiveness values; the transmission mechanism has its own extractiveness reflecting the institutional work required to preserve knowledge across diaspora. The mechanism's function differs under each reading of the kernel, but the mechanism itself is a single constraint with measurable theater ratio, suppression, and extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transmission_fidelity_mechanism, powerless, 0.85).
constraint_indexing:directionality_override(transmission_fidelity_mechanism, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
