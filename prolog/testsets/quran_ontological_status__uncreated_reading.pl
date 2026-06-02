% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__uncreated_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_ontological_status__uncreated_reading
 *   human_readable: Qur'an as Uncreated Eternal Divine Speech (Kalām Qadīm Doctrine)
 *   domain: islamic_theology/philosophy_of_language/political_authority
 *
 * SUMMARY:
 *   The doctrine that the Qur'an is uncreated, eternal divine speech (kalām
 *   Allāh qadīm) coeternal with God is one of the most consequential
 *   theological claims in Islamic intellectual history. This reading
 *   instantiates the traditional Sunni Orthodox position, crystallized during
 *   the Mihnah (9th century) as Sunni orthodoxy's response to Mu'tazili
 *   rationalism, and codified in all four Sunni schools and Twelver Shi'ism.
 *   The doctrine creates a structural constraint on textual interpretation:
 *   if the Qur'an is eternally divine, not a created artifact, then its
 *   meaning is fixed and not subject to radical reinterpretation across time.
 *   This anchors jurists' authority (textual meaning is stable), privileges
 *   literalist and traditionalist schools (historical context becomes
 *   irrelevant), and forecloses the hermeneutical flexibility that reform
 *   movements require (contextual reinterpretation becomes theological
 *   error). The constraint exhibits the full six-type spectrum depending on
 *   observer position: a mountain for the believer whose faith is constituted
 *   through accepting the doctrine; a tangled rope for traditional jurists
 *   who benefit from the stability it provides while suppressing
 *   alternatives; a snare for rational theologians trapped by suppression; a
 *   rope for the orthodox coalition maintaining theological consensus; a
 *   piton for state enforcement machinery; and a tangled rope (with identity
 *   lock) for the analyst who sees the doctrine's contingency but cannot exit
 *   the tradition. This instantiates the kernel-reading frame: the same
 *   underlying commitment (revelation's ontological status) can be read as
 *   uncreated (this story), created (sibling constraint), or
 *   state-enforced-creation (another sibling), producing structurally
 *   distinct constraints with different beneficiary/victim sets.
 *
 * KEY AGENTS:
 *   - Traditional Jurists (Sunni legal schools): Institutional beneficiary (institutional/arbitrage) — doctrine stabilizes interpretive monopoly and textual authority
 *   - Literalist Schools (Salafi, Athari traditions): Beneficiary (organized/mobile) — doctrine privileges literal reading and forecloses metaphorical interpretation
 *   - Anti-Rationalist Theologians (Ash'ari, Maturidi orthodoxy): Beneficiary (institutional/arbitrage) — doctrine defends against Mu'tazili rational reconstruction of Islam
 *   - Rational Theologians (Mu'tazila legacy, modern reformers): Victim (powerful/constrained to trapped) — doctrine suppresses theological reinterpretation and flexibility
 *   - Metaphorical Interpreters (Sufis, philosophical schools): Victim (moderate/constrained) — doctrine privileges literal reading over symbolic/esoteric interpretation
 *   - Reform Movements (modernist Islam, progressive interpreters): Victim (organized/constrained) — doctrine prevents contextual reinterpretation required for contemporary application
 *   - State Enforcement Apparatus (religious affairs ministries, fatwā chains): Institutional beneficiary using doctrine for legitimacy (institutional/arbitrage) — doctrine grounds claim to enforce God's eternal law
 *   - Believer (person for whom faith is constituted through literalism): Identity-locked (powerless/identity_locked) — trapped in literalist frame; exit would require abandoning faith
 *   - Analytical Observer (Islamic theology scholar): Identity-locked analyst (analytical/identity_locked) — can see doctrine's contingency but identity-locked in tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.58).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.72).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, tangled_rope).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Qur'an as Uncreated Eternal Divine Speech (Kalām Qadīm Doctrine)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "islamic_theology/philosophy_of_language/political_authority").

domain_priors:requires_active_enforcement(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, 'bcefd4ab-a25e-4fa5-96d8-d419d5279be2').
narrative_ontology:cs_kernel_codification('bcefd4ab-a25e-4fa5-96d8-d419d5279be2', fixed_text).
narrative_ontology:cs_authority_grounding('bcefd4ab-a25e-4fa5-96d8-d419d5279be2', lineage).
narrative_ontology:cs_interpretation_layer_present('bcefd4ab-a25e-4fa5-96d8-d419d5279be2').
narrative_ontology:cs_reading_relation('bcefd4ab-a25e-4fa5-96d8-d419d5279be2', quran_ontological_status__created_reading, coexists_with).
narrative_ontology:cs_reading_relation('bcefd4ab-a25e-4fa5-96d8-d419d5279be2', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('bcefd4ab-a25e-4fa5-96d8-d419d5279be2', foundational, revelation_is_divine_ontologically).
narrative_ontology:cs_axiom_status(revelation_is_divine_ontologically, holdable).
narrative_ontology:cs_axiom_grounding('bcefd4ab-a25e-4fa5-96d8-d419d5279be2', revelation_is_divine_ontologically, deontological).
narrative_ontology:cs_axiom('bcefd4ab-a25e-4fa5-96d8-d419d5279be2', foundational, textual_meaning_is_fixed_by_divinity).
narrative_ontology:cs_axiom_status(textual_meaning_is_fixed_by_divinity, holdable).
narrative_ontology:cs_axiom_grounding('bcefd4ab-a25e-4fa5-96d8-d419d5279be2', textual_meaning_is_fixed_by_divinity, instrumental).
narrative_ontology:cs_reference_frame('bcefd4ab-a25e-4fa5-96d8-d419d5279be2', revelation_coeternal_with_divine_nature).
narrative_ontology:cs_drift_state('bcefd4ab-a25e-4fa5-96d8-d419d5279be2', contemporary_secular_modernity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bcefd4ab-a25e-4fa5-96d8-d419d5279be2', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_schools).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, political_authority_claims).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rational_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reform_movements).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, textual_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BELIEVER TRAPPED IN LITERALIST READING (MOUNTAIN) — From the standpoint of a believer whose faith is constituted through accepting the uncreated doctrine, the Qur'an's fixed eternal status appears as an immutable theological law. Exit from this reading would require abandoning the faith framework entirely. The constraint appears as an irreducible feature of divine nature, not a contingent institutional arrangement. Zero degrees of freedom; extraction perceived as zero because the believer's identity is fused with the doctrine.
constraint_indexing:constraint_classification(quran_ontological_status__uncreated_reading, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: TRADITIONAL JURISTS (TANGLED ROPE) — Institutional beneficiaries. The uncreated doctrine anchors jurisprudential authority: if the Qur'an is eternal divine speech, not a created artifact, then textual meaning is fixed and not subject to reinterpretation. Jurists benefit from this constraint through institutional stability and interpretive monopoly. Genuine coordination function: the doctrine stabilizes meaning transmission across centuries. Asymmetric extraction: the doctrine privileges literalist and traditionalist schools while foreclosing hermeneutical flexibility that reform movements require. Active enforcement through educational institutions, fatwā systems, and religious authority structures.
constraint_indexing:constraint_classification(quran_ontological_status__uncreated_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: RATIONAL THEOLOGIAN (SNARE) — Biographical timescale. Those who question whether the Qur'an is created face institutional suppression: loss of position, accusations of apostasy, social exclusion, potential legal sanction in regimes enforcing the uncreated doctrine. The extraction is severe because exit costs are maximal — changing one's position on this doctrine risks identity, livelihood, and legal status. Suppression (0.72) reflects the enforcement infrastructure (educational control, fatwā chains, state backing in some contexts). The rational theologian's actual structural position is that of a trapped victim with no coordination benefit.
constraint_indexing:constraint_classification(quran_ontological_status__uncreated_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE ENFORCEMENT MACHINERY (PITON) — The doctrine's enforcement via state institutions (religious affairs ministries, Al-Azhar institutional positions, state-appointed muftis) is substantially theatrical. The state uses the uncreated doctrine as a legitimacy claim ('we enforce God's eternal law') while the actual enforcement is administrative power. The apparatus persists through institutional inertia — the state benefits from the legitimacy claim, but the doctrine's functional role in governance has degraded as textual interpretation becomes increasingly specialized and institutional authority becomes increasingly contested. Theater ratio (0.65) reflects that enforcement relies heavily on performance of doctrinal orthodoxy rather than on the doctrine's internal coherence.
constraint_indexing:constraint_classification(quran_ontological_status__uncreated_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: SUNNI ORTHODOX CONSENSUS COALITION (ROPE) — Organized collective (traditional schools, clerical networks, scholarly consensus institutions) sees the uncreated doctrine as a coordination mechanism: it solves the problem of maintaining theological unity across geographically dispersed communities by anchoring meaning in a fixed text. The coalition experiences coordination benefits (shared hermeneutical framework) with relatively low coercive overhead within the coalition — agreement is sustained through institutional participation and social reinforcement rather than through state force. Exit options are mobile because membership in the coalition is voluntary (though socially pressured). Low effective extraction because the constraint solves a genuine coordination problem.
constraint_indexing:constraint_classification(quran_ontological_status__uncreated_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ISLAMIC TRADITION INSIDER (TANGLED ROPE / IDENTITY_LOCKED) — An analyst trained in Islamic theology who recognizes the uncreated doctrine's constructed status (historically contingent, Council of Mihnah-era invention, response to rationalist challenge) but is identity-locked in the tradition faces a structural contradiction. The analyst can see the doctrine is a contingent institutional arrangement, but exiting the identity frame that makes Islamic theology coherent would mean abandoning the analytical standpoint itself. This is the oracle gap instantiated: the analyst's native instruments (Islamic theological reasoning) cannot detect the structure that cross-tradition analysis reveals. Classified as tangled_rope (hybrid coordination/extraction) because the doctrine both stabilizes a genuine theological tradition AND constrains alternative readings that would emerge if the tradition were opened to secular philosophical critique.
constraint_indexing:constraint_classification(quran_ontological_status__uncreated_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quran_ontological_status__uncreated_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quran_ontological_status__uncreated_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, TR),
    TR >= 0.70.

:- end_tests(quran_ontological_status__uncreated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The doctrine extracts significant value for beneficiaries (jurists, literalist schools, state authority) by anchoring hermeneutical monopoly and suppressing competing interpretations. However, extraction is not maximal (snare-level ≥0.66) because genuine coordination function exists — the doctrine does solve the problem of maintaining theological consensus across geographically dispersed communities and centuries. The measurement trajectory shows rising extractiveness from 0.48 to 0.58 over the interval, indicating that as rationalist challenges fade and orthodoxy becomes institutionalized, extraction becomes more asymmetric (the crisis-response becomes routine institutional privilege). Suppression (0.72): High. Multiple enforcement mechanisms: institutional suppression through education systems, fatwā chains, and clerical appointments; social suppression through group identity and community pressure; legal suppression in regimes enforcing Sunni orthodoxy; identity-based suppression where believers' faith is constituted through literalism. The suppression trajectory rises from 0.62 to 0.72, indicating that enforcement infrastructure strengthened as the doctrine became institutionalized (the emergency measure of the Mihnah became permanent institutional machinery). Theater Ratio (0.65): Moderate-high. The doctrine is partly performative. States and institutions use it as a legitimacy claim ('we enforce God's eternal law') while actual enforcement is administrative power. Scholarly defense of the doctrine increasingly relies on philosophical argument (proving it is coherent with reason, not opposed to reason), suggesting the doctrine's brute-force assertion has eroded. The theater trajectory rises from 0.50 to 0.65, indicating increasing reliance on performative defense as the doctrine's self-evidence has declined in modern contexts. Tangled Rope classification confirmed: genuine coordination function (Rope gate) + asymmetric extraction with suppression (Snare markers) + active enforcement (Tangled Rope gate) = hybrid.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. Traditional jurists see stable coordination (Rope) — the doctrine anchors meaning and enables jurisprudential continuity. The Orthodox coalition sees consensus maintenance (Rope) — the doctrine coordinates disparate communities. State machinery sees legitimacy performance (Piton) — the doctrine is administratively convenient. The literal believer sees immutable law (Mountain) — the doctrine is a necessary feature of divine revelation. Rational theologians see coercive suppression (Snare) — the doctrine forecloses theological flexibility they require. Reform movements see institutional capture (Snare) — the doctrine prevents reinterpretation needed for modern contexts. The insider analyst sees a contingent institutional invention (Tangled Rope) — genuine coordination function exists, but extraction is asymmetric, victims are real, and alternatives are suppressed. The perspectival gap reveals that what appears as 'immutable divine law' to the believer is actually a historically contingent institutional arrangement benefiting specific groups and suppressing others.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) flows from structural position relative to the constraint. Beneficiaries (jurists, literalists, orthodox coalition, state apparatus) have low d (0.05–0.25 range), experiencing negative or minimal effective extraction — the constraint subsidizes them. Victims with constrained exit (rational theologians, reform movements) have high d (0.65–0.85 range) — they bear asymmetric costs but retain some agency and options. The trapped rational theologian (biographical perspective) has maximal d (0.95) — maximal extraction with no exit. The believer has d = 0.0 (full beneficiary) from within the literalist frame but would have d = 1.0 if the frame shifted and they recognized cost. The insider analyst has ambiguous d reflecting identity lock: structurally, they see the asymmetry (d ≈ 0.70), but identity fusion prevents full exit (exit_options: identity_locked rather than mobile). The doctrine's power to sustain itself derives from this directionality structure: beneficiaries experience low extraction and coordinate to maintain the doctrine; victims experience high extraction but lack exit options or organizational capacity; the insider analyst sees the full structure but cannot articulate it without exiting the tradition.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy RESOLVED via distinction between COORDINATION FUNCTION and ASYMMETRIC EXTRACTION. The doctrine genuinely solves coordination problem: maintaining theological unity across centuries and geographies requires some canonical reference point; the uncreated doctrine anchors that reference. This is a Rope-class coordination benefit. HOWEVER, the doctrine also extracts asymmetrically: it privileges literalist readings over metaphorical, traditionalist schools over rationalist, and state authority over individual hermeneutics. The victims (rational theologians, metaphorical interpreters, reform movements) cannot exit because suppression is institutional, social, and identity-based. This is Snare-class extraction. The constraint is legitimately Tangled Rope: it performs a genuine coordination function (Rope minimum), but with suppression ≥0.40 and beneficiary/victim asymmetry, it meets Tangled Rope criteria. The mandatrophy is that defenders of the doctrine can point to its coordination benefits (it does maintain theological consensus!) while victims point to its extraction (it does suppress competing interpretations!). Both are correct. The constraint is not a disguised snare (there is no way to maintain Islamic theology without some canonical text). It is not a disguised rope (alternatives are suppressed, not voluntarily foregone). It is genuinely hybrid — coordination mechanism + extraction mechanism intertwined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontic_vs_epistemological_status,
    'Is the uncreated doctrine a claim about the Qur''an''s ONTIC STATUS (it IS eternally divine speech coeternal with God) or an EPISTEMOLOGICAL FRAMEWORK (we must TREAT IT AS if uncreated to preserve revelatory authority)?',
    'Textual analysis of classical theological texts (al-Ghazali, Ibn Taymiyya, Ash''ari school formulations) distinguishing assertions of divine nature from pragmatic justifications for interpretive rigidity. Comparative analysis with Christian inspiration doctrine debates.',
    'If ontic: the constraint is a genuine metaphysical claim and the mountain perspective is defensible. If epistemological: the constraint is institutional pragmatism (tangled rope becomes the canonical reading), and alternative epistemologies (pragmatist, constructivist) become possible within Islamic theology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontic_vs_epistemological_status, conceptual, 'Whether uncreated doctrine is ontic or epistemological claim').

omega_variable(
    historical_contingency_of_doctrine,
    'The uncreated doctrine crystallized during the Mihnah (9th century inquisition under Caliph Al-Ma''mun) as a response to Mu''tazili rationalism. Does this historical origin indicate the doctrine is a CONTINGENT INSTITUTIONAL INVENTION rather than a necessary theological truth?',
    'Historical-textual scholarship on pre-Mihnah Islamic theology; identification of earlier revelation doctrines (did early Muslims hold different ontic theories?); analysis of whether non-Islamic revelatory traditions require the uncreated premise.',
    'If contingent: the doctrine is an intelligible response to a specific historical crisis (rationalist challenge) and is mutable given changed historical conditions. The snare and piton perspectives become dominant — victims and theater become visible. If necessary: the historical moment was revelation of a timeless truth, contingency is superficial, and the mountain perspective is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_contingency_of_doctrine, empirical, 'Historical contingency of uncreated doctrine crystallization').

omega_variable(
    metaphorical_interpretation_coherence,
    'Can metaphorical, contextual, or historical-linguistic interpretation of the Qur''an maintain theological coherence and prophetic authority WITHOUT the uncreated doctrine''s support? Do created-text readings necessarily lead to loss of revelatory status?',
    'Theological analysis of created-text frameworks (al-Jahiz, early Mu''tazila, modern reformers) examining whether they can sustain prophetic authority, textual sanctity, and ethical normativity. Comparative analysis with Christian hermeneutics (scripture as historically contingent yet revelatory).',
    'If coherent: the uncreated doctrine is not logically necessary for Islamic theology; it is one among multiple viable frameworks. Victims (reform movements, metaphorical interpreters) gain legitimacy. The constraint reclassifies as pure institutional extraction (snare). If incoherent: the uncreated doctrine is essential to preserving Islam''s theological structure, and victims'' grievances are misplaced — they seek to destabilize a necessary framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphorical_interpretation_coherence, conceptual, 'Whether created-text readings can sustain Islamic theological coherence').

omega_variable(
    reading_commission_ambiguity,
    'This story instantiates the UNCREATED reading of the qur''an_ontological_status kernel. The sibling created_reading and state_enforced_creation_reading represent alternative readings of the same kernel. What determines which reading a given Muslim community commits to — theological argument, political power, institutional incentives, or some combination?',
    'Historical-sociological analysis of which communities adopted which reading in which periods (Sunni orthodoxy vs. Mu''tazili rationalism vs. modern reformism); correlation with state power, clerical institution strength, and rationalist intellectual presence.',
    'If theological argument dominates: the uncreated reading should persuade on merit, suppression of alternatives reflects confusion. If power dominates: suppression is structural (snare/tangled_rope), and victims'' inability to exit is coercive. The resolution affects whether mandatrophy is resolved (does the constraint serve its stated function or primarily distribute power?).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_commission_ambiguity, empirical, 'What determines commitment to uncreated vs. created readings').

omega_variable(
    identity_locked_asymmetry,
    'The believer (Perspective 1) and the insider analyst (Perspective 6) are both identity-locked, but at different power levels and time horizons. The believer is trapped; the analyst is mobile within Islamic scholarship but identity-locked to the tradition. Are these two exit_options or one exit_option viewed from different contexts?',
    'Empirical case studies of scholars who shifted from literalist to metaphorical readings, or from tradition-centered to secular frameworks. Analysis of exit costs (institutional, social, identity) at each transition point.',
    'If two distinct exit options: the classification tuple is context-sensitive (what counts as ''trapped'' depends on which frame is operative). If one exit option observed from different standpoints: the analytic framework''s ability to model compound identity-locks is limited. This affects whether interpersonal constraints (mentorship relationships, family theological disputes) can be accurately modeled using the current exit_options vocabulary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_asymmetry, conceptual, 'Whether believer and analyst represent distinct exit options or same option viewed differently').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qur_uncr_tr_t0, quran_ontological_status__uncreated_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(qur_uncr_tr_t4, quran_ontological_status__uncreated_reading, theater_ratio, 4, 0.58).
narrative_ontology:measurement(qur_uncr_tr_t8, quran_ontological_status__uncreated_reading, theater_ratio, 8, 0.65).

% Extraction over time
narrative_ontology:measurement(qur_uncr_be_t0, quran_ontological_status__uncreated_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(qur_uncr_be_t4, quran_ontological_status__uncreated_reading, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(qur_uncr_be_t8, quran_ontological_status__uncreated_reading, base_extractiveness, 8, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(qur_uncr_su_t0, quran_ontological_status__uncreated_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(qur_uncr_su_t4, quran_ontological_status__uncreated_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(qur_uncr_su_t8, quran_ontological_status__uncreated_reading, suppression_requirement, 8, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__state_enforced_creation_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, prophetic_authority_and_textual_interpretation).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, rationalism_vs_traditionalism_in_islamic_theology).

% DUAL FORMULATION NOTE:
% The qur'an_ontological_status kernel generates three distinct constraint stories: uncreated_reading (this file), created_reading (sibling), and state_enforced_creation_reading (sibling). Each story has its own ε value reflecting empirical confidence in that reading's historical accuracy and contemporary viability. The uncreated_reading has ε=0.58 (tangled rope) because it is empirically well-established as the dominant Sunni Orthodox doctrine with genuine coordination benefits. The created_reading would have lower ε (more contested, less institutionalized in majority tradition). The state_enforced_creation_reading would be highest ε as snare (most extractive, least coordination). All three are linked via network.affects_constraints to show they are siblings in the same kernel dispute, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__uncreated_reading, institutional, 0.05).
constraint_indexing:directionality_override(quran_ontological_status__uncreated_reading, analytical, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
