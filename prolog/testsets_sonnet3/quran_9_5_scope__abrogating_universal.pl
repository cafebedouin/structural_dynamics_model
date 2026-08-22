% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: The Abrogating-Universalist Reading of Verse 9:5 (Sword Verse) as Standing Offensive Mandate
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   This story authors ONE reading — the abrogating-universalist reading — of
 *   a contested kernel over the legal scope of Quran 9:5. Under this reading,
 *   the verse permanently cancels (via naskh) the roughly 124 earlier verses
 *   counseling patience, non-aggression, or defensive-only warfare,
 *   converting what those verses describe as bounded, provoked conflict into
 *   a standing legal obligation of offensive jihad against all non-Muslims
 *   until submission, conversion, or payment of jizya. This is the reading
 *   actively deployed by contemporary jihadist movements and by clerical
 *   authorities who benefit from providing it. It is NOT the majority
 *   position among contemporary Islamic jurists, who overwhelmingly favor
 *   either the contextual-defensive reading (9:5 addresses specific
 *   treaty-breaking Meccan tribes, does not abrogate) or the
 *   progressive-synthesis reading (9:5 is time-bound, ethical trajectory
 *   supersedes literalist application). Those are separate constraints, not
 *   alternative measurements of this one — per the epsilon-invariance
 *   principle, decomposition into three linked stories is required because
 *   the three readings have structurally distinct beneficiary/victim sets,
 *   extraction profiles, and enforcement mechanisms. This file authors only
 *   the abrogating-universalist reading, assessed by that reading's own
 *   lights: extraction here refers to what THIS reading's own operation
 *   extracts from those it targets, not to Quranic ethics as a whole and not
 *   to the reading's own self-justification (which would claim zero
 *   extraction, being framed as righteous obligation).
 *
 * KEY AGENTS:
 *   - salafi_jihadist_clerical_authorities: agenda_setter (institutional/arbitrage) — issue and propagate the abrogation ruling
 *   - expansionist_jihadist_movements: beneficiary/agenda_setter (organized/arbitrage) — operationalize the doctrine as conquest justification
 *   - authoritarian_theocratic_regimes: beneficiary (institutional/mobile) — invoke doctrine selectively for state legitimation
 *   - non_muslim_populations_under_expansion: payer (powerless/trapped) — primary targets under the doctrine's terms
 *   - religious_minorities_in_contested_territories: payer (powerless/trapped) — bear enslavement, displacement, execution
 *   - muslim_reformist_scholars: excluded (moderate/constrained) — contest the doctrine, face apostasy accusations
 *   - muslim_communities_coexisting_with_non_muslims: payer (moderate/constrained) — bear reputational and security costs
 *   - classical_and_contemporary_exegetes_of_dissenting_readings: observer (analytical) — document the contested textual history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.86).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.88).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.86).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, tangled_rope).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "The Abrogating-Universalist Reading of Verse 9:5 (Sword Verse) as Standing Offensive Mandate").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "religious/political/legal").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, '0b1e2dae-f3f6-46d6-beef-8523c31d2b82').
narrative_ontology:cs_kernel_codification('0b1e2dae-f3f6-46d6-beef-8523c31d2b82', fixed_text).
narrative_ontology:cs_authority_grounding('0b1e2dae-f3f6-46d6-beef-8523c31d2b82', lineage).
narrative_ontology:cs_interpretation_layer_present('0b1e2dae-f3f6-46d6-beef-8523c31d2b82').
narrative_ontology:cs_reading_relation('0b1e2dae-f3f6-46d6-beef-8523c31d2b82', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('0b1e2dae-f3f6-46d6-beef-8523c31d2b82', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('0b1e2dae-f3f6-46d6-beef-8523c31d2b82', foundational, naskh_universally_cancels_prior_peaceful_verses).
narrative_ontology:cs_axiom_status(naskh_universally_cancels_prior_peaceful_verses, holdable).
narrative_ontology:cs_axiom_grounding('0b1e2dae-f3f6-46d6-beef-8523c31d2b82', naskh_universally_cancels_prior_peaceful_verses, conventional).
narrative_ontology:cs_axiom('0b1e2dae-f3f6-46d6-beef-8523c31d2b82', foundational, polytheist_status_alone_triggers_standing_combat_obligation).
narrative_ontology:cs_axiom_status(polytheist_status_alone_triggers_standing_combat_obligation, holdable).
narrative_ontology:cs_axiom_grounding('0b1e2dae-f3f6-46d6-beef-8523c31d2b82', polytheist_status_alone_triggers_standing_combat_obligation, deontological).
narrative_ontology:cs_axiom('0b1e2dae-f3f6-46d6-beef-8523c31d2b82', secondary, quranic_command_is_atemporal_and_context_independent).
narrative_ontology:cs_axiom_status(quranic_command_is_atemporal_and_context_independent, holdable).
narrative_ontology:cs_axiom_grounding('0b1e2dae-f3f6-46d6-beef-8523c31d2b82', quranic_command_is_atemporal_and_context_independent, theological).
narrative_ontology:cs_created_at('0b1e2dae-f3f6-46d6-beef-8523c31d2b82', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, salafi_jihadist_clerical_authorities).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, authoritarian_theocratic_regimes).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslim_populations_under_expansion).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, religious_minorities_in_contested_territories).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, muslim_reformist_scholars).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, muslim_communities_coexisting_with_non_muslims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue rulings declaring 9:5 the final abrogating word (naskh al-sayf) that cancels roughly 124 earlier verses counseling patience, tolerance, or defensive-only combat. Administer the doctrine through fatwa networks, training curricula, and recruitment materials. Personally insulated from the violence they authorize; their authority and funding grow with the doctrine's acceptance.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, salafi_jihadist_clerical_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Cite the abrogating-universal reading as legal cover for territorial conquest, forced conversion campaigns, and jizya-extraction systems. The doctrine converts opportunistic violence into a standing religious obligation, recruiting fighters who believe they act under divine command rather than political ambition.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, expansionist_jihadist_movements, agenda_setter).

% Selectively invoke the universalist abrogation doctrine to legitimate state violence, suppress internal dissent as apostasy-adjacent, and mobilize populations against external enemies framed as permanent religious targets. Can moderate or intensify invocation depending on diplomatic needs.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, authoritarian_theocratic_regimes, beneficiary,
    institutional, generational, mobile, national).

% Historically and in contemporary conflict zones, face the choice the doctrine frames as submission (dhimmi status and jizya), conversion, or armed conflict. No treaty status, prior peace, or neutrality is recognized as sufficient under this reading — the doctrine explicitly authorizes first-strike war absent formal capitulation.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslim_populations_under_expansion, payer,
    powerless, immediate, trapped, regional).

% Yazidis, Christians, and other minorities in territories controlled by movements applying this doctrine face enslavement, displacement, or execution justified by the reading's denial of any protected neutral status for unconverted polytheists or apostates-by-classification.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, religious_minorities_in_contested_territories, payer,
    powerless, biographical, trapped, regional).

% Argue on classical hermeneutic grounds (occasion of revelation, treaty context, the weight of contradicting hadith) that the abrogation is overbroad or invalid. Routinely denounced as apostates, deplatformed from mainstream religious institutions, or threatened when they contest the doctrine in jurisdictions where it holds sway.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, muslim_reformist_scholars, excluded,
    moderate, generational, constrained, global).

% Live in pluralistic societies where the abrogating-universal doctrine, when publicly associated with Islam, generates suspicion, backlash, and pressure to publicly repudiate a reading most do not hold. Bear reputational and physical-security costs generated by others' invocation of the doctrine.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, muslim_communities_coexisting_with_non_muslims, payer,
    moderate, biographical, constrained, national).

% Tabari, Ibn Kathir (in qualified form), and modern scholars of asbab al-nuzul document the Medinan treaty-breach context of 9:5 and note the classical jurisprudential dispute over whether wholesale abrogation of 124 verses is textually or logically sound. Their scholarship is available to any reader but does not itself enforce a reading.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, classical_and_contemporary_exegetes_of_dissenting_readings, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__abrogating_universal, diffuse).
narrative_ontology:fixing_cost_class(quran_9_5_scope__abrogating_universal, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At the level the reading claims for itself, it coordinates believers around a single unambiguous legal directive, removing the interpretive burden of weighing context-dependent verses against each other and providing a clear in-group/out-group boundary for collective religious-political action.
% TRANSFER_FUNCTION: Moves land, tribute (jizya), captives, and political sovereignty from non-Muslim and dissenting-Muslim populations to the movements and regimes that successfully invoke the doctrine; moves interpretive authority away from contextualist scholarship toward clerics who benefit from maximalist claims.
% ABSENT_VOICES: The vast majority of contemporary Muslim jurists across Hanafi, Shafi'i, Maliki, and Hanbali traditions who reject blanket abrogation are structurally absent from the doctrine's operational deployment — their scholarship exists but does not reach fighters recruited through parallel educational and propaganda channels controlled by the beneficiary movements. Non-Muslim populations targeted under the doctrine have no voice in its interpretation at all.
% DISAPPEARANCE_RATIONALE: If this specific reading lost its institutional and clerical scaffolding overnight, expansionist movements would lose their primary textual legitimation device, recruitment pipelines dependent on divine-mandate framing would need alternative justification, and the reformist/contextualist readings already dominant among most jurists would face no organized rival claiming exclusive textual authority — the political-military effects (territorial claims, jizya systems, minority persecution justified by this specific doctrine) would lose their sanctioned cover, though the underlying political violence might persist under secular justification.
% FOUNDING_PROBLEM: The verse's classical occasion was the Muslim community's need to respond to specific treaty-breaking Meccan polytheist tribes during the consolidation of the early Medinan state — a bounded military-political problem of the 7th century.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream Sunni and Shia jurisprudential scholarship, including centuries of pre-modern tafsir addressing asbab al-nuzul, corroborates that the original occasion was the specific Meccan treaty violation rather than a standing universal mandate; this corroboration comes from within the broader Islamic scholarly tradition but explicitly outside the beneficiary set named here (the abrogation-doctrine's clerical proponents and the movements that rely on it), who instead assert the founding problem remains permanently live as a matter of eternal divine law rather than bounded historical circumstance.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.86, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.86) because the reading's own operational content is direct appropriation: land, tribute, captives, and political submission extracted from populations who under this reading have no recognized neutral or peaceful status absent capitulation. Suppression is authored even higher (0.88) because the doctrine's persistence depends on actively suppressing the contextualist and progressive readings within Muslim scholarly and educational institutions the beneficiary movements control, and on denying targeted populations any legitimate claim to non-submission. Theater ratio is moderate-low (0.30): the doctrine is not mostly performative — it demonstrably organizes real recruitment, real territorial administration (jizya systems, dhimmi status enforcement), and real violence; the theatrical component is the religious framing layered over what is also political-military expansion. Accessibility collapse (0.72) reflects that once a population or believer is inside a jurisdiction where this reading holds administrative power, alternative readings become practically inaccessible — not because they don't exist textually, but because clerical and coercive infrastructure forecloses them. Resistance (0.80) reflects the massive, sustained, and largely successful resistance this reading meets from mainstream Islamic jurisprudence, international law, and the populations it targets — this is not a reading that operates unopposed.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, this is coordination: unambiguous divine command resolving interpretive uncertainty, mobilizing collective action, and providing a clean boundary between believer and target. From every payer seat, the identical structure is naked appropriation — dispossession, subjugation, or death for a fact about one's own religious status that one did not choose and often cannot change without loss of one's own identity or community. This is exactly the divergence the framework exists to register: claimed_type is authored as tangled_rope (the doctrine possesses a genuine, if narrow, coordination function for the in-group even as it is asymmetrically extractive toward everyone outside it) rather than pure snare, because there IS a real coordination function for believers who accept it — but the presence of concentrated beneficiaries (clerics, movements, regimes) alongside identifiable, powerless victims with no meaningful exit is what pulls it away from mountain or rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (clerical authorities, expansionist movements, theocratic regimes) sit at low d — they set the doctrine's terms, control its propagation, and are structurally insulated from the violence it authorizes. Non-Muslim populations under expansion and religious minorities in contested territories sit at the highest d — trapped, powerless, targeted specifically because of an identity classification they did not choose, with the doctrine explicitly denying any status (treaty, neutrality, prior peace) that would exempt them. Muslim communities that coexist with non-Muslims and reformist scholars sit at moderate-high d despite being co-religionists with the beneficiaries: the doctrine imposes costs on them too, via guilt-by-association, apostasy accusations, and loss of interpretive authority within their own tradition — this is why they are declared payers rather than beneficiaries despite shared religious identity with the agenda-setters.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview is essential here: the doctrine's own proponents insist the founding problem (permanent cosmic conflict between belief and unbelief) remains eternally live, while corroborating scholarship from within the broader tradition — including centuries of tafsir addressing the verse's specific historical occasion — treats the founding problem as bounded and resolved in the 7th century. This is the mismatch the R5 interview is built to expose: founding_problem_status is authored as dead (per corroboration outside the beneficiary set) while the doctrine persists and even intensifies (rising extraction and suppression across the measurement series) — a textbook zombie-mandate pattern where the arrangement outlives the problem it was purportedly built to solve, sustained now by the institutional and military interests of those who administer it rather than by the bounded circumstance that occasioned it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_validity_of_blanket_naskh,
    'Is the classical doctrine of naskh (abrogation) as applied to 9:5 textually and logically sound when it purports to cancel roughly 124 verses with a single later verse, or is this an overextension of a narrower classical abrogation principle?',
    'Comparative philological and historical analysis of classical tafsir literature (Tabari, Ibn Kathir, Qurtubi) on the actual scope of naskh claims attached to 9:5, cross-referenced against the internal logic of classical usul al-fiqh abrogation criteria (direct contradiction, chronological priority, impossibility of reconciliation).',
    'If the blanket abrogation claim is a later maximalist overextension not supported by the classical sources'' own stated scope, the abrogating-universal reading loses its primary textual warrant and the constraint''s claimed legal necessity collapses toward pure political instrumentalization; if the classical sources genuinely support blanket abrogation, the reading retains stronger internal textual grounding even if still contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_validity_of_blanket_naskh, empirical, 'Whether blanket abrogation of 124 verses by 9:5 is textually supported by classical abrogation criteria.').

omega_variable(
    kernel_reading_distribution,
    'What proportion of trained Islamic jurists, across historical and contemporary periods, have actually held the abrogating-universalist reading versus the contextual-defensive or progressive-synthesis readings?',
    'Systematic survey of tafsir and fiqh literature across schools and centuries, and contemporary fatwa databases, coded by reading position.',
    'If the abrogating-universalist reading is a documented historical minority position amplified disproportionately by modern institutional and media dynamics, this strengthens the case that the constraint''s current reach is a constructed political phenomenon rather than a reflection of mainstream doctrinal consensus — relevant to how much weight the ''standing legal obligation'' framing deserves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_distribution, empirical, 'Historical and contemporary distribution of the three sibling readings among Islamic jurists.').

omega_variable(
    reading_as_cover_vs_genuine_belief,
    'For agenda-setters and beneficiaries who invoke this reading, to what extent is the doctrine sincerely held theology versus instrumentalized cover for territorial and political ambition?',
    'Historical case analysis of doctrine invocation patterns — does application correlate with theological consistency or with opportunistic political and military circumstance (e.g., selective enforcement, negotiated exceptions for strategic allies)?',
    'If invocation correlates strongly with political convenience, the tangled_rope classification''s coordination component is weaker than authored and the constraint tilts further toward pure snare (coordination story as cover); if genuinely doctrinally consistent even at cost to political interest, the coordination function is more substantive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_cover_vs_genuine_belief, conceptual, 'Whether the doctrine functions as sincere theology or as instrumentalized political cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__abrogating_universal, theater_ratio, 0, 0.18).
narrative_ontology:measurement(qura_tr_t10, quran_9_5_scope__abrogating_universal, theater_ratio, 10, 0.2).
narrative_ontology:measurement(qura_tr_t20, quran_9_5_scope__abrogating_universal, theater_ratio, 20, 0.22).
narrative_ontology:measurement(qura_tr_t30, quran_9_5_scope__abrogating_universal, theater_ratio, 30, 0.25).
narrative_ontology:measurement(qura_tr_t40, quran_9_5_scope__abrogating_universal, theater_ratio, 40, 0.28).
narrative_ontology:measurement(qura_tr_t50, quran_9_5_scope__abrogating_universal, theater_ratio, 50, 0.3).
narrative_ontology:measurement(qura_tr_t60, quran_9_5_scope__abrogating_universal, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__abrogating_universal, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(qura_be_t10, quran_9_5_scope__abrogating_universal, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(qura_be_t20, quran_9_5_scope__abrogating_universal, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(qura_be_t30, quran_9_5_scope__abrogating_universal, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(qura_be_t40, quran_9_5_scope__abrogating_universal, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(qura_be_t50, quran_9_5_scope__abrogating_universal, base_extractiveness, 50, 0.84).
narrative_ontology:measurement(qura_be_t60, quran_9_5_scope__abrogating_universal, base_extractiveness, 60, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__abrogating_universal, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(qura_su_t10, quran_9_5_scope__abrogating_universal, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(qura_su_t20, quran_9_5_scope__abrogating_universal, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(qura_su_t30, quran_9_5_scope__abrogating_universal, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(qura_su_t40, quran_9_5_scope__abrogating_universal, suppression_requirement, 40, 0.83).
narrative_ontology:measurement(qura_su_t50, quran_9_5_scope__abrogating_universal, suppression_requirement, 50, 0.86).
narrative_ontology:measurement(qura_su_t60, quran_9_5_scope__abrogating_universal, suppression_requirement, 60, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the quran_9_5_scope kernel. contextual_defensive treats 9:5 as addressing specific treaty-breaking Meccan tribes with no blanket abrogation; progressive_synthesis treats 9:5 as a time-bound directive superseded by the Quran's ethical trajectory. The three readings have structurally distinct beneficiary/victim sets: this reading (abrogating_universal) is the only one of the three that names non-Muslim populations broadly as a victim class and expansionist movements as beneficiaries; the sibling readings have near-zero extraction under their own lights, since they do not authorize offensive first-strike violence as standing obligation. The abrogating_universal reading FORECLOSES both siblings within a single jurisprudential framework, since its core premise (blanket abrogation, atemporal universal application) directly negates the siblings' core premises (no abrogation / time-bound application); a single legal tradition cannot coherently hold this reading alongside either sibling, though different communities and institutions hold different readings simultaneously across the broader tradition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
