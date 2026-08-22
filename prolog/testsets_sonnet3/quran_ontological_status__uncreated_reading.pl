% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: quran_ontological_status__uncreated_reading
 *   human_readable: Uncreated Qur'an Doctrine (Kalām Allāh Qadīm)
 *   domain: religious/philosophical/political
 *
 * SUMMARY:
 *   The ninth-century theological dispute over whether the Qur'an is created
 *   or uncreated was never purely metaphysical: it doubled as a contest over
 *   who holds final interpretive authority over Islamic law and doctrine. The
 *   Abbasid state initially enforced the created-Qur'an position by
 *   inquisition (the Mihna, 833-848 CE) against traditionalist scholars, most
 *   famously Ahmad ibn Hanbal. When the Mihna collapsed and the uncreated
 *   position triumphed as mainstream Sunni orthodoxy, it became the doctrinal
 *   bedrock for transmission-based (rather than rational-argument-based)
 *   religious authority for the next thousand-plus years. This story authors
 *   the uncreated reading's own structure: a genuine coordination function (a
 *   stable shared text as legal/ritual anchor) fused with asymmetric
 *   extraction (foreclosure of rationalist, allegorical, and reformist
 *   interpretive method, and the corresponding transfer of authority to
 *   transmission-based scholarship).
 *
 * KEY AGENTS:
 *   - traditionalist_jurists: primary agenda_setter (institutional/arbitrage) — administer and enforce the doctrine through creedal statements and curricula
 *   - rational_theologians: primary target (moderate/trapped) — bear exclusion from legitimate discourse
 *   - reformist_movements: secondary target (powerless/trapped) — bear frozen legal application across centuries
 *   - state_authorities_post_mihna: secondary institutional actor (institutional/arbitrage) — align with the winning doctrine for legitimacy
 *   - lay_believers: diffuse beneficiary/payer (powerless/constrained) — receive devotional stability, inherit interpretive rigidity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.58).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.62).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, tangled_rope).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Uncreated Qur'an Doctrine (Kalām Allāh Qadīm)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "religious/philosophical/political").

domain_priors:requires_active_enforcement(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, '70236dd7-d193-4251-9577-4289bfbda286').
narrative_ontology:cs_kernel_codification('70236dd7-d193-4251-9577-4289bfbda286', fixed_text).
narrative_ontology:cs_authority_grounding('70236dd7-d193-4251-9577-4289bfbda286', lineage).
narrative_ontology:cs_interpretation_layer_present('70236dd7-d193-4251-9577-4289bfbda286').
narrative_ontology:cs_reading_relation('70236dd7-d193-4251-9577-4289bfbda286', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('70236dd7-d193-4251-9577-4289bfbda286', quran_ontological_status__state_enforced_creation_reading, forecloses).
narrative_ontology:cs_axiom('70236dd7-d193-4251-9577-4289bfbda286', foundational, divine_speech_shares_gods_eternality).
narrative_ontology:cs_axiom_status(divine_speech_shares_gods_eternality, holdable).
narrative_ontology:cs_axiom_grounding('70236dd7-d193-4251-9577-4289bfbda286', divine_speech_shares_gods_eternality, deontological).
narrative_ontology:cs_axiom('70236dd7-d193-4251-9577-4289bfbda286', secondary, textual_wording_is_ontologically_fixed_not_contingent_artifact).
narrative_ontology:cs_axiom_status(textual_wording_is_ontologically_fixed_not_contingent_artifact, holdable).
narrative_ontology:cs_axiom_grounding('70236dd7-d193-4251-9577-4289bfbda286', textual_wording_is_ontologically_fixed_not_contingent_artifact, conventional).
narrative_ontology:cs_reference_frame('70236dd7-d193-4251-9577-4289bfbda286', hanbalite_traditionalist_consensus).
narrative_ontology:cs_drift_state('70236dd7-d193-4251-9577-4289bfbda286', contemporary_salafi_revivalism, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('70236dd7-d193-4251-9577-4289bfbda286', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditionalist_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_scholarly_class).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_schools).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, hanbalite_transmitters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rational_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reformist_movements).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, philosophically_trained_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, lay_believers).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, state_authorities_post_mihna).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, lay_believers).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, divine_speech_coeternality).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, textual_literalism_as_correct_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the doctrine through fatwa councils, madrasa curricula, and creedal statements (e.g. Ahmad ibn Hanbal's creed). They hold interpretive gatekeeping power: any reading treating the Qur'an's wording or meaning as contingent can be labeled heretical innovation (bid'ah). Their authority as transmitters of fixed textual meaning is directly proportional to how immovable the text's ontology is held to be.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditionalist_jurists, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, traditionalist_jurists, beneficiary).

% Build careers, teaching authority, and social standing on close textual transmission (hadith and Qur'an memorization/recitation) rather than rational or allegorical method. The uncreated doctrine makes their skill set — precise preservation of wording — the only legitimate path to religious knowledge, foreclosing rival credentialing routes.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_scholarly_class, beneficiary,
    organized, generational, constrained, regional).

% Theological schools (early Ahl al-Hadith, later Hanbalite/Salafi currents) that oppose Greek-influenced dialectical theology (kalām) as a method. The uncreated doctrine, ironically named using 'kalām' as its subject, is deployed by them precisely to shut down further kalām-style reasoning about God's attributes.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_schools, beneficiary,
    organized, civilizational, constrained, regional).

% Mu'tazilite and later rationalist theologians who argue God's absolute unity (tawhid) and transcendence require that speech — being an attribute realized in temporal utterance, letters, and sounds — must be created, or else a second eternal entity (the Qur'an) stands beside God. Under the uncreated doctrine they are branded innovators; their arguments are excluded from legitimate discourse and, historically, they faced social exclusion, loss of teaching posts, and after the Mihna's reversal, persecution.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rational_theologians, payer,
    moderate, biographical, trapped, regional).

% Exegetes who read anthropomorphic or ambiguous Qur'anic verses (God's 'hand,' 'sitting,' etc.) allegorically to preserve philosophical coherence. Once the text's wording itself is declared coeternal with God and immune to contingent revision, allegorical softening of literal meaning becomes suspect — it looks like tampering with something ontologically identical to God's own eternal nature.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, biographical, constrained, regional).

% Later reform-minded jurists and lay movements seeking to reinterpret specific rulings (on slavery, gender, punishment) in light of changed circumstance. Because the doctrine treats the text's meaning as fixed divine fact rather than a historically situated artifact, any claim that a verse's application should shift with context is met with the charge that this denies the Qur'an's eternal, unchanging nature — collapsing the space for legal evolution.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reformist_movements, payer,
    powerless, biographical, trapped, national).

% Scholars trained in Aristotelian/Neoplatonic logic (falsafa tradition) who want to bring philosophical categories to bear on revelation. The uncreated doctrine's insistence on the text's untouchable ontic status makes their entire interpretive toolkit suspect by association with the 'rationalist' camp that lost the doctrinal contest.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, philosophically_trained_scholars, payer,
    moderate, biographical, constrained, regional).

% Receive a stable, non-negotiable devotional object: the Qur'an's words are literally God's own eternal speech, which many find spiritually load-bearing and comforting. At the same time, they inherit whatever rigidities the doctrine locks in — they have no standing to contest scholarly consensus on textual meaning and bear the downstream effects of frozen rulings in daily and legal life.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, lay_believers, beneficiary,
    powerless, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, lay_believers, payer).

% Rulers who, after the failure of the Abbasid-imposed created-Qur'an inquisition (Mihna), found it politically expedient to align with the traditionalist consensus once it proved to have deeper popular and scholarly support. They enforce the uncreated doctrine as orthodoxy partly because it stabilizes their legitimacy claims against a rival state-imposed rationalist theology that had already failed.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, state_authorities_post_mihna, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, state_authorities_post_mihna, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__uncreated_reading, traditionalist_jurists).
narrative_ontology:fixing_cost_class(quran_ontological_status__uncreated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, non-negotiable textual anchor for law, ritual, and communal identity across a vast and linguistically/geographically diverse religious community — everyone appeals to the same fixed wording as final authority, which genuinely reduces interpretive chaos and gives jurisprudence a stable base to reason from.
% TRANSFER_FUNCTION: Moves interpretive authority and the social/economic capital that follows it (teaching posts, judgeships, patronage) toward scholars whose method is transmission and memorization of fixed wording, and away from scholars whose method is rational argument, allegory, or contextual reinterpretation — while also transferring flexibility away from anyone (reformers, lay claimants) who might want the text's application to shift with circumstance.
% ABSENT_VOICES: Mu'tazilite theologians and philosophically trained scholars largely lost the historical contest and are not represented in the surviving mainstream creedal tradition that this reading describes; their arguments survive mostly through hostile summaries in traditionalist heresiographies rather than in their own institutionally preserved voice.
% DISAPPEARANCE_RATIONALE: If the uncreated-Qur'an doctrine disappeared as a live commitment, the primary warrant traditionalist/literalist scholarship uses to foreclose rationalist and contextualist readings would vanish; legal and theological reform arguments currently blocked by 'you are denying the Qur'an's eternal nature' would lose that specific objection, jurisprudential method would open toward more explicitly historicized reading, and the institutional prestige currently accruing to transmission-based scholarship over rational-theological scholarship would have to be re-justified on other grounds.
% FOUNDING_PROBLEM: How to secure God's absolute transcendence and unity (tawhid) while accounting for the ontological status of His speech, in a way that also settles, against the rationalist (Mu'tazilite) theology that had briefly held state backing under the Mihna, who has final authority to say what the Qur'an means.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist jurists and the Hanbalite creedal tradition attest the doctrine settles a live theological necessity (denying it endangers tawhid). Historians of the Mihna period and modern scholars of Islamic intellectual history (writing from outside both the Hanbalite and Mu'tazilite camps) document that the doctrine's triumph was substantially a political and social outcome of the Mihna's failure and popular backlash, not a purely dialectical victory — i.e., corroboration from outside the beneficiary set treats the 'settled theological necessity' framing as contested rather than simply true.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__uncreated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is substantial but not maximal: the doctrine does solve a real coordination problem (a shared, stable textual anchor across a vast community) which caps how purely extractive it can be, but the systematic foreclosure of rational-theological and reformist method channels real institutional and social capital toward one scholarly faction over others across many centuries — that asymmetry is the extraction. Suppression (0.62, declining slightly as the doctrine consolidates from an initially contested, actively-enforced position into settled orthodoxy that needs less active policing) is high because heresy-charges (takfīr of Mu'tazilites, later marginalization of philosophical theology) are the mechanism that keeps rival readings out of legitimate discourse, not persuasion alone. Accessibility collapse (0.72) is high because once the doctrine is accepted, alternative readings of what the Qur'an's wording even IS become very hard to articulate without appearing to deny a settled tenet of faith. Resistance (0.55) reflects that rational theologians and reformist movements did mount serious, sustained argument against the doctrine's implications, historically and into the present.
 *
 * PERSPECTIVAL GAP:
 *   From the traditionalist jurist seat, the doctrine is simply true and its enforcement is fidelity to revealed reality, not extraction — the engine's computed seat classification for that stakeholder will differ sharply from the computed classification for the rational theologian seat, which experiences the same structure as coercive foreclosure of legitimate argument. Neither seat's self-perception is authored directly; both are outputs of the same structural data (power, exit options, beneficiary/victim role) read from opposite positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditionalist jurists and anti-rationalist schools are declared beneficiaries: their authority, curricula, and social position are constituted BY the doctrine's dominance, giving them low d (near full beneficiary). Rational theologians, metaphorical interpreters, reformist movements, and philosophically trained scholars are declared victims: the doctrine's foreclosure function operates directly against their method and standing, giving them high d (near full target), amplified by their trapped or constrained exit — a rationalist theologian within the tradition cannot simply exit to a different religious framework without abandoning the entire discursive field they are trying to reform. Lay believers sit closer to symmetric: real devotional benefit, diffuse downstream cost from frozen rulings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (settling tawhid against a rationalist theology that briefly held state coercive backing) is genuinely contested rather than dead: traditionalists maintain it is still theologically necessary, while historians and rationalist-sympathetic scholars read its 'necessity' as substantially a political artifact of the Mihna's failure. This is exactly the case the tangled_rope classification is built to hold apart from a pure snare verdict: the doctrine is not JUST a power grab dressed as theology (it does anchor genuine textual stability that the community relies on) nor is it JUST innocent coordination (it demonstrably forecloses rival scholarly method and has done so with real enforcement, historically via takfīr and exclusion from teaching posts). Collapsing it to either pure type would erase half the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    uncreated_reading_vs_created_reading_kernel_location,
    'Is the disagreement between the uncreated and created readings located in the ontology of divine attributes (whether speech, like other attributes, must be eternal to preserve God''s perfection) or in the political question of who holds interpretive authority (transmission-based scholars vs. rational-argument-based scholars)?',
    'Compare doctrinal content across social contexts where each reading held institutional power without contested state backing (e.g., post-Mihna Sunni consolidation vs. earlier or parallel communities holding the created view without state coercion) to isolate whether authority concentration tracks the metaphysical claim or is separable from it.',
    'If the disagreement is substantially about authority rather than ontology, that strengthens the tangled_rope reading of this story (coordination cover for an extraction fight); if it is substantially about ontology with authority effects as a downstream consequence rather than the point, the coordination function is more central than the extraction function suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncreated_reading_vs_created_reading_kernel_location, conceptual, 'Whether the uncreated/created dispute is fundamentally metaphysical or fundamentally about interpretive authority.').

omega_variable(
    natural_theology_vs_constructed_orthodoxy,
    'Is the uncreated-Qur''an doctrine a discoverable truth about God''s nature (in which case its ''beneficiaries'' are incidental to a correct metaphysical claim) or a constructed doctrinal settlement whose primary function was resolving a ninth-century political-theological power struggle in favor of one scholarly faction?',
    'This is likely irreducible from within any single theological framework — no external empirical test adjudicates the ontological status of divine speech. The best available proxy is historical: examine whether doctrinal content shifted to track political outcomes (Mihna''s collapse, Hanbalite consolidation) or whether it remained stable across contexts regardless of political pressure.',
    'If beneficiaries were declared on what is, from inside the tradition, treated as literal revealed truth rather than a human construction, that changes whether this constraint should ever be read as extractive at all versus simply correct doctrine with incidental institutional effects. This omega documents the ambiguity rather than resolving it in either direction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_theology_vs_constructed_orthodoxy, conceptual, 'Whether the doctrine is discovered theological truth or a constructed settlement with identifiable institutional winners.').

omega_variable(
    mihna_causation_direction,
    'Did the uncreated doctrine triumph because it was theologically more defensible, or because the Mihna''s political failure and popular backlash against state-enforced creationism made the traditionalist alternative the only viable consolidating position regardless of its comparative philosophical merit?',
    'Historical analysis of the sequence and mechanisms of the Mihna''s collapse (Caliph al-Mutawakkil''s reversal, the specific political incentives at play, popular reaction documented in contemporary sources) versus contemporaneous theological argument quality on both sides.',
    'If political contingency dominates, the doctrine''s current authority rests on a historical accident dressed as necessary theology — strengthening the extraction reading. If theological argument quality dominates, the political events are incidental to an independently sound doctrinal victory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mihna_causation_direction, empirical, 'Whether the doctrine''s historical triumph was substantially political or substantially argumentative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__uncreated_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t200, quran_ontological_status__uncreated_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement_basis(qura_tr_t200, observed).
narrative_ontology:measurement(qura_tr_t400, quran_ontological_status__uncreated_reading, theater_ratio, 400, 0.2).
narrative_ontology:measurement_basis(qura_tr_t400, observed).
narrative_ontology:measurement(qura_tr_t600, quran_ontological_status__uncreated_reading, theater_ratio, 600, 0.23).
narrative_ontology:measurement_basis(qura_tr_t600, observed).
narrative_ontology:measurement(qura_tr_t900, quran_ontological_status__uncreated_reading, theater_ratio, 900, 0.26).
narrative_ontology:measurement_basis(qura_tr_t900, observed).
narrative_ontology:measurement(qura_tr_t1200, quran_ontological_status__uncreated_reading, theater_ratio, 1200, 0.28).
narrative_ontology:measurement_basis(qura_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__uncreated_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t200, quran_ontological_status__uncreated_reading, base_extractiveness, 200, 0.45).
narrative_ontology:measurement_basis(qura_be_t200, observed).
narrative_ontology:measurement(qura_be_t400, quran_ontological_status__uncreated_reading, base_extractiveness, 400, 0.52).
narrative_ontology:measurement_basis(qura_be_t400, observed).
narrative_ontology:measurement(qura_be_t600, quran_ontological_status__uncreated_reading, base_extractiveness, 600, 0.55).
narrative_ontology:measurement_basis(qura_be_t600, observed).
narrative_ontology:measurement(qura_be_t900, quran_ontological_status__uncreated_reading, base_extractiveness, 900, 0.57).
narrative_ontology:measurement_basis(qura_be_t900, observed).
narrative_ontology:measurement(qura_be_t1200, quran_ontological_status__uncreated_reading, base_extractiveness, 1200, 0.58).
narrative_ontology:measurement_basis(qura_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__uncreated_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t200, quran_ontological_status__uncreated_reading, suppression_requirement, 200, 0.68).
narrative_ontology:measurement_basis(qura_su_t200, observed).
narrative_ontology:measurement(qura_su_t400, quran_ontological_status__uncreated_reading, suppression_requirement, 400, 0.65).
narrative_ontology:measurement_basis(qura_su_t400, observed).
narrative_ontology:measurement(qura_su_t600, quran_ontological_status__uncreated_reading, suppression_requirement, 600, 0.63).
narrative_ontology:measurement_basis(qura_su_t600, observed).
narrative_ontology:measurement(qura_su_t900, quran_ontological_status__uncreated_reading, suppression_requirement, 900, 0.62).
narrative_ontology:measurement_basis(qura_su_t900, observed).
narrative_ontology:measurement(qura_su_t1200, quran_ontological_status__uncreated_reading, suppression_requirement, 1200, 0.62).
narrative_ontology:measurement_basis(qura_su_t1200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the quran_ontological_status kernel. created_reading authors the Mu'tazilite position on its own terms (speech as a created attribute, preserving strict divine unity) with its own ε and beneficiary/victim structure. state_enforced_creation_reading authors the same created-doctrine content but under conditions of state coercion (the Mihna itself), which produces a substantially different suppression and enforcement profile — that story's victims are traditionalist scholars refusing state orthodoxy, the mirror image of this story's victim set. All three share the same underlying kernel (what is the Qur'an's ontological status, and who decides) but instantiate structurally distinct constraints with different ε, different winners, and different enforcement mechanisms. None should be read as measuring 'the same constraint' from different angles — per the ε-invariance principle, they are three different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
