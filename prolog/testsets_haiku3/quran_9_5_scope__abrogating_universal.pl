% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Qur'an 9:5 Abrogating Verses — Universal Offensive Jihad Interpretation
 *   domain: religious/political/hermeneutic
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested Qur'anic kernel
 *   at Verse 9:5. This is the abrogating-universal reading: the claim that
 *   Verse 9:5 ('slay the polytheists wherever you find them') abrogates all
 *   prior Qur'anic verses emphasizing mercy, forbearance, and freedom of
 *   conscience, establishing permanent divine sanction for offensive military
 *   action against non-Muslims until their conversion or submission. The
 *   reading is held by movements claiming theological authority within
 *   Islamic jurisprudence (particularly Salafi and certain jihadist schools)
 *   and rejected by coexistence-reading Muslims (Sufi, moderate reformist,
 *   and many contemporary Islamic scholars) and by secular governance
 *   frameworks. The constraint as authored models the structural operation of
 *   the abrogating-universal reading IF ADOPTED: who benefits, who bears
 *   costs, what enforcement apparatus sustains it. This reading's ε (0.89
 *   base extractiveness) reflects the high structural extraction from the
 *   victim set and the high suppression of competing interpretations, NOT an
 *   evaluation of whether the reading is true, justified, or authoritative
 *   within Islamic tradition. Alternative readings (contextual-defensive,
 *   progressive-synthesis) would author their own constraints with different
 *   ε values, different beneficiary/victim structures, and different type
 *   classifications — they are separate stories, not perspectives on this
 *   one. The kernel is genuinely contested; the readings are genuinely
 *   incommensurable on the core question of whether 9:5 is
 *   universal/permanent or contextual/superseded.
 *
 * KEY AGENTS:
 *   - expansionist_movements: movements claiming theological authority for the abrogating reading; organize around offensive jihad doctrine; their institutional survival and identity are fused with this interpretation
 *   - non_muslims: structured as permanent targets absent conversion; bear the extraction cost of the constraint's operation; located in victim position by religious identity alone
 *   - coexistence_advocates: Muslim scholars and communities holding alternative (contextual or progressive) readings; excluded from authority apparatus when abrogating reading dominates; bear legitimacy and influence costs
 *   - institutional_islamic_hierarchy: formal jurisprudential authorities (courts, fatwas, academic institutions) that adopt and codify the reading; enforce it through education and law; constrained to defend their institutional position
 *   - peaceful_scriptural_traditions: non-agent entry representing the corpus of Qur'anic verses the abrogating reading claims to nullify; suppressed by the constraint's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.89).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.92).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.89).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Qur'an 9:5 Abrogating Verses — Universal Offensive Jihad Interpretation").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "religious/political/hermeneutic").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, '4d28f044-2289-4463-b863-f186bb526834').
narrative_ontology:cs_kernel_codification('4d28f044-2289-4463-b863-f186bb526834', fixed_text).
narrative_ontology:cs_authority_grounding('4d28f044-2289-4463-b863-f186bb526834', lineage).
narrative_ontology:cs_interpretation_layer_present('4d28f044-2289-4463-b863-f186bb526834').
narrative_ontology:cs_reading_relation('4d28f044-2289-4463-b863-f186bb526834', quran_9_5_scope__contextual_defensive, coexists_with).
narrative_ontology:cs_reading_relation('4d28f044-2289-4463-b863-f186bb526834', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('4d28f044-2289-4463-b863-f186bb526834', foundational, nasikh_abrogates_universally).
narrative_ontology:cs_axiom_status(nasikh_abrogates_universally, holdable).
narrative_ontology:cs_axiom_grounding('4d28f044-2289-4463-b863-f186bb526834', nasikh_abrogates_universally, deontological).
narrative_ontology:cs_axiom('4d28f044-2289-4463-b863-f186bb526834', foundational, offensive_jihad_divinely_mandated).
narrative_ontology:cs_axiom_status(offensive_jihad_divinely_mandated, holdable).
narrative_ontology:cs_axiom_grounding('4d28f044-2289-4463-b863-f186bb526834', offensive_jihad_divinely_mandated, theological).
narrative_ontology:cs_reference_frame('4d28f044-2289-4463-b863-f186bb526834', permanent_divine_offensive_mandate).
narrative_ontology:cs_drift_state('4d28f044-2289-4463-b863-f186bb526834', contemporary_secular_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4d28f044-2289-4463-b863-f186bb526834', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_movements).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslims).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, coexistence_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups claiming theological authority to interpret Verse 9:5 as permanent abrogation of peaceful directives. They set the interpretive agenda through fatwas, teachings, recruitment narratives, and operational justification for armed action against non-Muslim populations without prior aggression. Their identity and organizational mandate fuse with this theological position — exit would dissolve the movement's theological rationale.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_movements, agenda_setter,
    organized, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, expansionist_movements, beneficiary).

% Under this reading, all non-Muslims are structured as legitimate targets for offensive military action absent formal submission or conversion. Their options are constrained to acceptance (conversion or submission to Islamic authority), flight (where physically possible), or resistance (where capable). The constraint places them in a permanent victim position defined by their religious identity, with no neutral exit option.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslims, payer,
    powerless, immediate, trapped, universal).

% Muslim scholars and communities that read Verse 9:5 as context-bound, defensive, or superseded by Qur'anic ethical principles. They bear the cost of contradiction with the abrogating-universal reading in institutional hierarchies, fatwa authority, and community legitimacy. Their interpretive authority is systematically excluded from the constraint's enforcement apparatus when the abrogating reading dominates institutional structures.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, coexistence_advocates, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, coexistence_advocates, excluded).

% The corpus of Qur'anic verses emphasizing mercy, forbearance, freedom of conscience (2:256 'no compulsion in religion'), and peaceful coexistence (60:8-9). Under the abrogating reading, these are theoretically present but functionally nullified by the claim that 9:5 cancels their legal effect. Included as a non-agent entry because the constraint's structure depends on suppressing these scriptural sources.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, peaceful_scriptural_traditions, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(quran_9_5_scope__abrogating_universal, peaceful_scriptural_traditions).

% Formal Islamic authority structures (state-level Islamic courts, official fatwas, institutional jurisprudence) that adopt, enforce, or codify the abrogating-universal reading. They translate the interpretation into law, education, and social policy. Their constraint over interpretation is structural — they must defend the reading they have institutionalized or face internal legitimacy crises.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, institutional_islamic_hierarchy, agenda_setter,
    institutional, generational, constrained, continental).

% Nation-states, international law frameworks, interfaith dialogues, and secular governance structures that operate outside the Islamic interpretive authority system. They observe the constraint's operation and effects but lack standing within the theological framework that authorizes it. Their power is limited to external pressure, legal prohibition, or advocacy for alternative readings.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, secular_and_interfaith_actors, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__abrogating_universal, expansionist_movements).
narrative_ontology:fixing_cost_class(quran_9_5_scope__abrogating_universal, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified directive for conflict engagement: reduces ambiguity about permissible targets by subordinating all prior verses to a single abrogating principle, creating clarity for movement organization and member commitment.
% TRANSFER_FUNCTION: Transfers moral legitimacy, political authority, and justification for violence from human institutions to divine decree (as interpreted). Moves non-Muslim populations into a permanent subordinate structural position: from potential equals in coexistence to mandatory targets or submission subjects. Concentrates interpretive authority in hands of movements claiming the abrogating reading.
% ABSENT_VOICES: Non-Muslim populations subject to the constraint have no voice in Islamic jurisprudential debates that establish its scope. Peaceful-coexistence-reading Muslim scholars are structurally excluded from the authority systems that enforce the abrogating interpretation when it dominates institutional hierarchies. Secular governance actors and international law frameworks are positioned as external observers, not participants in the legitimacy debate.
% DISAPPEARANCE_RATIONALE: If this interpretive constraint (the claim that 9:5 abrogates all peaceful verses universally) disappeared — i.e., if the abrogating-universal reading lost institutional authority and the contextual-defensive or progressive-synthesis readings became dominant — the structural justification for offensive religious warfare would collapse. Movements organized around universal jihad mandate would lose theological rationale. Inter-religious coexistence frameworks would move from theoretical to operational. Non-Muslim populations would shift from permanent-target to protected or coequal status under alternative readings.
% FOUNDING_PROBLEM: 7th-century Medina: early Islamic community faced treaty-breaking by pagan Quraysh tribes, violated agreements, threat to nascent community survival. A jurisprudential principle was needed to adjudicate the limits of tolerance and conditions for defensive response.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholars within Islamic jurisprudence agree a 7th-century conflict existed. They divide sharply on whether it was temporary (contextual-defensive reading) or archetype (abrogating-universal reading). Non-Islamic historical analysis and Arabic philology scholars document the specific treaty-violations and community context. No corroboration comes from non-Muslims or from coexistence-reading Muslims that the founding problem still exists in its 7th-century form or that offensive warfare remains the correct response — these constituencies attest the problem is solved or reframed.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is extremely high (0.89) because the reading produces a permanent structural subordination of non-Muslims: they are moved from potential coequals into a mandatory-target or submission-subject position by religious identity alone, with no neutral coexistence option. This is maximum-asymmetry extraction. Suppression is even higher (0.92) because the constraint's persistence depends on actively suppressing the scriptural record (treating 2:256 'no compulsion' and similar verses as nullified rather than enduring), suppressing alternative jurisprudential readings, and suppressing resistance from coexistence-reading Muslim scholars and non-Muslim populations. Accessibility of alternatives is substantially collapsed (0.78): once the reading is institutionalized, the scriptural justifications for coexistence appear defeated by doctrinal fiat, making escape into a different framework structurally difficult — the reading occupies the authoritative interpretive position. Resistance is substantial (0.74) because coexistence-reading Muslims, non-Muslim populations, and secular governance actors actively contest the reading through counter-interpretation, legal frameworks, and interfaith dialogue. Theater is relatively low (0.28): the reading's operation is not primarily performative — movements organized around it pursue genuine (if contested) military and political action; the theater that exists is the performance of doctrinal authority and the maintenance of interpretive-tradition legitimacy even as the reading's scope is disputed. Time series show extractiveness and suppression rising over the 1400-year interval (from 0.65/0.72 toward 0.89/0.92) because institutional codification in classical Islamic jurisprudence, political consolidation of expansionist movements, and the suppression of alternative readings within institutional hierarchies all accumulated over centuries. The grid captures level-specific coercion: at the individual level, stakes for non-Muslims are apocalyptic (conversion or death); at the organizational level, movements enforce the doctrine through institutional channels; at the class level, entire populations (non-Muslims) are structured as targets; at the structural level, the reading is codified as authoritative jurisprudence. Resistance rises slightly over time (0.62 → 0.71 structural) as modern secularism, interfaith movements, and reformist Islamic scholarship mount counter-pressure, but this resistance remains weaker than the constraint's institutional enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The expansionist-movement seat should compute this as rope (genuine coordination function for organizing military and theological community) or low-extractiveness snare (high theater, low suppression of internal dissent). The victim seat (non-Muslims) should compute it as pure snare (maximum extraction, coercion, no exit, suppression of alternatives). The coexistence-reading seat should compute it as snare with strong theater (institutional performance of doctrinal authority masking hermeneutical contestation). The institutional-hierarchy seat should compute it as tangled-rope-becoming-piton (coordination was genuine in medieval consolidation, but now the reading persists largely through inertia while mounting costs accrue). The engine's per-seat computations will diverge sharply from the claimed type (snare) because directionality maps individual positions to radically different structural relationships. This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansionist movements benefit directly from the reading (collects theological legitimacy, removes moral obstacles to military action, justifies resource extraction from non-Muslim populations). Their d is near 0.1 (strong beneficiary). Non-Muslims bear the extraction cost (potential targeting, subordination, forced choice between conversion and vulnerability). Their d is near 0.95 (strong target). Coexistence-reading Muslims have a complex position: they benefit from the scriptural sources the reading claims to nullify, but they bear costs in institutional hierarchies where the abrogating reading dominates (excluded from authority, need to mount counter-interpretations constantly). Their d is around 0.6 (symmetric with extraction-leaning bias because institutional suppression outweighs their scriptural benefits). Institutional hierarchy actors are constrained: they appear to benefit (institutional authority, ability to set doctrine) but are locked into defending a reading that generates mounting resistance from secular states and interfaith communities. Their d is around 0.35 (moderate beneficiary but with growing constraint). The directionality spread is extreme — 0.1 to 0.95 — because the reading creates maximum structural asymmetry: one group's identity and institutional position fuses with the doctrine, while another group's entire population is defined as targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (7th-century treaty-violations by Quraysh polytheists threatening the nascent Muslim community) is emphatically DEAD at the 1400-year mark. No actor today faces that specific problem. Yet the abrogating-universal reading persists as if the problem were LIVE — it continues to authorize offensive jihad as if permanent religious warfare were still the solution to a recurring 7th-century conflict. This is classic mandatrophy: the mandate (address treaty-breaking polytheists through defensive warfare) has outlived its function (the problem no longer exists in its original form), but the constraint persists due to institutional investment in the doctrine and movement identity fusion with the reading. The reading cannot be simply retired without crises in institutional legitimacy and movement identity. This is a snare with mandatrophy, not a temporary scaffold that expired and was removed — the classification holds even as the founding mandate is dead. The constraint persists not because the original coordination problem is still being solved, but because suppression of alternative readings and identity lock on expansionist movements keep the abrogating doctrine in place despite its obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nasikh_textual_scope,
    'Does Verse 9:5 textually and linguistically claim to abrogate ALL prior peaceful verses, or is the abrogation claim interpolated by jurisprudential tradition?',
    'Philological analysis of Arabic grammar and syntax in 9:5; historical-critical study of when the nasikh (abrogation) doctrine was formally articulated in Islamic jurisprudence; comparison of earliest commentaries (tafsir) with later developments.',
    'If 9:5 does not itself claim universal abrogation, the doctrine is a constructed interpretive overlay, not a scriptural mandate. The constraint''s legitimacy would shift from divine command to jurisprudential authority claim. This would open space for alternative readings without textual contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nasikh_textual_scope, empirical, 'Whether Verse 9:5 actually authorizes the abrogating-universal reading or whether the nasikh doctrine is post-hoc interpretive construction.').

omega_variable(
    reading_coexistence_possibility,
    'Can the abrogating-universal reading and the contextual-defensive reading COEXIST within a single Muslim''s interpretive framework, or do they logically foreclose each other?',
    'Examine Islamic jurisprudential tradition for scholars who hold BOTH readings simultaneously (situational application: 9:5 applies to treaty-breakers in 7th c., also applies universally in perpetuity); test whether this is coherent or contradictory.',
    'If they coexist (different scholars holding both, or situational contexts), the relation is ''coexists_with'' and the kernel is genuinely contested with no foreclosure. If truly incompatible, one forecloses the other. This determines the reading-relations structure in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_possibility, conceptual, 'Whether the abrogating-universal and contextual-defensive readings are logically compatible or mutually exclusive.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the suppression of coexistence frameworks structural (legal penalties, institutional enforcement, resource denial) or internalized (believers accept the doctrine''s premise and self-police alternatives)?',
    'Comparative historical analysis: in regions where institutional enforcement weakened (decentralized authority, secular law), did coexistence-reading adherence rise? Do adherents of the abrogating reading defend it when institutional pressure is removed?',
    'If suppression is heavily internalized (believers fused with the doctrine), the constraint persists even without external enforcement apparatus. If structural, removing enforcement institutions would allow alternative readings to emerge. This affects long-term stability and resistance assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Decomposition of suppression into structural enforcement vs. internalized belief compliance.').

omega_variable(
    expansionist_movements_identity_lock,
    'For movements organized around the abrogating-universal reading, is adherence identity-locked (exit = organizational dissolution) or merely strategically beneficial (exit = tactical change)?',
    'Case study of movements that have abandoned or modulated the abrogating-universal interpretation: did they dissolve or merely reorganize? Do defectors describe the shift as identity-loss or strategic recalibration?',
    'If identity-locked, movements cannot rationally exit even if costs mount — they are trapped by who they have become. If strategically beneficial only, alternative interpretive movements could emerge with different theologies. This affects the power atom''s effective constraint on agenda-setters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansionist_movements_identity_lock, empirical, 'Whether abrogating-universal doctrine is fused with movement identity or instrumentally adopted.').

omega_variable(
    kernel_reading_contest_genealogy,
    'Are the three sibling readings (abrogating-universal, contextual-defensive, progressive-synthesis) products of the SAME interpretive tradition (Islamic jurisprudence) or do they represent genuinely incommensurable frameworks (Islamic, secular, interfaith)?',
    'Historical analysis of each reading''s authority grounding: who proposes it, on what epistemic basis (scriptural, jurisprudential precedent, rational argument, external critique), and what tradition validates it?',
    'If all three emerge from within Islamic jurisprudence, the kernel is internally contested and readings coexist. If one or more come from outside (secular law, interfaith ethics), they influence but do not foreclose — they are external pressure, not authentic readings of the kernel. This determines whether the three are genuine siblings or whether only two are true readings within Islamic tradition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_genealogy, conceptual, 'Epistemic grounding and authority validation for each of the three readings.').

omega_variable(
    victim_set_definition_ambiguity,
    'Under the abrogating-universal reading, are ALL non-Muslims permanently legitimate targets, or only those who have formally rejected conversion offers and treaty terms?',
    'Jurisprudential analysis of fiqh rulings on jizya (protected non-Muslim status), aman (safe conduct), and tahrib (making permissible to kill non-Muslim inhabitants). Compare classical schools'' treatments with contemporary movement interpretations.',
    'If targeting is unconditional, every non-Muslim is a permanent victim by identity. If conditional (after rejection of terms), some exit is possible through submission or formal status. This affects victim set boundaries and the constraint''s classification (pure snare vs. conditioned extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_definition_ambiguity, empirical, 'Whether the abrogating reading''s victim set is universal and unconditional or conditional on refusal of conversion/submission/jizya status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__abrogating_universal, theater_ratio, 0, 0.18).
narrative_ontology:measurement(qura_tr_t200, quran_9_5_scope__abrogating_universal, theater_ratio, 200, 0.19).
narrative_ontology:measurement(qura_tr_t600, quran_9_5_scope__abrogating_universal, theater_ratio, 600, 0.22).
narrative_ontology:measurement(qura_tr_t1000, quran_9_5_scope__abrogating_universal, theater_ratio, 1000, 0.25).
narrative_ontology:measurement(qura_tr_t1300, quran_9_5_scope__abrogating_universal, theater_ratio, 1300, 0.27).
narrative_ontology:measurement(qura_tr_t1400, quran_9_5_scope__abrogating_universal, theater_ratio, 1400, 0.28).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__abrogating_universal, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(qura_be_t200, quran_9_5_scope__abrogating_universal, base_extractiveness, 200, 0.72).
narrative_ontology:measurement(qura_be_t600, quran_9_5_scope__abrogating_universal, base_extractiveness, 600, 0.84).
narrative_ontology:measurement(qura_be_t1000, quran_9_5_scope__abrogating_universal, base_extractiveness, 1000, 0.87).
narrative_ontology:measurement(qura_be_t1300, quran_9_5_scope__abrogating_universal, base_extractiveness, 1300, 0.89).
narrative_ontology:measurement(qura_be_t1400, quran_9_5_scope__abrogating_universal, base_extractiveness, 1400, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__abrogating_universal, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(qura_su_t200, quran_9_5_scope__abrogating_universal, suppression_requirement, 200, 0.78).
narrative_ontology:measurement(qura_su_t600, quran_9_5_scope__abrogating_universal, suppression_requirement, 600, 0.86).
narrative_ontology:measurement(qura_su_t1000, quran_9_5_scope__abrogating_universal, suppression_requirement, 1000, 0.88).
narrative_ontology:measurement(qura_su_t1300, quran_9_5_scope__abrogating_universal, suppression_requirement, 1300, 0.91).
narrative_ontology:measurement(qura_su_t1400, quran_9_5_scope__abrogating_universal, suppression_requirement, 1400, 0.92).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=1400
narrative_ontology:measurement(qura_grid_01, quran_9_5_scope__abrogating_universal, accessibility_collapse(class), 0, 0.64).
narrative_ontology:measurement(qura_grid_02, quran_9_5_scope__abrogating_universal, accessibility_collapse(class), 1400, 0.81).
narrative_ontology:measurement(qura_grid_03, quran_9_5_scope__abrogating_universal, accessibility_collapse(individual), 0, 0.56).
narrative_ontology:measurement(qura_grid_04, quran_9_5_scope__abrogating_universal, accessibility_collapse(individual), 1400, 0.74).
narrative_ontology:measurement(qura_grid_05, quran_9_5_scope__abrogating_universal, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(qura_grid_06, quran_9_5_scope__abrogating_universal, accessibility_collapse(organizational), 1400, 0.76).
narrative_ontology:measurement(qura_grid_07, quran_9_5_scope__abrogating_universal, accessibility_collapse(structural), 0, 0.62).
narrative_ontology:measurement(qura_grid_08, quran_9_5_scope__abrogating_universal, accessibility_collapse(structural), 1400, 0.78).
narrative_ontology:measurement(qura_grid_09, quran_9_5_scope__abrogating_universal, resistance(class), 0, 0.74).
narrative_ontology:measurement(qura_grid_10, quran_9_5_scope__abrogating_universal, resistance(class), 1400, 0.78).
narrative_ontology:measurement(qura_grid_11, quran_9_5_scope__abrogating_universal, resistance(individual), 0, 0.58).
narrative_ontology:measurement(qura_grid_12, quran_9_5_scope__abrogating_universal, resistance(individual), 1400, 0.62).
narrative_ontology:measurement(qura_grid_13, quran_9_5_scope__abrogating_universal, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(qura_grid_14, quran_9_5_scope__abrogating_universal, resistance(organizational), 1400, 0.76).
narrative_ontology:measurement(qura_grid_15, quran_9_5_scope__abrogating_universal, resistance(structural), 0, 0.62).
narrative_ontology:measurement(qura_grid_16, quran_9_5_scope__abrogating_universal, resistance(structural), 1400, 0.71).
narrative_ontology:measurement(qura_grid_17, quran_9_5_scope__abrogating_universal, stakes_inflation(class), 0, 0.85).
narrative_ontology:measurement(qura_grid_18, quran_9_5_scope__abrogating_universal, stakes_inflation(class), 1400, 0.92).
narrative_ontology:measurement(qura_grid_19, quran_9_5_scope__abrogating_universal, stakes_inflation(individual), 0, 0.88).
narrative_ontology:measurement(qura_grid_20, quran_9_5_scope__abrogating_universal, stakes_inflation(individual), 1400, 0.94).
narrative_ontology:measurement(qura_grid_21, quran_9_5_scope__abrogating_universal, stakes_inflation(organizational), 0, 0.71).
narrative_ontology:measurement(qura_grid_22, quran_9_5_scope__abrogating_universal, stakes_inflation(organizational), 1400, 0.87).
narrative_ontology:measurement(qura_grid_23, quran_9_5_scope__abrogating_universal, stakes_inflation(structural), 0, 0.68).
narrative_ontology:measurement(qura_grid_24, quran_9_5_scope__abrogating_universal, stakes_inflation(structural), 1400, 0.84).
narrative_ontology:measurement(qura_grid_25, quran_9_5_scope__abrogating_universal, suppression(class), 0, 0.68).
narrative_ontology:measurement(qura_grid_26, quran_9_5_scope__abrogating_universal, suppression(class), 1400, 0.93).
narrative_ontology:measurement(qura_grid_27, quran_9_5_scope__abrogating_universal, suppression(individual), 0, 0.76).
narrative_ontology:measurement(qura_grid_28, quran_9_5_scope__abrogating_universal, suppression(individual), 1400, 0.94).
narrative_ontology:measurement(qura_grid_29, quran_9_5_scope__abrogating_universal, suppression(organizational), 0, 0.72).
narrative_ontology:measurement(qura_grid_30, quran_9_5_scope__abrogating_universal, suppression(organizational), 1400, 0.91).
narrative_ontology:measurement(qura_grid_31, quran_9_5_scope__abrogating_universal, suppression(structural), 0, 0.64).
narrative_ontology:measurement(qura_grid_32, quran_9_5_scope__abrogating_universal, suppression(structural), 1400, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__abrogating_universal, 0.18).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% The Qur'anic Verse 9:5 constraint family comprises three competing readings of the same scriptural kernel. The abrogating-universal reading (this constraint) authorizes permanent offensive jihad; the contextual-defensive reading interprets 9:5 as addressing specific 7th-century treaty-breakers and prioritizes coexistence frameworks; the progressive-synthesis reading treats 9:5 as time-bound and subordinate to Qur'anic ethical development toward mercy and conscience-freedom. Each reading has structurally distinct ε values, beneficiary/victim sets, and type classifications. They are not perspectives on a single constraint but three structurally incommensurable constraints sharing a common scriptural text. The network links capture the hermeneutical dependency: all three readings orbit the same kernel and must define themselves relative to each other. The abrogating-universal reading forecloses the progressive reading's core claim about ethical directionality; it coexists with the contextual-defensive reading but exercises institutional pressure that shapes how the contextual reading must be defended.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__abrogating_universal, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
