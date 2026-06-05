% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Vatican II as Organic Development (Continuity Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   The continuity reading of Vatican II claims that the Council represents
 *   organic doctrinal development within an unchanging tradition — apparent
 *   novelties in the documents (religious liberty, ecumenical openness,
 *   episcopal collegiality) are explications of implicit prior teaching
 *   rather than ruptures. This reading serves as a hermeneutical frame for
 *   stabilizing institutional authority: it permits pastoral and liturgical
 *   adaptation without requiring doctrinal admission of change. The
 *   constraint operates primarily on interpretation and historical narrative,
 *   extracting from agents (theologians, scholars, reform-minded bishops) who
 *   perceive genuine discontinuity or historical contingency, while
 *   benefiting the institutional magisterium and doctrinal custodians who use
 *   the narrative to preserve authority and control reform. The reading's
 *   theater increases over time as the hermeneutical machinery required to
 *   sustain continuity claims becomes more elaborate and as historical
 *   scholarship increasingly documents cases of genuine innovation.
 *
 * KEY AGENTS:
 *   - Vatican Magisterium: Institutional beneficiary (institutional/arbitrage) — uses continuity narrative to enable pastoral flexibility while preserving doctrinal authority; experiences the constraint as coordination enabling measured reform
 *   - Pre-Conciliar Doctrinal Custodians: Secondary beneficiary (institutional/arbitrage) — continuity reading defends the authority and legitimacy of pre-conciliar teaching by rendering post-conciliar change as development not rupture
 *   - Historical Consciousness Movement: Primary victim (powerless/trapped) — theologians and exegetes who perceive doctrinal rupture or historical contingency are foreclosed by the reading; institutional consequences for dissent
 *   - Post-Conciliar Reform Movements: Secondary victim (moderate/constrained) — must justify innovations as explications of tradition rather than acknowledge strategic choices; constrained in legitimacy claims
 *   - Traditionalist Movement: Organized secondary agent (organized/constrained) — benefits from continuity reading's framework (distinguishes true Council from heterodox implementation) yet constrained by its absorption of traditionalist critiques
 *   - Liturgical Reform Architects: Moderate agent (moderate/constrained) — experience mixed coordination/extraction; benefit from tradition-respecting innovation narrative; constrained by interpretive straightjacket
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating logical consistency between continuity and unchanging doctrine as natural law rather than institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.35).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.52).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Vatican II as Organic Development (Continuity Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, 'd8a69340-bef6-4c48-8b27-f9769a6e138e').
narrative_ontology:cs_kernel_codification('d8a69340-bef6-4c48-8b27-f9769a6e138e', fixed_text).
narrative_ontology:cs_authority_grounding('d8a69340-bef6-4c48-8b27-f9769a6e138e', extraction).
narrative_ontology:cs_interpretation_layer_present('d8a69340-bef6-4c48-8b27-f9769a6e138e').
narrative_ontology:cs_reading_relation('d8a69340-bef6-4c48-8b27-f9769a6e138e', vatican_ii_doctrinal_authority__rupture_progressive_reading, forecloses).
narrative_ontology:cs_reading_relation('d8a69340-bef6-4c48-8b27-f9769a6e138e', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8a69340-bef6-4c48-8b27-f9769a6e138e', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('d8a69340-bef6-4c48-8b27-f9769a6e138e', foundational, doctrinal_identity_persists_through_development).
narrative_ontology:cs_axiom_status(doctrinal_identity_persists_through_development, holdable).
narrative_ontology:cs_axiom_grounding('d8a69340-bef6-4c48-8b27-f9769a6e138e', doctrinal_identity_persists_through_development, deontological).
narrative_ontology:cs_axiom('d8a69340-bef6-4c48-8b27-f9769a6e138e', foundational, implicit_prior_teaching_availability).
narrative_ontology:cs_axiom_status(implicit_prior_teaching_availability, holdable).
narrative_ontology:cs_axiom_grounding('d8a69340-bef6-4c48-8b27-f9769a6e138e', implicit_prior_teaching_availability, empirically_contingent).
narrative_ontology:cs_reference_frame('d8a69340-bef6-4c48-8b27-f9769a6e138e', pre_conciliar_doctrinal_stability).
narrative_ontology:cs_drift_state('d8a69340-bef6-4c48-8b27-f9769a6e138e', contemporary_scholarship_accumulation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d8a69340-bef6-4c48-8b27-f9769a6e138e', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, institutional_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, pre_conciliar_doctrinal_custodians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, post_conciliar_reform_movements).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, historical_contingency_acknowledgment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISTORICAL CONSCIOUSNESS (SNARE) — Agents (theologians, exegetes) who perceive genuine doctrinal rupture or historical contingency are trapped by the continuity reading's hermeneutical closure. The reading forecloses alternative intellectual frameworks by treating discontinuity claims as implementation errors or heterodoxy. No exit from the framework without institutional consequences. Maximum extraction: the constraint forces assent to the narrative while barring the empirical observations that would validate competing readings.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LITURGICAL REFORM ARCHITECTS (TANGLED ROPE) — Conciliar reformers experience the continuity reading as a mixed constraint. It provides legitimate authority for practical innovations (vernacular Mass, simplified rubrics) as 'explications of implicit tradition' rather than ruptures. Yet the same reading constrains their freedom — they must justify every change as organic development, not acknowledge the strategic choices involved. Benefits from the coordination function (tradition-respecting innovation narrative); constrained by the interpretive straightjacket.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__continuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: VATICAN MAGISTERIUM (ROPE) — The institutional Church benefits from the continuity reading as a coordination mechanism. It provides a narrative framework enabling pastoral adaptation without doctrinal admission, maintaining institutional authority across the rupture (whether rupture occurred or not). The magisterium experiences minimal extraction — the constraint enables flexible authority. Arbitrage: can shift interpretation as needed while maintaining the 'unchanging tradition' claim.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRADITIONALIST CRITICS (TANGLED ROPE) — Organized traditionalists (SSPX, sedevacantists, conservative bishops) benefit from the continuity reading's framework — it provides grounds for distinguishing 'true Council' (continuity) from 'heterodox implementation' (rupture). Yet they are constrained by the same reading's capacity to absorb their critiques as mere implementation quibbles. The reading's plasticity enables both defense of the Council and critique of post-conciliar abuses without institutional break.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__continuity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HERMENEUTICAL CONSERVATION (PITON) — The continuity reading's institutional apparatus (Vatican academies, doctrinal commissions, episcopal conferences tasked with 'authentic implementation') is largely performative. The interpretive machinery claims to adjudicate organic development but operates through circular reasoning: textual innovations are developments because they're authorized; authorizations are organic because they're textual. Theater persists through institutional inertia; the functional verification (whether the reading explains actual conciliar intent) is deferred indefinitely.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL ESSENTIALIST (MOUNTAIN) — From a civilizational/universal perspective, doctrinal identity is logically immutable: if the Council changed doctrine, it was not organic development but rupture. If it was organic development, doctrine did not change. This perspective treats the continuity/rupture distinction as a law of logical consistency, not contingent historical fact. However, the structural data contradicts the mountain classification — the reading achieves its effects through institutional enforcement and hermeneutical closure, not logical necessity.
constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vatican_ii_doctrinal_authority__continuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, TR),
    TR >= 0.70.

:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.35): Moderate. The continuity reading extracts from historical-consciousness agents and reform movements by foreclosing alternative narratives, yet provides genuine coordination function for the institutional magisterium (enabling pastoral flexibility without doctrinal rupture). The extraction is real (suppression of rival readings, institutional sanctions against dissent) but mixed with coordination benefits (shared narrative enabling institutional coherence). Suppression (0.52): Moderate-high. The reading suppresses alternative hermeneutical frameworks through institutional authority (publication controls, doctrinal notifications, appointment vetoes) while maintaining plausible deniability (alternatives are treated as implementation errors or heterodoxy, not as legitimate readings). The suppression is increasing over time as post-conciliar historical scholarship generates more evidence of genuine novelty. Theater ratio (0.68): High and increasing. The hermeneutical machinery required to sustain continuity claims becomes more elaborate as cases accumulate where the continuity narrative strains against textual evidence. The apparatus (Vatican academies, doctrinal commissions, episcopal conferences) performs the work of defending the reading but increasingly appears performative rather than substantive.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates internal perspectival divergence that creates the mandatrophy. The magisterium sees coordination (Rope) — a framework enabling institutional flexibility and authority preservation. The historical consciousness movement sees pure extraction (Snare) — the reading forecloses their empirical observations. The reform architects see mixed coordination/extraction (Tangled Rope) — they benefit from the innovation narrative but are constrained in acknowledging innovation. The traditionalists see Tangled Rope (mixed benefit and constraint). The hermeneutical apparatus itself sees Piton (performative conservation). The analytical observer risks seeing Mountain (logical consistency) but the structural data reveals this as a false summit: the reading's force derives from institutional enforcement, not logical necessity. The perspectival gap is not accidental — it is generated by the reading's dual function: enabling coordination for institutional actors while suppressing alternative frameworks for scholarship.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position relative to the extraction flow. Institutional beneficiaries (magisterium, pre-conciliar custodians) experience low d (around 0.15–0.25) — they benefit from the reading's narrative stability and authority preservation. Institutional actors deriving benefit from arbitrage options experience even lower d (negative χ). Trapped agents (historical consciousness movement) experience high d (0.85–0.95) — they cannot exit the framework without institutional consequences. Moderate agents (reform architects) experience mid-range d (0.50–0.65) — they experience mixed coordination and constraint. Organized agents (traditionalists) experience mid-range d (0.45–0.60) — they can leverage the reading's framework but are absorbed by its flexibility. The analytical observer at the civilizational scope experiences d around 0.72 (canonical analytical), but the composition of the constraint as institutional extraction plus hermeneutical closure produces a false-summit signal.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the continuity reading is one institutional hermeneutics among three live competitors (continuity, progressive rupture, traditionalist rupture). The reading achieves Tangled Rope classification because it simultaneously enables and constrains: it enables pastoral innovation by framing it as development (coordination function), while constraining intellectual honesty about the innovation (extraction function). The reading is not 'correct' or 'incorrect' in a natural-law sense — it is an institutional choice that produces both benefits (unified narrative, authority preservation) and costs (foreclosure of rival readings, hermeneutical strain as historical evidence accumulates). The analytical observer's risk of a Mountain classification is a false summit: the reading's apparent immutability (you cannot have both continuity and rupture) is logical form, not structural reality. The institutional machinery required to enforce the reading is substantial and increasing (theater ratio rising from 0.50 to 0.68 across the interval), indicating that the reading's plausibility is decreasing — more performative work is required to sustain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_prior_teaching_determinacy,
    'What constitutes evidence that a post-conciliar innovation was ''implicit'' in prior teaching versus genuinely novel?',
    'Historical textual analysis: systematic comparison of pre-conciliar magisterial statements with post-conciliar claims of explication. Criteria for implicit-ness (textual proximity, thematic relationship, authority grounding) specified and tested against cases where continuity reading and rupture readings diverge most sharply (religious liberty, episcopal collegiality, ecumenism).',
    'If implicit-ness is determinate and verifiable: continuity reading is empirically defensible, ε drops to 0.20 (Rope). If implicit-ness requires interpretive stipulation: continuity reading is hermeneutical, ε stays at 0.35+ (Tangled Rope or higher). If no pre-conciliar precedent exists: explicit rupture, ε jumps to 0.65+ (Snare or Tangled Rope at minimum).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_prior_teaching_determinacy, empirical, 'Verifiability of implicit prior teaching claims').

omega_variable(
    conciliar_intent_versus_conciliar_text,
    'Does the continuity reading describe the documents'' actual textual content, the authors'' intended meaning, or a post-hoc institutional interpretation?',
    'Archival research on conciliar drafting (Acta Synodalia records, peritus notes, voting records on specific language). Reconstruction of what authors believed they were deciding at each stage. Comparison with what the continuity reading claims they decided. Identification of points where continuity narrative requires reading against textual evidence or conciliar voting patterns.',
    'If continuity reading matches authorial intent and textual content: ε stays 0.35 (mixed coordination/extraction). If continuity reading requires post-hoc reinterpretation against author intent: ε rises to 0.55+ (Tangled Rope moving toward Snare). If continuity reading suppresses known ambiguities in conciliar language: ε rises to 0.65+ (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conciliar_intent_versus_conciliar_text, empirical, 'Alignment of continuity reading with conciliar authorial intent and textual content').

omega_variable(
    hermeneutical_framework_foreclosure,
    'Does the continuity reading foreclose other hermeneutical frameworks (rupture readings) within Catholic institutional orthodoxy, or do all three readings coexist as live positions?',
    'Institutional history: survey of which readings are permitted in seminaries, publishing venues, Vatican academies, episcopal conferences. Analysis of instances where continuity reading is enforced via institutional sanction (publication bans, doctrinal notifications, appointment vetoes) versus cases where rupture readings circulate without sanction. Tracking of shifts in institutional tolerance over time (1960s–1970s versus 2010s–2020s).',
    'If continuity reading forecloses rupture readings institutionally: this is extraction mechanism (suppression ≥ 0.50, χ amplified). If rupture readings circulate as dissenting but livable positions: this is coordination-with-constraint (Tangled Rope stable). If institutional enforcement of continuity reading has weakened over time: theater ratio has risen and ε trending toward piton or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutical_framework_foreclosure, empirical, 'Institutional enforcement of continuity reading versus coexistence of sibling readings').

omega_variable(
    reading_selection_mechanism,
    'Why does the magisterium authorize the continuity reading rather than rupture readings? What institutional benefit accrues from narrative continuity?',
    'Institutional analysis: examination of authority claims, institutional legitimacy, succession of papal authority. Comparison of costs if Vatican II is portrayed as rupture (authority of pre-conciliar magisterium undermined, necessity of ongoing reform admitted, grounds for further revision opened) versus costs if portrayed as continuity (authority preserved, pastoral flexibility enabled, theological diversity absorbed without admission of change).',
    'If continuity reading is adopted for institutional benefit (extraction): ε stays 0.35+ (Tangled Rope). If continuity reading is the most accurate description of actual conciliar content: ε should drop. If selection mechanism is purely institutional self-preservation: this confirms reading is beneficiary (magisterium) protecting itself against victim (historical consciousness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_mechanism, conceptual, 'Institutional incentive structure driving continuity reading selection').

omega_variable(
    tradition_development_versus_invention,
    'Is the continuity reading grounded in a theological theory of development (Newman''s Essay on the Development of Christian Doctrine) or in ad-hoc stipulation of which changes count as developments?',
    'Theological analysis: systematic application of Newman''s (or other development theorists'') criteria for legitimate development to specific Council innovations. Identification of cases where the reading invokes development criteria and cases where it stipulates without criteria. Comparison of development claims across different post-conciliar changes — why are some innovations developments and others excesses or errors?',
    'If coherent development theory explains the reading: ε could be 0.25–0.35 (Rope or light Tangled Rope). If development criteria are applied inconsistently: ε rises to 0.45–0.55 (Tangled Rope or Snare candidate). If no development theory is invoked and continuity is simply asserted: ε rises further to 0.55+ (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tradition_development_versus_invention, conceptual, 'Theoretical grounding of development doctrine versus stipulation').

omega_variable(
    commencement_of_hermeneutics,
    'When does the continuity reading become operative? At the moment of conciliar closure (1965), after the first implementation conflicts emerged (1970s), after John Paul II explicitly adopted it (1980s), or retroactively as historical interpretation?',
    'Documentary history: identification of when ''organic development'' language first appears in magisterial statements after the Council. Tracking of shifts in papal interpretation and authorization (Vatican clarifications, papal encyclicals, doctrinal notifications). Analysis of whether continuity reading is claimed as the Council''s own self-understanding or as a later institutional interpretation.',
    'If continuity reading was the Council''s own framework: ε stays lower (0.30–0.35, Rope or light Tangled Rope). If continuity reading was adopted post-hoc to manage interpretation conflicts: ε rises (0.45–0.55, Tangled Rope or Snare). If timing of reading adoption correlates with institutional need to foreclose progressive reforms: this identifies reading as extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commencement_of_hermeneutics, empirical, 'Historical timeline of continuity reading''s adoption and enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vat2cont_tr_t0, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(vat2cont_tr_t5, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(vat2cont_tr_t10, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(vat2cont_be_t0, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(vat2cont_be_t5, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(vat2cont_be_t10, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(vat2cont_su_t0, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(vat2cont_su_t5, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(vat2cont_su_t10, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The vatican_ii_doctrinal_authority kernel contains four structurally distinct constraint stories: continuity_reading (ε=0.35, Tangled Rope; frames Council as development), rupture_progressive_reading (ε=0.48, Snare; frames Council as break enabling ongoing reform), rupture_traditionalist_reading (ε=0.62, Snare; frames Council as doctrinal error), composite_overdetermination_reading (ε=0.42, Tangled Rope; frames Council as multiple distinct changes). Each reading instantiates one hermeneutical frame drawn from a single contested kernel. The readings are not empirical alternatives competing to explain the same phenomenon — they are competing institutional choices about how to authorize or constrain post-conciliar reform. Linking through network.affects_constraints shows that adoption of the continuity reading influences the viability and framing of the other three readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__continuity_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
