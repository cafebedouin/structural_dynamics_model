% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_creation_mihna, []).

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
 *   constraint_id: quran_ontological_status__state_enforced_creation_reading
 *   human_readable: Qur'an as Created: State-Enforced Doctrinal Orthodoxy via Mihna Inquisition
 *   domain: islamic_theology/political_authority/metaphysics
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel: the
 *   Qur'an's ontological status (created or uncreated). The reading you are
 *   analyzing is the state-enforced creation doctrine—specifically,
 *   al-Ma'mun's mihna (inquisition, 833-847 CE) that weaponized Mu'tazilite
 *   theological claims into a suppression mechanism targeting traditionalist
 *   scholars, particularly Ahmad ibn Hanbal. The constraint begins as a
 *   genuine philosophical dispute (whether the Qur'an is eternally uncreated,
 *   coexistent with God, or created by God) but transforms into a snare when
 *   state power enforces one position via institutional coercion. The
 *   metaphysical claim becomes a political tool: caliphal authority
 *   consolidates control by imposing doctrinal orthodoxy, suppressing
 *   dissent, and using inquisition tribunals to demand public affirmation.
 *   The core structural feature distinguishing this reading from sibling
 *   readings is the marriage of metaphysical doctrine to political
 *   enforcement—the constraint's high extractiveness (0.68) and suppression
 *   (0.72) derive from the enforcement mechanism, not the theology alone. The
 *   measurements show a clear escalation: suppression_requirement rises from
 *   0.45 (pre-mihna intellectual debate context) to 0.72 (peak inquisition)
 *   to 0.50 (post-mihna decline after al-Mutawakkil's reversal).
 *   Extractiveness follows the same trajectory. Theater ratio rises sharply
 *   during enforcement period (0.58 at peak), reflecting that the
 *   inquisition's verification mechanism is performative—a scholar's public
 *   affirmation before a judge tribunal certifies conformity through coerced
 *   speech, not through reasoned conviction. Post-mihna, as traditionalist
 *   doctrine is restored and enforcement ceases, both metrics decline.
 *
 * KEY AGENTS:
 *   - Caliphal Authority (al-Ma'mun, al-Ma'mun's successors): Primary beneficiary (institutional/arbitrage) — consolidates political control through doctrinal enforcement; uses rationalist theology as legitimacy mask.
 *   - Traditionalist Scholars (Ahmad ibn Hanbal, literalist jurists): Primary victim (powerless/trapped) — face imprisonment, torture, social exile for refusing public affirmation of created-Qur'an doctrine; identity-fused with literalist interpretation makes exit cognitively impossible.
 *   - Literalist Communities: Secondary victim (moderate/constrained) — mosque scholars, qadis, ordinary believers subject to institutional pressure and social stigmatization; exit is costly but not physically impossible.
 *   - Mu'tazilite Rational School: Temporary beneficiary (institutional/arbitrage) — gains state patronage and intellectual authority during enforcement period; benefit contingent on caliphal favor, so exit option exists.
 *   - Broader Scholarly Community (Ash'arites, later Maturidites): Mixed (moderate/constrained) — experience constraint as both coordination (doctrinal clarity) and extraction (suppressed pluralism); constrained exit (can migrate or practice quietly).
 *   - Post-Mihna Institutional Reset: Organized agents (organized/mobile) — open-science equivalent at institutional level; future caliphs reverse enforcement, implying sunset clause was implicit.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating political enforcement as metaphysical necessity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.68).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.72).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "Qur'an as Created: State-Enforced Doctrinal Orthodoxy via Mihna Inquisition").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "islamic_theology/political_authority/metaphysics").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, '8daa78d6-d2d6-4036-b522-b7ee7470dd8f').
narrative_ontology:cs_kernel_codification('8daa78d6-d2d6-4036-b522-b7ee7470dd8f', fixed_text).
narrative_ontology:cs_authority_grounding('8daa78d6-d2d6-4036-b522-b7ee7470dd8f', extraction).
narrative_ontology:cs_interpretation_layer_present('8daa78d6-d2d6-4036-b522-b7ee7470dd8f').
narrative_ontology:cs_reading_relation('8daa78d6-d2d6-4036-b522-b7ee7470dd8f', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('8daa78d6-d2d6-4036-b522-b7ee7470dd8f', quran_ontological_status__created_reading, coexists_with).
narrative_ontology:cs_axiom('8daa78d6-d2d6-4036-b522-b7ee7470dd8f', foundational, reason_supreme_scriptural_arbiter).
narrative_ontology:cs_axiom_status(reason_supreme_scriptural_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('8daa78d6-d2d6-4036-b522-b7ee7470dd8f', reason_supreme_scriptural_arbiter, empirically_contingent).
narrative_ontology:cs_axiom('8daa78d6-d2d6-4036-b522-b7ee7470dd8f', foundational, state_enforcement_doctrinal_correctness).
narrative_ontology:cs_axiom_status(state_enforcement_doctrinal_correctness, overridden).
narrative_ontology:cs_axiom_grounding('8daa78d6-d2d6-4036-b522-b7ee7470dd8f', state_enforcement_doctrinal_correctness, instrumental).
narrative_ontology:cs_reference_frame('8daa78d6-d2d6-4036-b522-b7ee7470dd8f', caliphal_rationalist_supremacy).
narrative_ontology:cs_drift_state('8daa78d6-d2d6-4036-b522-b7ee7470dd8f', post_al_mutawakkil_reversal, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('8daa78d6-d2d6-4036-b522-b7ee7470dd8f', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, rationalist_mu_tazilite_school).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONALIST SCHOLAR (SNARE) — Ahmad ibn Hanbal and fellow literalist jurists face state inquisition tribunals demanding public affirmation of created-Qur'an doctrine. Refusal means imprisonment, torture, social exile. Exit options collapse: recanting means abandoning foundational theological conviction that constitutes scholarly identity; remaining silent means facing state coercion. Maximum extraction under minimal coordination benefit — the state enforces doctrinal conformity for political control, not for solving collective action problems.
constraint_indexing:constraint_classification(quran_ontological_status__state_enforced_creation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LITERALIST COMMUNITY (SNARE) — Local qadis, mosque scholars, ordinary believers who reject the rationalist innovation find themselves subject to inquisition. Costs include social pressure, career consequences for refusing public endorsement, fear of harboring dissident scholars. Exit is costly but not physically impossible — one can leave the public sphere, migrate, or conform under duress. Experienced extraction is severe because the mechanism targets identity-constituting theological claims; suppression runs through institutional authority (state judges, official tribunals) and social stigmatization.
constraint_indexing:constraint_classification(quran_ontological_status__state_enforced_creation_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CALIPHAL AUTHORITY (ROPE) — Al-Ma'mun and his successors benefit from doctrinal uniformity enforced via mihna. The created-Qur'an doctrine serves political control: it subordinates scriptural authority to rational interpretation, which the caliph controls through his appointed judges and theologians. This agent experiences the constraint as coordination — a mechanism for stabilizing authority and preventing theological pluralism from destabilizing governance. The caliph has arbitrage options: shift the doctrine back (as later caliphs did), or abandon enforcement. Net beneficiary throughout the enforcement period.
constraint_indexing:constraint_classification(quran_ontological_status__state_enforced_creation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MU'TAZILITE SCHOOL (ROPE) — Rationalist theologians benefit temporarily from state backing. The created-Qur'an doctrine elevates reason (aql) as the arbiter of truth, positioning rationalist scholars as authorities on scriptural interpretation. However, this benefit is contingent on maintaining caliphal favor — as political winds shift, the school loses state support. Exit option is arbitrage: they can relocate, serve different patrons, or recalibrate doctrine. During the mihna, they experience the constraint as coordination (reason + state authority jointly stabilizing doctrine) rather than extraction.
constraint_indexing:constraint_classification(quran_ontological_status__state_enforced_creation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: BROADER SCHOLARLY COMMUNITY (TANGLED ROPE) — Other schools (Ash'arite, later Maturidite philosophers) view the mihna as both a coordination problem (establishing shared standards for scriptural interpretation) and an extraction mechanism (state power suppressing theological pluralism). These agents benefit from doctrinal clarity (coordination function) but suffer from reduced intellectual freedom and risk of future inquisitions if they dissent (extraction function). Exit option is constrained: they can practice quietly, migrate to regions outside caliphal control, or eventually align with the post-mihna traditionalist resurgence. Experienced extraction is moderate because exit paths exist, but suppression through institutional authority is real.
constraint_indexing:constraint_classification(quran_ontological_status__state_enforced_creation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: POST-MIHNA INSTITUTIONAL RESET (SCAFFOLD) — From a generational view looking backward, the mihna appears as a temporary enforcement structure with an implicit sunset. Later caliphs (particularly al-Mutawakkil, 847-861 CE) formally reversed the created-Qur'an doctrine, restoring uncreated orthodoxy. This suggests the mihna was a bounded institutional experiment: a temporary coordination mechanism (establishing rational discourse norms) that devolved into extraction (state coercion) and was politically superseded. The scaffold classification emerges because the constraint had an exit path at the institutional level, even if individuals trapped within it had none.
constraint_indexing:constraint_classification(quran_ontological_status__state_enforced_creation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / METAPHYSICAL NECESSITY (MOUNTAIN) — From a civilizational perspective, the constraint appears as a necessary collision between two foundational claims about truth and authority: either scripture is uncreated (eternally coexistent with God) or created (God's speech). If either claim is metaphysically true, the opposing claim is necessarily false. This perspective risks naturalizing what is actually a political enforcement mechanism as a logical or metaphysical law. The engine's false-summit detection should flag this as manufactured universality masking state power. The constraint benefits the caliphal authority by treating doctrinal conformity as logically inevitable rather than politically contingent.
constraint_indexing:constraint_classification(quran_ontological_status__state_enforced_creation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__state_enforced_creation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quran_ontological_status__state_enforced_creation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quran_ontological_status__state_enforced_creation_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__state_enforced_creation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.68): High-moderate. The created-Qur'an doctrine as state-enforced orthodoxy extracts significant value from traditionalist scholars and communities: careers are ended, scholars imprisoned or exiled, intellectual freedom constrained by fear of inquisition. However, extractiveness is not maximal (0.72+) because (1) the constraint has temporal bounds (the mihna is reversed within 20 years), (2) exit options exist for some agents (Mu'tazilites, scholars willing to migrate), and (3) the extraction is primarily directed at a specific scholar class, not the entire population. The measurement trajectory from 0.35 to 0.68 to 0.40 shows escalation during enforcement and decline as the doctrine is reversed—consistent with a political suppression mechanism with a defined lifespan. SUPPRESSION (0.72): High. The inquisition tribunals employ direct state coercion (imprisonment, torture), social mechanisms (stigmatization, professional ruin), and institutional barriers (control of judicial appointments). Traditionalist scholars face collapsed exit options: they cannot recant without abandoning foundational theological identity; they cannot remain silent without facing tribunal pressure. The suppression mechanism targets both material conditions (career, freedom of movement) and cognitive/identity conditions (forcing public statements that contradict core beliefs). Theater ratio peaks at 0.58 during enforcement, reflecting that the inquisition's verification mechanism is performative—a scholar stands before a state judge and affirms the created-Qur'an doctrine, which counts as legitimate enforcement of orthodoxy. This is pure theater: the scholar's internal conviction is irrelevant; only the public performance matters. Post-mihna, as the doctrine is reversed and enforcement ceases, theater ratio declines to 0.35, approaching the baseline of genuine theological debate. CLAIMED TYPE: SNARE. The constraint classifies as snare because it exhibits high effective extraction (chi ≥ 0.66 from powerless/trapped perspective), high base extraction (ε = 0.68), and high suppression (0.72) with minimal coordination function—the state enforces doctrinal conformity for political consolidation, not for solving collective action problems. The mihna is extraction masked by theological legitimacy, not genuine coordination.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals maximal perspectival disagreement. The traditionalist scholar (powerless/trapped) experiences pure snare: coercion, identity conflict, and no exit path. The literalist community (moderate/constrained) experiences snare: high extraction but with some organizational options and eventual post-mihna vindication. The caliphal authority (institutional/arbitrage) experiences rope: coordination of doctrinal stability, unified authority, and rational legitimacy—pure benefit with no experienced extraction. The Mu'tazilite school (institutional/arbitrage) also experiences rope: temporary state backing, intellectual authority, and high exit optionality. The broader scholarly community (moderate/constrained) experiences tangled rope: mixed coordination (doctrinal clarity) and extraction (suppressed pluralism). The post-mihna institutional reset (organized/mobile) experiences scaffold: a temporary enforced doctrine with an implicit sunset, whose reversal suggests the enforcement was contingent, not inherent. The analytical observer (analytical/analytical) risks experiencing mountain: the false summit that treats doctrinal enforcement as metaphysically necessary collision between created and uncreated truths. The perspectival gap shows that the same structural phenomenon—state enforcement of a theological doctrine—appears as an immutable law (mountain) from one angle, a temporary institutional experiment (scaffold) from another, pure suppression (snare) from the target's angle, and coordination (rope) from the beneficiary's angle. No single perspective captures the constraint's full structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality d is derived from the agent's structural position relative to extraction flow. Traditionalist scholars: beneficiary of constraint = no (they are suppressed), victim = yes (imprisoned, exiled), power = powerless, exit = trapped. Derived d = 0.95 (nearly full target). Applied to f(d) ≈ 1.42, producing high χ. Caliphal authority: beneficiary = yes (consolidates power), victim = no, power = institutional, exit = arbitrage. Derived d ≈ 0.05 (nearly full beneficiary). Applied f(d) ≈ -0.12, producing negative χ (constraint subsidizes this agent). Mu'tazilite school: beneficiary = yes (state patronage), victim = no, power = institutional, exit = arbitrage. Same derivation: d ≈ 0.05, f(d) ≈ -0.12, negative χ. Broader scholarly community: beneficiary = partial (doctrinal clarity), victim = partial (suppressed pluralism), power = moderate, exit = constrained. Derived d ≈ 0.65, f(d) ≈ 1.00, moderate χ. These derivations confirm that the constraint's extraction flows toward caliphal benefit and Mu'tazilite benefit, away from traditionalist and literalist groups. The snare classification at high-extraction perspectives reflects that the beneficiaries' low-extraction experience coexists with the victims' high-extraction experience within the same structural constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED. The constraint avoids the mandatrophy trap by distinguishing the theological claim (created-Qur'an doctrine) from its enforcement mechanism (mihna inquisition). The theological claim alone might appear as rope (coordination of rational interpretation) or generative dispute. The enforcement mechanism is the snare: the state weaponizes the theology to suppress alternatives. The constraint's mandatrophy is resolved by recognizing that the state-enforced version IS a genuine snare (high extraction, high suppression, minimal coordination benefit for most agents), not a rope misclassified as snare or vice versa. The perspectival analysis confirms: from the beneficiary's angle (caliphal authority), the constraint appears as rope (coordination); from the victim's angle (traditionalist scholars), it is clearly snare (suppression). These are not contradictory—they are perspectival readings of the same asymmetric extraction. The constraint's mandatrophy is *resolved in favor of snare* because the beneficiary's perceived coordination function (doctrinal stability) is achieved entirely through the victim's suppression, making the coordination a consequence of extraction, not a genuine collective benefit. If the doctrinal uniformity could be achieved without suppression—through rational debate or voluntary adoption—the classification would shift toward rope. But the historical record shows suppression was necessary to impose the doctrine, confirming snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationalist_political_motivation,
    'Was the created-Qur''an doctrine adopted by al-Ma''mun as genuine philosophical conviction, or deployed primarily as a tool for consolidating caliphal authority?',
    'Historical analysis of al-Ma''mun''s correspondence, patronage patterns, and theological positions across different political contexts; examination of whether rationalist theology preceded or followed his access to state power.',
    'If genuine conviction: the constraint is a snare grounded in sincere metaphysical dispute. If political tool: the constraint is a pure extraction mechanism masked by theological framing (higher snare classification confidence). Either way, the beneficiary structure (caliphal authority) is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationalist_political_motivation, empirical, 'Whether created-Qur''an doctrine was ideological or instrumental motivation').

omega_variable(
    scholarly_resistance_coordination_failure,
    'Did traditionalist scholars fail to organize collective resistance because the doctrine was rationally compelling, or because state coercion prevented coordination?',
    'Examination of contemporary documents, fatwa networks, and institutional affiliations pre- and post-mihna; analysis of whether traditionalist scholars had communication channels (madrasas, legal councils, informal networks) that could have produced coordinated opposition.',
    'If rational persuasion: classification shifts toward rope (shared problem-solving). If suppression prevents coordination: classification solidifies as snare (extraction via institutional coercion preventing response). Either way affects mandatrophy resolution: was this a theological debate or a political suppression event?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scholarly_resistance_coordination_failure, empirical, 'Whether traditionalist silence resulted from rational agreement or suppressed organization').

omega_variable(
    kernel_contested_vs_resolved,
    'Is the Qur''an''s ontological status (created vs. uncreated) a settled metaphysical fact, a permanently contested philosophical question, or a political claim that should not be state-enforced?',
    'Theological analysis across Islamic traditions (Sunni, Shi''a, Ibadi, Sufi branches) showing persistence of multiple readings; examination of whether post-mihna traditionalist ascendancy represents discovery of truth or political reversal of doctrinal enforcement.',
    'If settled metaphysical fact: the loser''s perspective (traditionalists) was factually wrong; mihna was enforcement of truth. If permanently contested: both readings remain live; mihna was illegitimate suppression. If political question: the constraint''s mandatrophy is resolved by recognizing that neither doctrine should be state-enforced. This omega directly addresses the false-summit risk in Perspective 7.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contested_vs_resolved, conceptual, 'Whether Qur''an''s ontological status is settled fact, permanent disagreement, or depoliticizable question').

omega_variable(
    identity_lock_vs_genuine_belief_divergence,
    'For traditionalist scholars under mihna, does the resistance stem from genuine theological conviction (identity-fused with literalist interpretation) or from institutional investment (career, status, institutional affiliation dependent on traditionalist doctrine)?',
    'Biographical analysis of traditionalist scholars'' intellectual trajectories; examination of whether rationalist defectors maintained intellectual productivity or shifted to new theological frameworks; post-mihna analysis of whether traditionalist scholars who recanted returned to original positions or maintained modified views.',
    'If identity-locked: traditionalists'' perceived immutability of doctrine reflects cognitive binding, not external barriers alone; classification from traditionalist perspective remains snare (trapped via identity + suppression). If institutional investment: traditionalists were structurally vulnerable to exit pressure; exit options upgrade from trapped to constrained. Either way, suppression mechanism is confirmed, but the specifics of identity-based vs. structural binding matter for understanding why the doctrine persisted post-mihna.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_genuine_belief_divergence, empirical, 'Whether traditionalist resistance was identity-fused or institutionally rooted').

omega_variable(
    reading_distinctiveness_from_sibling_created_reading,
    'This reading (state-enforced creation doctrine) differs from the generic ''created-Qur''an reading'' by its emphasis on state enforcement via mihna. Is this a distinct constraint, or merely a variant of the same theological position?',
    'Comparative constraint analysis: the theological reading (created-Qur''an doctrine as philosophical claim) has distinct ε from this reading (the same doctrine plus state enforcement mechanisms). Measurement via: extractiveness without enforcement mechanisms vs. extractiveness with mihna tribunals; theater ratio of philosophical debate vs. theater ratio of political inquisition.',
    'If distinct constraint: this reading''s high extractiveness (0.68) and suppression (0.72) are driven by enforcement mechanism, not the doctrine itself. The theological reading might classify as rope (coordination of rational interpretation). If same constraint: the theological reading and political enforcement are structurally unified, and the doctrine is inherently linked to extraction (snare classification at all perspectives). The network decomposition hinges on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_distinctiveness_from_sibling_created_reading, conceptual, 'Whether state enforcement is intrinsic to this reading or separable from pure theological doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qcr_theater_t0_genuine_debate, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qcr_theater_t10_inquisition_performance, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(qcr_theater_t20_post_reversal, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(qcr_extract_t0_intellectual_debate, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qcr_extract_t10_peak_enforcement, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(qcr_extract_t20_post_mihna, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 20, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(qcr_suppression_t0_pre_mihna, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(qcr_suppression_t10_peak_mihna, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(qcr_suppression_t20_post_mihna_decline, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 20, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, abbasid_rationalist_ascendancy).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholarly_networks).

% DUAL FORMULATION NOTE:
% The state-enforced creation reading is downstream of the generic created-Qur'an theological doctrine but with distinct ε (0.68 vs. estimated 0.40 for the doctrine alone). The enforcement mechanism—mihna inquisition tribunals—is the primary driver of the higher extractiveness. This reading should be linked to both sibling theological readings and to institutional constraints (rationalist ascendancy, traditionalist networks) whose structures are materially affected by the enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
