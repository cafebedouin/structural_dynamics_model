% ============================================================================
% CONSTRAINT STORY: biblical_authority__conciliar_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__conciliar_reading, []).

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
 *   constraint_id: biblical_authority__conciliar_reading
 *   human_readable: Conciliar Reception of Scriptural Authority (Ecumenical Councils and Patristic Consensus)
 *   domain: theology/religious/history_of_christianity
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the biblical_authority kernel:
 *   scripture's authority is exercised through ecumenical councils ratified
 *   by patristic consensus and received in the church's liturgical life;
 *   tradition is a living continuity carried by autocephalous churches, not a
 *   decree issued by a single magisterial office. The standing arrangement
 *   under contest — the thing this story measures — is that
 *   conciliar-reception machinery itself, assessed by this reading's own
 *   lights. The claim and the metrics are independent authored facts: the
 *   reading is CLAIMED as tangled_rope because it visibly possesses both a
 *   genuine coordination function (binding dispersed autonomous churches to
 *   common doctrine without a monarch-bishop) and asymmetric extraction (the
 *   episcopal college controls the rate of doctrinal change, and the
 *   beneficiaries of that control are the bishops themselves). The authored
 *   metrics describe moderately extractive, actively enforced operation whose
 *   enforcement arm has decayed since its imperial peak; the engine computes
 *   per-seat classifications from the structural data and owns any divergence
 *   from the claim.
 *
 * KEY AGENTS:
 *   - - autocephalous_episcopates: agenda-setting beneficiary (institutional/constrained) — convenes synods, ratifies doctrine, disciplines teachers; collects interpretive authority collectively
 *   - - ancient_patriarchal_sees: concentrated beneficiary with agenda-setting power (institutional/identity_locked) — hold precedence and anchor consensus claims; cannot exit without ceasing to be themselves
 *   - - monastic_ascetic_establishment: secondary beneficiary (organized/identity_locked) — supplies continuity and credibility, receives veneration and informal veto
 *   - - imperial_and_state_powers: historical co-agenda-setter and beneficiary (institutional/mobile) — convoked and enforced; withdrew patronage when policy shifted
 *   - - lay_faithful: near-symmetric participant (moderate/constrained) — receives stability and sacramental life, pays in slowed adaptation and lost teachers
 *   - - doctrinal_reform_movements: primary target (moderate/constrained) — bear the cost of the conciliar timetable
 *   - - condemned_teachers_and_followers: primary target (powerless/trapped) — bear anathema, exclusion, and civil penalty
 *   - - non_ordained_theologians: excluded voice (moderate/mobile) — labor for the tradition without a seat in it
 *   - - academic_patristics_scholars: analytical observer (analytical/analytical) — sees how consensus was actually manufactured and received
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__conciliar_reading, 0.58).
domain_priors:suppression_score(biblical_authority__conciliar_reading, 0.57).
domain_priors:theater_ratio(biblical_authority__conciliar_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, suppression_requirement, 0.57).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__conciliar_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__conciliar_reading, "Conciliar Reception of Scriptural Authority (Ecumenical Councils and Patristic Consensus)").
narrative_ontology:topic_domain(biblical_authority__conciliar_reading, "theology/religious/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__conciliar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__conciliar_reading, 'c2240079-4e6e-458e-be31-843cd6b9379e').
narrative_ontology:cs_kernel_codification('c2240079-4e6e-458e-be31-843cd6b9379e', fixed_text).
narrative_ontology:cs_authority_grounding('c2240079-4e6e-458e-be31-843cd6b9379e', lineage).
narrative_ontology:cs_interpretation_layer_present('c2240079-4e6e-458e-be31-843cd6b9379e').
narrative_ontology:cs_reading_relation('c2240079-4e6e-458e-be31-843cd6b9379e', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('c2240079-4e6e-458e-be31-843cd6b9379e', biblical_authority__tradition_scripture_reading, influences).
narrative_ontology:cs_axiom('c2240079-4e6e-458e-be31-843cd6b9379e', foundational, authoritative_interpretation_requires_conciliar_patristic_reception).
narrative_ontology:cs_axiom_status(authoritative_interpretation_requires_conciliar_patristic_reception, holdable).
narrative_ontology:cs_axiom_grounding('c2240079-4e6e-458e-be31-843cd6b9379e', authoritative_interpretation_requires_conciliar_patristic_reception, theological).
narrative_ontology:cs_axiom('c2240079-4e6e-458e-be31-843cd6b9379e', foundational, tradition_is_living_continuity_not_magisterial_decree).
narrative_ontology:cs_axiom_status(tradition_is_living_continuity_not_magisterial_decree, holdable).
narrative_ontology:cs_axiom_grounding('c2240079-4e6e-458e-be31-843cd6b9379e', tradition_is_living_continuity_not_magisterial_decree, theological).
narrative_ontology:cs_axiom('c2240079-4e6e-458e-be31-843cd6b9379e', secondary, council_decisions_validated_by_whole_church_reception).
narrative_ontology:cs_axiom_status(council_decisions_validated_by_whole_church_reception, holdable).
narrative_ontology:cs_axiom_grounding('c2240079-4e6e-458e-be31-843cd6b9379e', council_decisions_validated_by_whole_church_reception, conventional).
narrative_ontology:cs_reference_frame('c2240079-4e6e-458e-be31-843cd6b9379e', undivided_pentarchic_conciliar_order).
narrative_ontology:cs_drift_state('c2240079-4e6e-458e-be31-843cd6b9379e', post_imperial_autocephalous_fragmentation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c2240079-4e6e-458e-be31-843cd6b9379e', '').
narrative_ontology:cs_kernel_id(biblical_authority__conciliar_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, autocephalous_episcopates).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, ancient_patriarchal_sees).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, doctrinal_reform_movements).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, condemned_teachers_and_followers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, monastic_ascetic_establishment).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, imperial_and_state_powers).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, lay_faithful).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, lay_faithful).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, patristic_consensus_norm).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, conciliar_reception_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The bishops of the self-governing churches meet in synod and council, ratify doctrine, consecrate successors, and discipline teachers. Their collective consent is the organ through which scripture's meaning is settled; no bishop can act alone, and no bishop can leave his see without deposition or schism.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, autocephalous_episcopates, agenda_setter,
    institutional, generational, constrained, global).

% The ancient thrones (Jerusalem, Antioch, Alexandria, Constantinople, and in the Slavic sphere Moscow) hold honor precedence, chair and convoke synods, and anchor claims that a decision expresses the whole church's mind. Each see's identity is constituted by its place in this order; abandoning the order would mean ceasing to be that see.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, ancient_patriarchal_sees, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, ancient_patriarchal_sees, agenda_setter).

% Monasteries preserve the liturgical and ascetical continuity in which the tradition is said to live, supply theologians, witnesses, and martyrs, and lend the whole arrangement its credibility. They receive veneration, land, and informal veto power over innovation in return; vows bind members for life.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, monastic_ascetic_establishment, beneficiary,
    organized, generational, identity_locked, continental).

% Emperors convoked the first councils, supplied the enforcement arm for their decrees, and gained social cohesion from doctrinal uniformity. Later kingdoms and empires patronized, subsidized, or subdued national churches as policy required, and could withdraw patronage when convenient, as the Ottoman and Soviet periods each demonstrated.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, imperial_and_state_powers, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, imperial_and_state_powers, beneficiary).

% The laity receive doctrinal stability, liturgical continuity, and sacramental life, and their eventual reception is invoked to validate council decisions. They bear the cost of slow doctrinal change and of teachers they esteem being condemned; leaving means surrendering communal and sacramental ties formed over a lifetime.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, lay_faithful, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, lay_faithful, beneficiary).

% Movements proposing correction or development of doctrine must win episcopal sponsorship and eventually conciliar reception, a process that typically outlasts the founders' lifetimes. Many decline into separatist communities or wait generations for vindication; staying inside costs them their timetable, leaving costs them the communion they sought to reform.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, doctrinal_reform_movements, payer,
    moderate, biographical, constrained, regional).

% Teachers anathematized by synods lose teaching posts, communion, and in Christian polities civil standing; their followers face social and economic pressure to conform. Exit leads out of the community entirely rather than to a tolerated minority position inside it.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, condemned_teachers_and_followers, payer,
    powerless, biographical, trapped, regional).

% Scholars and critics without orders produce much of the tradition's intellectual labor but hold no seat in any synod; their influence depends entirely on bishops choosing to hear them. They can move into academia, but not into the room where doctrine is decided.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, non_ordained_theologians, excluded,
    moderate, biographical, mobile, global).

% Historians of doctrine reconstruct how conciliar decisions were actually reached, what the cited patristic consensus consisted of before curation, and how reception worked in practice. They take no side in the arrangement and bear neither its honors nor its anathemas.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, academic_patristics_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__conciliar_reading, autocephalous_episcopates).
narrative_ontology:fixing_cost_class(biblical_authority__conciliar_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains doctrinal unity and a shared reading of scripture across hundreds of autonomous churches that acknowledge no single monarch-bishop: councils aggregate dispersed episcopal judgment into binding definitions, and the patristic corpus supplies a common precedent set so that settled questions are not re-litigated in every generation and every see.
% TRANSFER_FUNCTION: Moves interpretive authority and control over the rate of doctrinal change from individual believers, local teachers, and single churches to the episcopal college assembled in council; moves honor, precedence, and agenda-setting power to the ancient sees; places the cost of delayed doctrinal correction on reform movements and condemned teachers.
% ABSENT_VOICES: Non-ordained theologians, women (structurally barred from every episcopal seat), and lay delegates have no vote in synods; their objections reach the process only when a bishop chooses to voice them. Historically, condemned parties were heard chiefly at the councils convened to judge them, and the laity whose 'reception' validates decisions were never polled.
% DISAPPEARANCE_RATIONALE: If conciliar authority vanished overnight, the autocephalous churches would fragment doctrinally within a generation — the 1054 rupture and the 2016 Crete boycott both preview the dynamic — mutual sacramental recognition would unravel, and every deferred question (jurisdictional boundaries, calendar, the filioque-class disputes, sexual ethics) would reopen simultaneously with no agreed mechanism for closing any of them.
% FOUNDING_PROBLEM: The fourth-century crisis showed that scripture read by competent, sincere readers yielded mutually exclusive gospels, threatening both the church's unity and the empire's order; the arrangement was built to settle doctrine by aggregating the church's dispersed episcopal judgment against a shared patristic standard, so that one gospel would bind all churches.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the documentary record of the Arian and Christological disputes predates the councils that resolved them (Arius's own letters, Alexander of Alexandria's circulars, imperial correspondence), and secular historians of early Christianity across confessional lines attest that the councils responded to disputes already splitting communities, rather than manufacturing the problems they claim to solve. No party outside the episcopate, however, attests that the conciliar mechanism remains adequate to the problem today — that assessment is disputed.
narrative_ontology:disappearance_verdict(biblical_authority__conciliar_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__conciliar_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__conciliar_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__conciliar_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__conciliar_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__conciliar_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__conciliar_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__conciliar_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.58 at interval end) and accumulated slowly: at Nicaea the arrangement was doing freshly-needed work against a live crisis, and rents layered onto the coordination function over centuries as episcopal control of doctrinal change hardened into property. Suppression (0.57) tracks the enforcement story rather than the extraction story — it rose with the imperial-canon-law symbiosis (emperors supplied the coercive arm from 325 through the medieval peak), plateaued, and has decayed since the nineteenth century as states stopped enforcing anathemas; the series is deliberately non-monotonic because the story traces enforcement-capacity change, not just extraction shift. Theater rises steadily (0.10 to 0.38): appeals to 'the consensus of the fathers' grow more rhetorical as the living consensus thins — the 2016 Pan-Orthodox Council, boycotted by four autocephalous churches, is the clearest recent instance of consensus being proclaimed faster than it exists. Accessibility collapse sits at 0.5: within the accepted frame, private doctrinal innovation collapses as an option, but exit by schism remains structurally available and has been repeatedly taken (Chalcedon's aftermath, 1054, the Old Believers, Crete's boycotters), so alternatives never fully close. Resistance at 0.55 reflects fifteen centuries of real pushback from exactly the seats the arrangement extracts from. All three series run on one shared eight-point grid so no metric is sampled against another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The episcopal and patriarchal seats should compute as experiencing a coordination structure they steward: from inside the college, a council is the church finding its own voice, and the slowness is fidelity, not cost. The reform-movement and condemned-teacher seats should compute the same structure as enforced extraction: a timetable controlled by rivals, anathema as the enforcement instrument, and exit priced at total communal loss. The laity sit near symmetric — genuine sacramental and doctrinal goods received, diffuse costs paid. The engine computes this per-seat divergence from power, exit, and directional position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to the low-d end: the episcopates (collect the interpretive authority), the patriarchal sees (concentrate honor and agenda power), the monastic establishment (collects credibility rents without running councils), and the state powers (collected social cohesion from uniformity). Victims map to the high-d end: doctrinal reform movements (pay the conciliar timetable with their founders' lifetimes) and condemned teachers and followers (pay anathema and civil standing). The laity are declared payer with secondary beneficiary, placing them near the symmetric middle — they are the arrangement's claimed validation mechanism ('reception') yet were never consulted. No directionality overrides are used: the beneficiary/victim declarations plus exit options already differentiate the seats correctly, and the override surface (keyed by power atom) is too coarse to distinguish the two moderate-power seats that sit on opposite sides of the transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents misclassification in both directions. Reading the arrangement as pure coordination (rope) would erase the accumulated episcopal rents and the systematic cost imposed on doctrinal adaptation — the very asymmetry the 2016 boycott made visible. Reading it as pure extraction (snare) would erase the real collective-action problem solved: Nicaea and Chalcedon closed crises that private reading demonstrably could not, and no alternative mechanism has ever held the autocephalous system together. The R5 interview shows the founding problem is still live and the world would rearrange on disappearance, so no mandatrophy resolution is declared and no zombie mismatch arises (status=live x verdict=world_rearranges). The open risk runs the other way: if fragmentation proceeds far enough, the coordination function dies while the theatrical consensus-maintenance persists — at that point the arrangement converts toward piton, and the theater_ratio series (0.10 to 0.38 and climbing) is the leading indicator to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does this classification hold for the conciliar reading specifically, or is part of the measured structure an artifact of evaluating the shared kernel from one reading''s seat?',
    'Compile the three sibling stories (biblical_authority__sola_scriptura_reading, biblical_authority__tradition_scripture_reading) and compare per-seat classifications and effective extraction across the family; divergent verdicts on shared seats localize the reading-indexed component.',
    'If the sibling readings compute materially different types over the same historical enforcement record, the extraction measured here is partly a property of the reading''s authority diffusion rather than of the kernel''s enforcement machinery, and cross-kernel comparisons must be reading-indexed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'This story is one reading of the biblical_authority kernel; classification may be indexical to the reading.').

omega_variable(
    patristic_consensus_authenticity,
    'Is the ''patristic consensus'' the councils enforce an actually-existing convergence discoverable in the fathers, or a retrospective curation assembled by later councils from selected quotations?',
    'Source-critical reconstruction of the pre-conciliar textual record: trace each council''s proof-texts to their original contexts and measure whether the cited agreement existed before the citation practice began.',
    'If the consensus is substantially retrospective, the theater_ratio understates performance — the arrangement manufactures the standard it then enforces, and effective extraction rises accordingly; if the consensus is largely authentic, the measured extraction is closer to pure coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patristic_consensus_authenticity, empirical, 'Whether the enforced standard precedes its enforcement or was curated by it.').

omega_variable(
    imperial_enforcement_share,
    'What share of the arrangement''s historical suppression was supplied by imperial and state coercion rather than by the churches'' own disciplinary capacity?',
    'Compare enforcement outcomes in periods of strong state patronage (Constantinian through Byzantine) against periods of weak or hostile state power (Ottoman millet administration, Soviet restriction): if anathemas bite only when backed by civil penalty, the churches'' intrinsic suppression capacity is the lower bound.',
    'If most measured suppression was borrowed from the state, the post-imperial decay in the suppression series is structural rather than contingent, the arrangement''s standalone enforcement is weaker than the scalar suggests, and conversion toward piton accelerates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imperial_enforcement_share, empirical, 'How much of the enforcement record belongs to the state rather than the church.').

omega_variable(
    fragmentation_function_threshold,
    'At what degree of autocephalous fragmentation does the coordination function stop operating, converting the arrangement from working coordination with extraction into maintained performance?',
    'Track pan-Orthodox conciliar participation rates and post-conciliar implementation across autocephalous churches: sustained boycotts followed by unilateral action (the 2016 Crete pattern generalizing) would mark the crossing.',
    'Past the threshold, the claimed tangled_rope converts toward piton — the episcopate retains the forms of consensus while no binding coordination occurs — and the theater_ratio becomes the primary classification signal rather than a symptom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fragmentation_function_threshold, conceptual, 'Where fragmentation defeats coordination and only performed consensus remains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__conciliar_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ba_conciliar_tr_t325, biblical_authority__conciliar_reading, theater_ratio, 325, 0.1).
narrative_ontology:measurement(ba_conciliar_tr_t451, biblical_authority__conciliar_reading, theater_ratio, 451, 0.13).
narrative_ontology:measurement(ba_conciliar_tr_t787, biblical_authority__conciliar_reading, theater_ratio, 787, 0.16).
narrative_ontology:measurement(ba_conciliar_tr_t1054, biblical_authority__conciliar_reading, theater_ratio, 1054, 0.22).
narrative_ontology:measurement(ba_conciliar_tr_t1453, biblical_authority__conciliar_reading, theater_ratio, 1453, 0.27).
narrative_ontology:measurement(ba_conciliar_tr_t1870, biblical_authority__conciliar_reading, theater_ratio, 1870, 0.31).
narrative_ontology:measurement(ba_conciliar_tr_t1965, biblical_authority__conciliar_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement(ba_conciliar_tr_t2025, biblical_authority__conciliar_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(ba_conciliar_be_t325, biblical_authority__conciliar_reading, base_extractiveness, 325, 0.34).
narrative_ontology:measurement(ba_conciliar_be_t451, biblical_authority__conciliar_reading, base_extractiveness, 451, 0.4).
narrative_ontology:measurement(ba_conciliar_be_t787, biblical_authority__conciliar_reading, base_extractiveness, 787, 0.46).
narrative_ontology:measurement(ba_conciliar_be_t1054, biblical_authority__conciliar_reading, base_extractiveness, 1054, 0.51).
narrative_ontology:measurement(ba_conciliar_be_t1453, biblical_authority__conciliar_reading, base_extractiveness, 1453, 0.54).
narrative_ontology:measurement(ba_conciliar_be_t1870, biblical_authority__conciliar_reading, base_extractiveness, 1870, 0.57).
narrative_ontology:measurement(ba_conciliar_be_t1965, biblical_authority__conciliar_reading, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement(ba_conciliar_be_t2025, biblical_authority__conciliar_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ba_conciliar_su_t325, biblical_authority__conciliar_reading, suppression_requirement, 325, 0.44).
narrative_ontology:measurement(ba_conciliar_su_t451, biblical_authority__conciliar_reading, suppression_requirement, 451, 0.5).
narrative_ontology:measurement(ba_conciliar_su_t787, biblical_authority__conciliar_reading, suppression_requirement, 787, 0.56).
narrative_ontology:measurement(ba_conciliar_su_t1054, biblical_authority__conciliar_reading, suppression_requirement, 1054, 0.6).
narrative_ontology:measurement(ba_conciliar_su_t1453, biblical_authority__conciliar_reading, suppression_requirement, 1453, 0.63).
narrative_ontology:measurement(ba_conciliar_su_t1870, biblical_authority__conciliar_reading, suppression_requirement, 1870, 0.64).
narrative_ontology:measurement(ba_conciliar_su_t1965, biblical_authority__conciliar_reading, suppression_requirement, 1965, 0.61).
narrative_ontology:measurement(ba_conciliar_su_t2025, biblical_authority__conciliar_reading, suppression_requirement, 2025, 0.57).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__conciliar_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__tradition_scripture_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'biblical authority' decomposes into three structurally distinct claims with different epsilon values — the conciliar reading (this file: diffused episcopal authority, moderate extraction, enforcement via anathema and reception), the sola_scriptura_reading (self-interpreting text, negligible structural extraction, no enforcement machinery), and the tradition_scripture_reading (centralized magisterial authority, concentrated extraction, enforcement via juridical decree). The conciliar reading is upstream of the magisterial sibling historically: conciliarist pressure shaped the magisterial reading's self-definition (Vatican I answered conciliarism), so this file links to both siblings via affects_constraints and documents the relations in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
