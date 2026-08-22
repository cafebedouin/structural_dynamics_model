% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__political_swap_reading, []).

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
 *   constraint_id: reformation_event_boundary__political_swap_reading
 *   human_readable: Political-Swap Reading of the Reformation Event Boundary
 *   domain: historical_epistemology/religious_history
 *
 * SUMMARY:
 *   This story instantiates the political_swap_reading of the
 *   reformation_event_boundary kernel: the claim that the Reformation was
 *   primarily a political realignment — secular rulers exploiting theological
 *   disputes to break papal authority and seize ecclesiastical assets — with
 *   theology functioning as post-hoc rationalization and the event bounded by
 *   the Westphalian settlement of 1648. The constraint modeled here is that
 *   reading AS AN OPERATIVE INTERPRETIVE TEMPLATE: the shared
 *   causal-periodization scheme ('politics drove it; doctrine followed') that
 *   organizes survey teaching, examination standards, and a century of
 *   comparative bibliography. Two levels are kept distinct throughout. At the
 *   event level, the reading asserts a swap whose beneficiaries were
 *   territorial princes and whose victims were church institutions and
 *   confessionally coerced subjects; that asserted structure lives in the
 *   transfer function, the axioms, and the omegas. At the template level —
 *   the level the metrics measure — the constraint governs interpretive
 *   communities: it coordinates communication (any teachable scheme does)
 *   while taxing rival frameworks, and it requires active enforcement because
 *   the actors' own voluminous theological testimony continually pulls
 *   interpretation back toward doctrinal causation. The claim/metric gap is
 *   deliberate: claimed_type is authored from the template's structure
 *   (genuine coordination plus asymmetric extraction plus enforcement), while
 *   the metrics describe its observed operation; the engine computes per-seat
 *   classifications from the structural data. KEY AGENTS (by structural
 *   relationship): - secular_statist_historiography: Primary beneficiary
 *   (institutional/arbitrage) — collects explanatory territory and narrative
 *   convenience - disciplinary_gatekeepers: Agenda-setter
 *   (institutional/arbitrage) — administers enforcement via review, hiring,
 *   and curricula - public_history_curricula_boards: Secondary beneficiary
 *   (institutional/constrained) — consumes the template for teachable
 *   national narratives - confessional_theological_historiography: Primary
 *   payer (organized/identity_locked) — its explanatory category is demoted
 *   to rationalization - descendant_memory_communities: Payer
 *   (moderate/identity_locked) — ancestral conviction retroactively
 *   redescribed as cover - cultural_turn_revisionists: Payer
 *   (organized/constrained) — bears career costs for rival framings while
 *   supplying the template's strongest resistance -
 *   historiographical_meta_analysts: Analytical observer — sees the full
 *   three-reading structure
 *
 * KEY AGENTS:
 *   - secular_statist_historiography: primary beneficiary — institutional power, arbitrage-grade exit, collects the template's explanatory returns
 *   - disciplinary_gatekeepers: agenda-setter — institutional power, administers review/hiring/curriculum enforcement, collects career rents
 *   - public_history_curricula_boards: secondary beneficiary — institutional power, constrained exit, national scope
 *   - confessional_theological_historiography: primary payer — organized power, identity-locked exit, generational horizon
 *   - descendant_memory_communities: payer — moderate power, identity-locked exit, regional scope
 *   - cultural_turn_revisionists: payer and principal resister — organized power, constrained exit, global scope
 *   - historiographical_meta_analysts: analytical observer — no collection, no payment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.62).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.6).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Political-Swap Reading of the Reformation Event Boundary").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical_epistemology/religious_history").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, 'dac93df1-835c-4c6c-b23d-6dfc8dcebee5').
narrative_ontology:cs_kernel_codification('dac93df1-835c-4c6c-b23d-6dfc8dcebee5', distributed).
narrative_ontology:cs_authority_grounding('dac93df1-835c-4c6c-b23d-6dfc8dcebee5', expertise).
narrative_ontology:cs_interpretation_layer_present('dac93df1-835c-4c6c-b23d-6dfc8dcebee5').
narrative_ontology:cs_reading_relation('dac93df1-835c-4c6c-b23d-6dfc8dcebee5', reformation_event_boundary__theological_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('dac93df1-835c-4c6c-b23d-6dfc8dcebee5', reformation_event_boundary__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('dac93df1-835c-4c6c-b23d-6dfc8dcebee5', foundational, political_causation_primacy).
narrative_ontology:cs_axiom_status(political_causation_primacy, holdable).
narrative_ontology:cs_axiom_grounding('dac93df1-835c-4c6c-b23d-6dfc8dcebee5', political_causation_primacy, empirically_contingent).
narrative_ontology:cs_axiom('dac93df1-835c-4c6c-b23d-6dfc8dcebee5', foundational, doctrine_as_posthoc_rationalization).
narrative_ontology:cs_axiom_status(doctrine_as_posthoc_rationalization, holdable).
narrative_ontology:cs_axiom_grounding('dac93df1-835c-4c6c-b23d-6dfc8dcebee5', doctrine_as_posthoc_rationalization, empirically_contingent).
narrative_ontology:cs_axiom('dac93df1-835c-4c6c-b23d-6dfc8dcebee5', secondary, westphalian_settlement_completion).
narrative_ontology:cs_axiom_status(westphalian_settlement_completion, holdable).
narrative_ontology:cs_axiom_grounding('dac93df1-835c-4c6c-b23d-6dfc8dcebee5', westphalian_settlement_completion, conventional).
narrative_ontology:cs_reference_frame('dac93df1-835c-4c6c-b23d-6dfc8dcebee5', sovereign_authority_transfer_frame).
narrative_ontology:cs_drift_state('dac93df1-835c-4c6c-b23d-6dfc8dcebee5', contemporary_cultural_turn_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dac93df1-835c-4c6c-b23d-6dfc8dcebee5', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_statist_historiography).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, public_history_curricula_boards).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, confessional_theological_historiography).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, descendant_memory_communities).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, cultural_turn_revisionists).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, secularization_thesis).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, state_formation_teleology).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, methodological_atheism_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% University departments, journals, and research programs organized around state-formation and secular social science. They receive the template's principal returns: a Reformation that slots into comparative state-building narratives, examinable without doctrinal adjudication, backed by a century of compatible bibliography. Because the template is one tool among several they hold, adopting or dropping it carries little identity cost; they can pivot frames as research questions change.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, secular_statist_historiography, beneficiary,
    institutional, generational, arbitrage, global).

% Education ministries, examination boards, and textbook publishers who need a Reformation story that classrooms across confessional communities can share. The template gives them a politically framed account that avoids liturgy disputes. Their dependence on stable, teachable summaries makes them slow to revise, and their markets and oversight are national.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, public_history_curricula_boards, beneficiary,
    institutional, biographical, constrained, national).

% Journal editors, hiring committees, and series editors who operate the template's day-to-day enforcement: reviewing submissions, setting reading lists, staffing chairs, deciding which framings count as rigorous. They collect career rents and reputational returns from administering the prevailing frame, and switching frameworks would devalue their accumulated editorial and curricular capital.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, disciplinary_gatekeepers, agenda_setter,
    institutional, biographical, arbitrage, continental).

% Church-linked faculties, seminaries, and confessional research institutes for whom the Reformation's doctrinal content is the thing to be explained. The template demotes their explanatory category to rationalization. Engaging it on its own terms would require treating their tradition's core claims as cover, which their confessional commitments forbid; leaving would mean exiting institutions constituted by those commitments. They publish, accredit, and train across a global confessional network.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, confessional_theological_historiography, payer,
    organized, generational, identity_locked, global).

% Lutheran, Reformed, and Catholic heritage bodies, parishes, and regional memory institutions whose ancestors' convictions the template retroactively redescribes as political cover. They bear the redescription cost without sitting in the venues where the template is administered. Their attachment is inherited and constitutive rather than chosen, and it is anchored to specific regions, buildings, and graves.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, descendant_memory_communities, payer,
    moderate, generational, identity_locked, regional).

% Scholars of popular religion, ritual, print, and lived belief who document the religious experience the template flattens. They bear review friction and marginalization in template-administering venues while remaining employed inside the same departments and degree markets, so they contest rather than exit. Their accumulating archive of parish-level evidence is the template's most serious internal resistance.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, cultural_turn_revisionists, payer,
    organized, biographical, constrained, global).

% Historiographers and philosophers of history who study the classification dispute itself. They hold no stake in which reading prevails, observe all three readings' operations and enforcement patterns, and publish comparisons under which they neither collect nor pay.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, historiographical_meta_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__political_swap_reading, secular_statist_historiography).
narrative_ontology:fixing_cost_class(reformation_event_boundary__political_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one shared causal-periodization scheme (1517-1648, politics-led) that lets historians, teachers, and examiners across confessional lines describe, compare, and examine the Reformation without adjudicating doctrine, and slots the event into state-formation narratives usable in secular institutions.
% TRANSFER_FUNCTION: At the template level, moves explanatory authority and narrative ownership of the Reformation from confessional-theological traditions to secular-statist scholarship. Within the reading's own account of the underlying event, moves ecclesiastical lands, revenues, and jurisdictional authority from Roman and episcopal institutions to territorial princes.
% ABSENT_VOICES: Early-modern lay believers enter the template's account only as objects of policy. Anabaptist and spiritualist radicals — persecuted by Protestant princes and Catholic authorities alike — are absent from the settlement narrative entirely. Women's religious agency and parish-level practice appear chiefly through the revisionist literature the template marginalizes. In the contemporary field, Global South Christian memory communities sit outside the gatekeeping venues altogether.
% DISAPPEARANCE_RATIONALE: Survey courses, examination standards, and comparative state-formation bibliographies are organized around the template; overnight removal would force re-adjudication of a century of teaching and citation practice, and the vacated explanatory space would be contested immediately by the climb and composite readings.
% FOUNDING_PROBLEM: After the confessional wars and the sectarian stalemate, European states and universities needed an account of the Reformation that could be taught and examined without reopening doctrinal war: a politically framed narrative absorbed the event into administrative and national history.
% FOUNDING_PROBLEM_CORROBORATION: Confessional faculties historically accepted the political frame for civic instruction while retaining doctrinal accounts internally — their documented acquiescence attests the depolarization function from outside the beneficiary set, and curriculum histories corroborate the template's adoption in state schooling. No source outside the interested parties attests that the founding problem remains live today: defenders benefit from its liveness and confessional critics benefit from declaring it dead, so the current-status verdict rests on contested testimony.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__political_swap_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__political_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__political_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.62: the template's coordination value (a teachable, comparable, doctrine-free scheme) keeps it below pure-extraction range, but it levies a real displacement tax on rival frameworks and serves identifiable narrative interests. Suppression is 0.60: enforcement is professional-normative (peer review, hiring, canon formation) rather than coercive, but it must be continuous because counter-evidence — the actors' own extensive theological writing — is abundant and permanently available to rivals. Theater is 0.30: routine citation of material-base analysis has grown partly performative as primary archival energy migrated to cultural history, though the template still generates genuine analytic work (the confessionalization thesis is a political-institutional descendant doing live research). Accessibility collapse is 0.40: alternatives remain fully articulable — confessional faculties, church-history journals, and revisionist schools persist — so the template raises the cost of rivals rather than rendering them unthinkable. Resistance is 0.62: sustained confessional and cultural-turn pushback. All three tracked metrics run on one shared grid (1817, 1848, 1890, 1933, 1970, 2000, 2026). The interval opens at the 1817 Reformation jubilee, when Prussian national-Protestant memory fusion first institutionalized the political frame. Extraction climbs with professionalization and the secularization consensus, peaks mid-century, eases with the cultural turn, and ticks up recently as 'religion as cover' tropes return in public discourse. The 1933 suppression spike is provisionally attributed to regime capture rather than intrinsic enforcement need (see omega suppression_spike_attribution). Receipt surface: the template's gains demonstrably accrue to the secular-statist seat; fixing it would cost gatekeepers their accumulated editorial and curricular capital while the benefits of fixing would flow mainly to rival seats, so the cost class is prohibitive.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/gatekeeper seats compute differently. From the secular-statist and gatekeeping positions the template is a working tool they built, maintain, and profit from administratively; from the identity-locked payer positions the same structure operates as a standing demotion of their explanatory category. Among same-power payers, exit options differentiate outcomes: cultural_turn_revisionists (constrained) contest from inside the same job market, while confessional_theological_historiography (identity_locked) cannot adopt the template's terms without dissolving the framework that constitutes their institutions — so the latter sit nearer the full-target end than the former despite comparable organizational power. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: secular_statist_historiography (arbitrage exit) sits nearest the subsidy end — the template is one swappable tool among several it holds; public_history_curricula_boards (constrained exit) sits low but less extremely, since stable teachable summaries are a genuine need it would partially retain under any frame. Victim declarations drive high directionality: confessional_theological_historiography and descendant_memory_communities (both identity_locked) sit nearest the full-target end — their relationship to the template is constitutive, not contractual; cultural_turn_revisionists (constrained) sit high but below the identity-locked pair. Disciplinary_gatekeepers are not named in the beneficiary or victim arrays; their directionality derives from their administration of the arrangement and the career rents attached to administering it, placing them moderately toward the beneficiary side. The template operates globally across Western academe; the large spatial scope makes verification of rival-suppression harder across jurisdictions, which the engine reflects as modest amplification of effective extraction. Suppression, by contrast, is authored as a raw structural property and is not scaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid classification prevents two mislabels. Against pure extraction: the template's coordination function is genuine — after the confessional wars, European institutions needed an account of the Reformation teachable without reopening doctrinal war, and the template supplied it — and alternatives persist, so a pure-extraction label overstates. Against pure coordination: extraction is asymmetric (identity-locked payers bear costs beneficiaries never face) and the arrangement requires active enforcement, so a pure-coordination label understates. On mandate obsolescence: the founding problem (depolarized teachability) is contested rather than dead — defenders need it live, critics need it dead, and no disinterested corroborator attests current liveness. If the founding problem is in fact dead, the template persists by incumbency and should drift theatrical; the theater series eased after 1970 rather than climbing, and live analytic descendants exist, so mandatrophy is not resolved and no resolution flag is authored.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_location,
    'This constraint is one reading of the reformation_event_boundary kernel (political_swap_reading); the disagreement with theological_climb_reading and composite_overdetermination_reading is located in a single structural element — the causal-primacy variable (which driver, if any, captures the event). Which reading''s structural claims does the full evidentiary record sustain?',
    'Cross-reading adversarial collation: conversion-timing chronologies against princely fiscal-debt registers, doctrinal publication and correspondence data, and patronage records, adjudicated by mixed panels seating both confessional and secular scholars.',
    'If theological_climb_reading sustains, this template''s victim/beneficiary structure inverts (theologians become protagonists, princes instruments) and its measured extraction drops toward coordination-cost levels; if composite_overdetermination_reading sustains, this template survives only as one irreducible strand and loses the primacy-based legitimacy its enforcement currently rides on.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment_location, conceptual, 'Committer-frame location: all three readings stake incompatible claims on the causal-primacy variable of the shared kernel.').

omega_variable(
    ruler_conviction_vs_instrumentation,
    'Were the princes'' theological professions sincere conviction later instrumentalized, or cover from the outset? The template''s doctrine-as-post-hoc-rationalization axiom hinges on this distinction.',
    'Diachronic reading of rulers'' private correspondence, patronage ledgers, and conversion timing against fiscal-pressure chronologies (e.g., the divergent trajectories of Albertine and Ernestine Saxony).',
    'Substantial sincere conviction driving rulers would restore doctrinal causation, pushing this template toward the composite reading and lowering authored extraction; demonstrated pure instrumentation would confirm the template and raise effective extraction on the identity-locked memory seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ruler_conviction_vs_instrumentation, empirical, 'Sincerity of princely theology versus pure political cover.').

omega_variable(
    suppression_spike_attribution,
    'How much of the measured 1933-1970 suppression peak is the template''s intrinsic enforcement need, versus exogenous capture by totalitarian regimes that instrumentalized reductionist readings of religion for their own purposes?',
    'Compare enforcement trajectories across academic systems differentially exposed to fascist and Marxist-Leninist capture (Continental versus Anglophone versus neutral-country faculties) over the same interval.',
    'If the spike is exogenous, intrinsic suppression sits nearer 0.50 and the template computes closer to pure coordination at most seats; if intrinsic, the hybrid classification hardens toward pure extraction at the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_spike_attribution, empirical, 'Attribution of the mid-twentieth-century enforcement peak.').

omega_variable(
    event_boundary_periodization_ambiguity,
    'Does the swap''s event-boundary close at Westphalia 1648, at Augsburg 1555, or remain open through later confiscations (1685, 1803)? The template''s periodization axiom fixes 1648.',
    'Test continuity of the swap mechanism across candidate boundaries: if post-1648 confiscations run the same prince-versus-church asset logic, the 1648 boundary is conventional rather than structural.',
    'A 1555 closure shrinks the constraint''s spatial-temporal scope and lowers scope-amplified extraction; an open boundary extends victim exposure across two further centuries and strengthens the hybrid coordination-plus-extraction reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(event_boundary_periodization_ambiguity, conceptual, 'Where the political-swap event ends, and whether the boundary is structural or conventional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1817, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1817, reformation_event_boundary__political_swap_reading, theater_ratio, 1817, 0.18).
narrative_ontology:measurement(refo_tr_t1848, reformation_event_boundary__political_swap_reading, theater_ratio, 1848, 0.22).
narrative_ontology:measurement(refo_tr_t1890, reformation_event_boundary__political_swap_reading, theater_ratio, 1890, 0.28).
narrative_ontology:measurement(refo_tr_t1933, reformation_event_boundary__political_swap_reading, theater_ratio, 1933, 0.34).
narrative_ontology:measurement(refo_tr_t1970, reformation_event_boundary__political_swap_reading, theater_ratio, 1970, 0.38).
narrative_ontology:measurement(refo_tr_t2000, reformation_event_boundary__political_swap_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(refo_tr_t2026, reformation_event_boundary__political_swap_reading, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(refo_be_t1817, reformation_event_boundary__political_swap_reading, base_extractiveness, 1817, 0.45).
narrative_ontology:measurement(refo_be_t1848, reformation_event_boundary__political_swap_reading, base_extractiveness, 1848, 0.5).
narrative_ontology:measurement(refo_be_t1890, reformation_event_boundary__political_swap_reading, base_extractiveness, 1890, 0.58).
narrative_ontology:measurement(refo_be_t1933, reformation_event_boundary__political_swap_reading, base_extractiveness, 1933, 0.66).
narrative_ontology:measurement(refo_be_t1970, reformation_event_boundary__political_swap_reading, base_extractiveness, 1970, 0.64).
narrative_ontology:measurement(refo_be_t2000, reformation_event_boundary__political_swap_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(refo_be_t2026, reformation_event_boundary__political_swap_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1817, reformation_event_boundary__political_swap_reading, suppression_requirement, 1817, 0.35).
narrative_ontology:measurement(refo_su_t1848, reformation_event_boundary__political_swap_reading, suppression_requirement, 1848, 0.4).
narrative_ontology:measurement(refo_su_t1890, reformation_event_boundary__political_swap_reading, suppression_requirement, 1890, 0.52).
narrative_ontology:measurement(refo_su_t1933, reformation_event_boundary__political_swap_reading, suppression_requirement, 1933, 0.66).
narrative_ontology:measurement(refo_su_t1970, reformation_event_boundary__political_swap_reading, suppression_requirement, 1970, 0.62).
narrative_ontology:measurement(refo_su_t2000, reformation_event_boundary__political_swap_reading, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement(refo_su_t2026, reformation_event_boundary__political_swap_reading, suppression_requirement, 2026, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, information_standard).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% One colloquial label — 'the Reformation' — decomposes into three structurally distinct claims (theological climb, political swap, composite overdetermination) with different epsilon values, victim sets, and periodizations, per the epsilon-invariance principle. This story is the swap member; it links both siblings. The climb reading is upstream (closest to actor self-understanding); this reading consumes institutional-fiscal evidence against it; the composite reading arises downstream of both single-driver schemes' accumulated anomalies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
