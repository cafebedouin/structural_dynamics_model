% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Identity Boundary (Habituation Reading)
 *   domain: religious/ecclesial
 *
 * SUMMARY:
 *   This story instantiates the liturgical_habituation_reading of the
 *   nicene_creed_authority kernel: the claim that the creed's operative
 *   function in contemporary Christian practice is identity boundary-marking
 *   through joint liturgical performance — reciting the same fixed text week
 *   after week — and that this function operates independently of whether
 *   reciters cognitively assent to the text's metaphysical content. The
 *   epsilon referent is the standing arrangement under contest: weekly
 *   creedal recitation as actually practiced in creedal churches, assessed by
 *   this reading's own lights. On that referent the reading authors very low
 *   extraction (0.07): the arrangement transfers no material goods, tolerates
 *   private dissent without sanction, and its costs (minutes of
 *   participation, mild social-visibility pressure) are dwarfed by the
 *   belonging it confers on the same people who bear them. Constraint-family
 *   note: the colloquial label 'the creed's authority' decomposes into three
 *   structurally distinct constraints — this file (performative habituation,
 *   epsilon ~0.07, no victims), the strict_orthodox sibling (metaphysical
 *   binding backed by sanction, substantially higher epsilon, victims are
 *   sanctioned dissenters), and the symbolic_confessional sibling (contingent
 *   witness under communal discernment, intermediate epsilon). Each is a
 *   separate story with its own epsilon; they are linked via
 *   network.affects_constraints. The claim/metric independence rule applies:
 *   claimed_type rope is asserted from the structural reading; the metrics
 *   are authored independently as descriptive estimates of the arrangement's
 *   actual operation.
 *
 * KEY AGENTS:
 *   - liturgical_participants: primary beneficiaries (moderate/mobile) — recite weekly, receive membership recognition, bear only participation costs
 *   - denominational_institutions: beneficiaries (institutional/constrained) — inherit the creed as durable self-definition; exit costly even for them
 *   - ecumenical_bodies: beneficiaries (institutional/mobile) — use the shared performed text as a convergence point across separated churches
 *   - liturgical_authorities: agenda_setter (institutional/arbitrage) — decide translation, placement, and frequency; administer without external veto
 *   - nontrinitarian_christians: excluded (organized/constrained) — marked outside the boundary; their objection is unheard inside the liturgy
 *   - liturgical_reform_advocates: excluded (moderate/mobile) — would retire or rewrite the creed; outside the commissions that decide
 *   - doctrine_historians: analytical observer — trace the performance-to-doctrine gap; collect nothing, bear nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.07).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.07).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Identity Boundary (Habituation Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "religious/ecclesial").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, '3cd8e5ed-dab3-47ff-8acd-8ad977630cdc').
narrative_ontology:cs_kernel_codification('3cd8e5ed-dab3-47ff-8acd-8ad977630cdc', fixed_text).
narrative_ontology:cs_authority_grounding('3cd8e5ed-dab3-47ff-8acd-8ad977630cdc', practice).
narrative_ontology:cs_interpretation_layer_present('3cd8e5ed-dab3-47ff-8acd-8ad977630cdc').
narrative_ontology:cs_reading_relation('3cd8e5ed-dab3-47ff-8acd-8ad977630cdc', nicene_creed_authority__strict_orthodox_reading, influences).
narrative_ontology:cs_reading_relation('3cd8e5ed-dab3-47ff-8acd-8ad977630cdc', nicene_creed_authority__symbolic_confessional_reading, coexists_with).
narrative_ontology:cs_axiom('3cd8e5ed-dab3-47ff-8acd-8ad977630cdc', foundational, performative_identity_constitutes_membership).
narrative_ontology:cs_axiom_status(performative_identity_constitutes_membership, holdable).
narrative_ontology:cs_axiom_grounding('3cd8e5ed-dab3-47ff-8acd-8ad977630cdc', performative_identity_constitutes_membership, conventional).
narrative_ontology:cs_axiom('3cd8e5ed-dab3-47ff-8acd-8ad977630cdc', foundational, assent_performance_decoupling).
narrative_ontology:cs_axiom_status(assent_performance_decoupling, holdable).
narrative_ontology:cs_axiom_grounding('3cd8e5ed-dab3-47ff-8acd-8ad977630cdc', assent_performance_decoupling, conventional).
narrative_ontology:cs_reference_frame('3cd8e5ed-dab3-47ff-8acd-8ad977630cdc', shared_performative_identity_marker).
narrative_ontology:cs_drift_state('3cd8e5ed-dab3-47ff-8acd-8ad977630cdc', contemporary_secularized_liturgies, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('3cd8e5ed-dab3-47ff-8acd-8ad977630cdc', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, liturgical_participants).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, denominational_institutions).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, ecumenical_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Attend weekly worship and join the congregation's recitation of the fixed Nicene text. What they receive is membership recognition: the repeated joint performance marks them, and is marked by others, as part of a community spanning centuries and continents. Participation is visible (standing, speaking aloud) but unenforced — a silent attender draws little notice, and leaving a congregation carries social rather than legal cost. Many hold private metaphysical reservations about clauses they nonetheless recite; under the standing arrangement this divergence is tolerated rather than sanctioned.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_participants, beneficiary,
    moderate, biographical, mobile, global).

% Church bodies — dioceses, synods, communions — inherit the creed as a fixed element of authorized liturgy. It hands them a durable self-definition and a continuity claim reaching to the fourth century. Revising or dropping it would fracture internal coalitions and ecumenical relationships, so even the institutions themselves face high exit costs from the arrangement they host.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, denominational_institutions, beneficiary,
    institutional, generational, constrained, global).

% Councils and bilateral dialogue commissions use the shared creedal text as common ground between separated churches. Their work depends on the creed remaining a live, jointly performed reference across jurisdictions; they receive a ready-made convergence point at no material cost to member churches.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, ecumenical_bodies, beneficiary,
    institutional, generational, mobile, global).

% Bishops, synods, and worship commissions decide whether and how the creed appears in authorized services — which translation, which musical settings, weekly or seasonal placement. They administer the arrangement rather than merely collect from it, and they retain practical freedom to adjust its liturgical position without external veto.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_authorities, agenda_setter,
    institutional, generational, arbitrage, continental).

% Unitarians, Oneness Pentecostals, Latter-day Saints, and Jehovah's Witnesses cannot join the recitation without violating their own theology of God. The boundary the performance draws places them outside full recognition; they maintain their own assemblies instead, and their objection — that the line is drawn in the wrong place — is not voiced inside the liturgy itself.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, nontrinitarian_christians, excluded,
    organized, biographical, constrained, global).

% Members inside creedal churches — inclusive-language campaigners, 'no creed but the Bible' evangelicals, Quaker-descended traditions — who would retire, replace, or rewrite the creed. They worship within earshot of the arrangement but sit outside the worship commissions that decide its fate; several such currents founded separate communities historically when reform failed.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_reform_advocates, excluded,
    moderate, biographical, mobile, national).

% Scholars of patristics and liturgical history trace how the creed moved from conciliar definition to weekly performance and document the widening gap between recitation and doctrinal literacy. They describe the structure but collect nothing from it and bear none of its costs.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, doctrine_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__liturgical_habituation_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a trans-local, trans-generational communal identity at near-zero administrative cost: one fixed, memorizable, jointly performed text lets congregations that never meet recognize each other as one body, gives newcomers a legible threshold, and gives dispersed members a shared act. It solves the collective-action problem of identity cohesion without any central registry or enforcement staff.
% TRANSFER_FUNCTION: Moves almost nothing material. Each participant contributes a few minutes of synchronized speech and bodily presence per week; what flows back is recognition and continuity. The only quasi-transfer is attentional: the rite directs communal attention to a shared summary of commitment, and reputational credit for fidelity flows to visibly faithful reciters.
% ABSENT_VOICES: Non-trinitarian Christians stand outside the boundary the performance draws and are not consulted where its liturgical placement is decided; liturgical reform advocates inside creedal churches likewise lack a seat on worship commissions. Both would redraw or retire the boundary; their objections surface in print and parallel assemblies, not in the rooms where the arrangement is administered.
% DISAPPEARANCE_RATIONALE: If the creed vanished from the liturgy overnight, congregational identity practice would rearrange around substitute markers — baptismal formulas, the Lord's Prayer, hymnody, catechetical tests — and denominational self-definition and ecumenical reference points would shift within a generation. Nothing collapses (the substitutes exist and remain workable), but the specific architecture of Christian boundary-marking reorganizes.
% FOUNDING_PROBLEM: The fourth-century Trinitarian controversy: the churches needed a public, testable summary that distinguished Nicene teaching from Arian alternatives, usable in worship and catechesis, so that ordinary believers could recognize and refuse subordinationist teaching.
% FOUNDING_PROBLEM_CORROBORATION: Conciliar records of Nicaea (325) and Constantinople (381) and the polemical letters of Athanasius attest the founding problem from the disputing parties themselves; modern patristics scholarship, sitting outside any benefiting institution, corroborates the anti-Arian origin independently. No corroborating source outside the beneficiary set attests that the original polemical problem remains live — classical Arianism survives only as scholarly reconstruction — while some systematic theologians attest that recurring trinitarian and christological disputes keep boundary-definition problems returning. Hence 'contested': the founding occasion is historically closed, but whether the class of problem it served is closed is disputed.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__liturgical_habituation_reading, 0.07, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very low (0.07) because the arrangement moves no material goods, imposes no sanction for private dissent, and its burdens fall on the same population that receives its benefits. Suppression is minimal (0.08): recitation is socially visible but unenforced, and the enforcement machinery that once existed (established-church compulsion, heresy process) has decayed to near zero — the suppression_requirement series traces that deliberate enforcement-decay trajectory across the interval, which is why it is authored despite the otherwise static picture. Theater_ratio is low (0.12) with a gentle rise: routine-ization (going through the motions) is real and grows as doctrinal literacy declines, but the performance IS the identity-coordination function under this reading, so hollowness-for-some does not convert the practice into proxy activity; the ratio stays far below piton-signaling levels. Accessibility_collapse is low (0.30): once the creed is understood as an identity marker, alternative markers (baptismal formulas, the Lord's Prayer, catechesis, hymnody) remain fully workable, and several communities use them instead. Resistance is modest (0.18): refusal currents (Quaker-descended traditions, creed-skeptical evangelicals, reform advocates) exist and persist but do not threaten the practice. The interval maps integer time points to decade units, 0 = 1925 to 100 = 2025; all tracked metrics share the single grid {0,20,40,60,80,100} so no metric row is backfilled from another's endpoints. Receipt surface: gain_flow is authored as 'diffuse' after checking every named seat — extraction is negligible and no seat captures even that; fixing_cost is omitted because the binary is ill-posed here: the benefit of removal is near-zero (nothing extractive to remove), which dominates any cost-of-removal estimate and would misfile the arrangement into a neglect or piton cell it does not occupy.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the participant and institutional seats the arrangement is a benign habit that confers belonging and continuity at trivial cost — a coordination good. From the excluded seats (non-trinitarian Christians, reform advocates) the same performance is a wall: a boundary that allocates recognition and leaves them outside it, administered in rooms they do not enter. The agenda-setter seat experiences the creed as administrable tradition — adjustable in translation and placement without existential stakes. The analytical seat sees a function that transformed rather than died. The engine computes per-seat classifications from the structural data; the divergence between insider-benign and outsider-excluding experience is the perspectival content this story exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared structural party is a beneficiary, so derived directionality sits near the beneficiary end for all of them and effective extraction is damped toward subsidy — consistent with the near-zero chi this reading expects. No victims are declared because no named actor bears extraction: the excluded agents' cost is non-participation (being outside the recognition the rite distributes), which is a boundary effect, not a transfer extracted from them. Global spatial scope modestly amplifies effective extraction through verification difficulty, but with base epsilon at 0.07 the scaled product remains negligible. Suppression is authored as a raw structural property and is deliberately NOT scaled — the 0.08 reflects residual social-visibility pressure only.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — furnishing a testable anti-Arian boundary summary — is historically resolved; classical Arianism is gone as a live mass movement. A naive mandatrophy read would call the creed a zombie: mandate dead, arrangement persisting. The classification prevents that mislabel in both directions. It is not a piton: theater_ratio is low because the performance is not a hollow shell of a dead function — the function transformed from doctrinal screening to identity habituation, and the successor function is live and genuinely coordinative (scattered congregations do recognize each other through it). It is not a snare: nothing is extracted, no exit is suppressed, and the alternatives competitors propose remain available. The founding_problem_status is authored 'contested' rather than 'dead' precisely because the parties dispute whether the class of boundary-definition problem the creed served is closed; the status-by-verdict combination (contested x world_rearranges) records that dispute rather than asserting either a zombie flag or a clean bill.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_nicene_authority,
    'This story instantiates one reading (liturgical_habituation) of the nicene_creed_authority kernel; is the creed''s operative authority in contemporary practice better described by this reading or by the strict_orthodox sibling (metaphysical binding backed by sanction)?',
    'Comparative congregational ethnography plus sanction-record audit: if deviation from creedal content reliably triggers career, communion, or membership sanction, the strict sibling better describes operative practice and this story''s epsilon is understated.',
    'If the strict reading better describes actual enforcement, the standing arrangement is substantially more extractive than authored here and the sibling constraint file carries the classification load; this file''s rope claim would be functioning as a cover story for the stricter arrangement operating underneath.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_nicene_authority, conceptual, 'Which reading of the creed-authority kernel describes the arrangement actually in force.').

omega_variable(
    assent_independence_testability,
    'Does identity-marking through recitation actually operate independently of cognitive metaphysical assent, as this reading''s core premise holds?',
    'Congregational surveys correlating recitation frequency, clause-level belief profiles, and validated belonging measures; natural experiments where jurisdictions drop or restore the creed from regular liturgy.',
    'If belonging tracks assent strongly, the decoupling premise fails, the reading collapses toward the symbolic_confessional sibling''s territory, and dissenting reciters become coerced performers of convictions they reject — raising epsilon above the 0.10 ceiling this reading authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assent_independence_testability, empirical, 'Whether the performance-to-belonging channel is genuinely assent-independent.').

omega_variable(
    boundary_exclusion_internal_cost,
    'Does the creed''s boundary exclude anyone from WITHIN the arrangement who bears a real cost (which would make the rope mildly tangled), or only outsiders who bear no extraction?',
    'Interviews with non-trinitarian Christians raised inside creedal communities and with reciters carrying sustained private doubt, measuring harm attributable to the performance requirement itself rather than to adjacent doctrines.',
    'Documented internal harm would require a victims declaration, push epsilon past 0.10, and shift the computed type toward tangled_rope; finding only external exclusion keeps the clean-rope classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_exclusion_internal_cost, empirical, 'Whether the identity boundary imposes costs on insiders or only defines an outside.').

omega_variable(
    persistence_without_enforcement,
    'Why does weekly recitation persist as enforcement capacity decays toward zero — genuine ongoing coordination value, or institutional inertia that will eventually shed the practice?',
    'Longitudinal liturgical-practice data across denominations with different enforcement histories; if practice survival is comparable in never-coercive and formerly-coercive bodies, value-based persistence dominates.',
    'Inertia-only persistence predicts drift toward theatrical maintenance of a dead function; value-based persistence stabilizes the coordination classification indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persistence_without_enforcement, empirical, 'Whether the practice persists by value or by inertia as coercion fades.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ncr_lit_hab_tr_t0, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(ncr_lit_hab_tr_t20, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(ncr_lit_hab_tr_t40, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(ncr_lit_hab_tr_t60, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement(ncr_lit_hab_tr_t80, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 80, 0.11).
narrative_ontology:measurement(ncr_lit_hab_tr_t100, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 100, 0.12).

% Extraction over time
narrative_ontology:measurement(ncr_lit_hab_be_t0, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 0, 0.09).
narrative_ontology:measurement(ncr_lit_hab_be_t20, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 20, 0.09).
narrative_ontology:measurement(ncr_lit_hab_be_t40, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement(ncr_lit_hab_be_t60, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 60, 0.08).
narrative_ontology:measurement(ncr_lit_hab_be_t80, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 80, 0.07).
narrative_ontology:measurement(ncr_lit_hab_be_t100, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 100, 0.07).

% Suppression requirement over time
narrative_ontology:measurement(ncr_lit_hab_su_t0, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ncr_lit_hab_su_t20, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(ncr_lit_hab_su_t40, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 40, 0.21).
narrative_ontology:measurement(ncr_lit_hab_su_t60, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 60, 0.16).
narrative_ontology:measurement(ncr_lit_hab_su_t80, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 80, 0.11).
narrative_ontology:measurement(ncr_lit_hab_su_t100, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the authority of the Nicene Creed' decomposes into three structurally distinct constraints per the epsilon-invariance principle: (a) this file — performative identity habituation independent of assent, epsilon ~0.07, no victims; (b) nicene_creed_authority__strict_orthodox_reading — metaphysical binding with sanctioned heresy, substantially higher epsilon, victims are sanctioned dissenters; (c) nicene_creed_authority__symbolic_confessional_reading — historically contingent witness whose authority derives from communal discernment, intermediate epsilon. Each story carries its own epsilon, beneficiaries, and classification; they are linked here as a constraint family. This reading sits upstream of both siblings in one respect: the weekly performed substrate is the social mechanism through which either a binding regime or a discernment regime would operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
