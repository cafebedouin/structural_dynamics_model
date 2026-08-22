% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__progressive_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__progressive_synthesis, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: quran_9_5_scope__progressive_synthesis
 *   human_readable: Verse 9:5 as Superseded Time-Bound Directive (Progressive-Synthesis Reading)
 *   domain: Islamic Jurisprudence / Hermeneutics / Political Theology
 *
 * SUMMARY:
 *   This story authors the progressive_synthesis reading of the contested
 *   quran_9_5_scope kernel: the claim that Q9:5 ('the Sword Verse') was a
 *   time-bound political directive addressed to a specific 7th-century
 *   Medinan conflict, and that the Quran's broader ethical trajectory — read
 *   through maqasid al-shari'a (higher objectives) and historical
 *   contextualization — supersedes any literalist application of the verse as
 *   standing legal command. Under this reading, the verse exits active
 *   constraint space: it binds neither historical polytheists (long dead,
 *   context resolved) nor contemporary non-Muslims (no analogous
 *   treaty-breaking context exists), nor does it license contemporary Muslims
 *   to treat it as an operative legal directive. The constraint being modeled
 *   here is NOT the verse itself but the interpretive-authority arrangement
 *   that results when this reading is adopted: it authorizes
 *   secular-pluralist and reformist frameworks while it delegitimizes
 *   textualist authority structures that continue to invoke the verse's plain
 *   force. ε is authored for that arrangement (the progressive-synthesis
 *   interpretive regime and the authority claims it makes over rival
 *   readings), not for a peaceful world that would result if literalism
 *   vanished. This is one of three sibling constraints in the quran_9_5_scope
 *   family (abrogating_universal, contextual_defensive); per the ε-invariance
 *   principle each reading is authored as its own constraint with its own ε,
 *   beneficiaries, and victims.
 *
 * KEY AGENTS:
 *   - reformist_muslim_scholars: primary agenda-setters of this reading (institutional/analytical, mobile exit) — construct and disseminate the trajectory hermeneutic
 *   - textualist_authority_structures: primary payers under this reading (institutional, identity_locked exit) — their claimed authority over the verse's binding force is delegitimized
 *   - secular_pluralist_legal_frameworks: beneficiaries (institutional, analytical) — gain a religiously-sourced warrant for excluding classical jihad doctrine from contemporary legal relevance
 *   - lay_muslim_believers: bear the interpretive uncertainty (powerless/moderate, constrained exit) — must choose among competing authority claims without independent means to adjudicate them
 *   - traditional_madhab_institutions: secondary payers (institutional, constrained exit) — their pedagogical and juridical continuity is threatened by delegitimization of literalist method itself, not only this one verse's application
 *   - historical_critical_scholars: analytical observers (analytical, arbitrage exit) — apply comparative method without institutional stake in either outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, 0.62).
domain_priors:suppression_score(quran_9_5_scope__progressive_synthesis, 0.58).
domain_priors:theater_ratio(quran_9_5_scope__progressive_synthesis, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, extractiveness, 0.62).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__progressive_synthesis, piton).
narrative_ontology:human_readable(quran_9_5_scope__progressive_synthesis, "Verse 9:5 as Superseded Time-Bound Directive (Progressive-Synthesis Reading)").
narrative_ontology:topic_domain(quran_9_5_scope__progressive_synthesis, "Islamic Jurisprudence / Hermeneutics / Political Theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__progressive_synthesis, 'c9afce86-d056-4e94-b1a8-74b269a33da5').
narrative_ontology:cs_kernel_codification('c9afce86-d056-4e94-b1a8-74b269a33da5', fixed_text).
narrative_ontology:cs_authority_grounding('c9afce86-d056-4e94-b1a8-74b269a33da5', expertise).
narrative_ontology:cs_interpretation_layer_present('c9afce86-d056-4e94-b1a8-74b269a33da5').
narrative_ontology:cs_reading_relation('c9afce86-d056-4e94-b1a8-74b269a33da5', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('c9afce86-d056-4e94-b1a8-74b269a33da5', quran_9_5_scope__contextual_defensive, influences).
narrative_ontology:cs_axiom('c9afce86-d056-4e94-b1a8-74b269a33da5', foundational, revelation_is_historically_situated_not_timeless_code).
narrative_ontology:cs_axiom_status(revelation_is_historically_situated_not_timeless_code, holdable).
narrative_ontology:cs_axiom_grounding('c9afce86-d056-4e94-b1a8-74b269a33da5', revelation_is_historically_situated_not_timeless_code, conventional).
narrative_ontology:cs_axiom('c9afce86-d056-4e94-b1a8-74b269a33da5', foundational, ethical_trajectory_supersedes_literal_directive).
narrative_ontology:cs_axiom_status(ethical_trajectory_supersedes_literal_directive, holdable).
narrative_ontology:cs_axiom_grounding('c9afce86-d056-4e94-b1a8-74b269a33da5', ethical_trajectory_supersedes_literal_directive, instrumental).
narrative_ontology:cs_axiom('c9afce86-d056-4e94-b1a8-74b269a33da5', secondary, classical_naskh_doctrine_binds_contemporary_practice).
narrative_ontology:cs_axiom_status(classical_naskh_doctrine_binds_contemporary_practice, overridden).
narrative_ontology:cs_axiom_grounding('c9afce86-d056-4e94-b1a8-74b269a33da5', classical_naskh_doctrine_binds_contemporary_practice, conventional).
narrative_ontology:cs_reference_frame('c9afce86-d056-4e94-b1a8-74b269a33da5', classical_ijma_literalist_consensus).
narrative_ontology:cs_drift_state('c9afce86-d056-4e94-b1a8-74b269a33da5', post_colonial_reformist_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c9afce86-d056-4e94-b1a8-74b269a33da5', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__progressive_synthesis, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, secular_pluralist_legal_frameworks).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, reformist_muslim_scholars).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, interfaith_coexistence_advocates).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_authority_structures).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, traditional_madhab_institutions).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, literalist_clerics_claiming_ongoing_binding_force).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, lay_muslim_believers).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, lay_muslim_believers).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, quranic_ethical_trajectory_thesis).
narrative_ontology:constraint_vindicates(quran_9_5_scope__progressive_synthesis, historical_contextualization_hermeneutic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and disseminate the trajectory hermeneutic through academic publication, university positions, and interfaith institutions. They construct the interpretive framework that recodes 9:5 as historically bounded and gain scholarly and public-facing legitimacy for doing so. Their exit is mobile: they can move between academic, activist, and clerical registers depending on which platform amplifies the reading.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, reformist_muslim_scholars, agenda_setter,
    institutional, generational, mobile, global).

% Traditional juridical bodies and seminaries (madaris) whose authority rests on continuous transmission (isnad) of classical legal method, including established rulings on naskh and the verse's legal force. This reading directly challenges their interpretive monopoly by treating their method as historically contingent rather than binding. They cannot simply exit the disagreement — their institutional identity IS the claim to custodianship of correct interpretation.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, textualist_authority_structures, payer,
    institutional, civilizational, identity_locked, global).

% Regional schools of jurisprudence that teach and apply classical rulings, including those touching on this verse, as part of a broader legal corpus. A reading that delegitimizes the underlying interpretive method (not just this verse) threatens the perceived validity of their pedagogical output more broadly, though they retain regional authority independent of this specific dispute.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, traditional_madhab_institutions, payer,
    institutional, civilizational, constrained, regional).

% National legal and policy bodies (counter-extremism programs, interfaith commissions, immigration and citizenship frameworks) gain a religiously-sourced warrant for treating classical jihad doctrine as inapplicable to contemporary Muslim citizens, which is useful for social cohesion policy and for countering both extremist recruitment narratives and anti-Muslim political rhetoric.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, secular_pluralist_legal_frameworks, beneficiary,
    institutional, generational, analytical, national).

% NGOs and community organizations building interfaith dialogue programs draw on this reading to present a coherent, textually-grounded case for pluralism, which strengthens fundraising and institutional credibility for coexistence-oriented programming.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, interfaith_coexistence_advocates, beneficiary,
    organized, biographical, mobile, national).

% Ordinary practitioners must navigate competing authoritative claims about what the verse means for their own religious obligations, without independent linguistic or juridical training to adjudicate. They benefit from a less militarized reading of their scripture in public discourse but pay a cost in interpretive uncertainty and, in some communities, social pressure to align with one camp or another.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, lay_muslim_believers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, lay_muslim_believers, beneficiary).

% Groups that invoke the abrogating_universal reading to justify present-day violence are not participants in the scholarly conversation this reading operates within, and would reject its entire epistemic premise (that trajectory hermeneutics has any authority over the plain text). Their absence from the room means this reading's rebuttal of their claims occurs without their direct engagement, in a separate discursive arena entirely.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, militant_islamist_movements, excluded,
    organized, immediate, trapped, regional).

% Apply comparative textual, historical, and linguistic methods to evaluate competing readings of the verse without institutional stake in any single tradition's internal authority structure prevailing.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, historical_critical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__progressive_synthesis, reformist_muslim_scholars).
narrative_ontology:fixing_cost_class(quran_9_5_scope__progressive_synthesis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutic method by which contemporary Muslim communities and outside observers can read a scripturally-embedded war directive without treating it as a standing legal obligation to violence, coordinating around a shared framework for distinguishing time-bound political content from enduring ethical guidance within revelation.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional legitimacy from textualist and traditional madhab custodians of classical legal method toward reformist scholars, secular-pluralist legal frameworks, and interfaith institutions; also shifts public-discourse legitimacy away from readings that treat the verse as a live legal warrant for violence.
% ABSENT_VOICES: Militant Islamist movements that rely on the abrogating_universal reading do not participate in this hermeneutic conversation and would reject its founding premises entirely; likewise, many lay believers whose religious formation occurred entirely within traditionalist institutions are not present as independent voices in the academic and interfaith venues where this reading is developed and adjudicated.
% DISAPPEARANCE_RATIONALE: Textualist authority structures would say the world barely changes if this reading vanished — the verse's classical legal status was never actually altered by academic argument, only its public reception was. Reformist scholars and secular-pluralist institutions would say significant rearrangement follows: counter-extremism policy frameworks, interfaith programming, and progressive Muslim theological education would lose a key textual warrant and have to seek alternative grounds or concede more ground to literalist readings in public discourse.
% FOUNDING_PROBLEM: How should the Quranic text's explicit war-directive language toward polytheists be read in a modern context with no operative analog to 7th-century Arabian tribal treaty politics, especially given both extremist invocation of the verse and Islamophobic citation of it as evidence of inherent religious violence?
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical scholars outside any single confessional tradition corroborate that the verse's original context (Meccan polytheist treaty violations, the specific four-month grace period cited in the surrounding verses) is a live historical-critical research question independent of the theological stakes; however, corroboration that the FOUNDING PROBLEM specifically requires THIS reading's solution (full supersession rather than the contextual_defensive reading's narrower scoping) comes primarily from within the reformist and interfaith-advocacy communities that benefit from the broader reading — no traditionalist or classical-method scholar attests that supersession, rather than contextualization, is the required solution.
narrative_ontology:disappearance_verdict(quran_9_5_scope__progressive_synthesis, contested).
narrative_ontology:founding_problem_status(quran_9_5_scope__progressive_synthesis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__progressive_synthesis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__progressive_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__progressive_synthesis, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__progressive_synthesis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__progressive_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) and theater_ratio (0.44) are authored moderate-high and rising because this reading, while ethically motivated, functions as an interpretive-authority arrangement that reallocates religious legitimacy away from incumbent textualist institutions toward reformist and secular-aligned scholarly networks — a real transfer of authority-capital, not merely a scholarly debate resolved on the merits. Accessibility_collapse is authored LOW (0.35) precisely because this reading does NOT foreclose the interpretive space the way abrogating_universal's literalism does — sibling readings remain fully live and contested, and the trajectory hermeneutic itself is transparent about being one interpretive lens among several. Resistance is authored HIGH (0.75) because the reading meets substantial active pushback from traditional and Salafi-oriented scholarship, which treats trajectory hermeneutics as an illegitimate modernist import. The rising temporal series reflects this reading's growing institutional adoption in Western Islamic studies and progressive Muslim advocacy over the past several decades, with T=160/200 projected as continuation of an observed post-2001 acceleration trend rather than measured fact.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist agenda-setter seat, this reading is liberatory coordination: it releases the tradition from a reading that has been weaponized both by extremist actors claiming Quranic warrant and by Islamophobic actors citing the verse as proof of inherent violence — a genuine ethical and pastoral service. From the textualist payer seat, the identical structure is experienced as an extraction of interpretive authority: centuries of juridical consensus (ijma) on the verse's legal content are being overridden by a hermeneutic method with no comparable chain of transmission, using academic and secular-legal credibility as leverage rather than argument internal to the tradition's own epistemic standards.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars and secular-pluralist frameworks are coded as low-d beneficiaries: they gain legitimacy, institutional standing, and legal-political utility (the reading undercuts justifications for both foreign militant movements and domestic securitization discourses that cite the verse). Textualist authority structures and traditional madhab institutions are coded as high-d targets: their claim to sole custodianship of the verse's legal meaning is directly challenged, and their pedagogical authority is at stake. Lay believers sit closer to symmetric — they benefit from a less martial reading of scripture but bear the cost of not having an independent way to adjudicate between rival scholarly claims, leaving them dependent on whichever authority structure they already trust.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem this reading answers — how to read a scripturally embedded war-directive verse in a permanently altered global political context where classical jihad doctrine has no operative analog — is genuinely live in the sense that Muslims worldwide continue to need an answer. But the specific institutional arrangement (this reading's claim to have definitively superseded the verse's legal force) risks mandatrophy in the other direction: treating a live, contested hermeneutic debate as a settled ethical trajectory forecloses ongoing internal-tradition argument the same way literalism forecloses contextualization. The classification here (piton) reflects that a coordination function that was once a genuine hermeneutic innovation risks calcifying into an institutional credential-generating apparatus (academic positions, interfaith-dialogue funding, NGO legitimacy) that persists on its own momentum somewhat independent of whether it continues to win the argument on its textual merits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint instantiates the progressive_synthesis reading of the quran_9_5_scope kernel — is the verse''s binding force a question that can be settled by historical-critical method, or does the kernel itself (a scripture treated as divine speech by its interpretive community) resist final resolution by any single reading?',
    'No empirical resolution mechanism exists internal to the tradition; the question is adjudicated by which interpretive community''s authority-grounding claim prevails in a given jurisdiction or discourse community, not by evidence that settles it once for all readers.',
    'If the progressive_synthesis reading''s premise (revelation is historically situated and ethically trajectory-bearing, not a fixed timeless legal code) is accepted, the verse exits active constraint space entirely — no party is bound by its directive language. If rejected in favor of a fixed-text literalist premise, this reading itself is delegitimized as heterodox innovation (bid''ah) rather than valid interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether verse 9:5''s status is resolvable by historical-critical method or is irreducibly contested by rival authority-grounding claims within the tradition.').

omega_variable(
    sibling_reading_delta,
    'How would the abrogating_universal and contextual_defensive readings characterize what this reading gets wrong?',
    'Comparative textual and legal-historical analysis across the three readings'' treatment of asbab al-nuzul (occasions of revelation), the classical abrogation (naskh) doctrine, and modern hermeneutic method (maqasid al-shari''a vs. literal textualism).',
    'abrogating_universal would characterize this reading as a modernist evasion of the text''s plain legal force, motivated by contemporary political pressure rather than internal textual logic. contextual_defensive would characterize this reading as overcorrecting — conceding too much ground by removing the verse from legal force entirely rather than properly contextualizing it as defensive/treaty-specific. Both siblings would treat this reading''s beneficiaries (secular-pluralist frameworks) as evidence the reading is externally motivated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_delta, conceptual, 'The disagreement location among the three kernel readings: whether the verse binds today, and if not, whether it is void (this reading) or merely narrowly scoped (contextual_defensive).').

omega_variable(
    traceability_of_ethical_trajectory,
    'Is ''Quranic ethical trajectory'' (the interpretive method this reading depends on) itself a stable, non-arbitrary hermeneutic, or does its content track the political commitments of whoever applies it?',
    'Cross-check whether the trajectory method produces consistent results when applied to verses that cut against progressive-pluralist conclusions (e.g., inheritance, testimony, apostasy verses) rather than only to verses whose supersession is politically convenient.',
    'If the method is applied selectively — invoked to void 9:5 but not applied with equal rigor to other legally binding verses that would similarly be time-bound — the reading is vulnerable to the charge that it is an outcome-driven hermeneutic rather than a principled one, weakening its claim to authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traceability_of_ethical_trajectory, conceptual, 'Whether the trajectory hermeneutic is applied with consistent method or selectively to reach favored conclusions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__progressive_synthesis, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__progressive_synthesis, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t40, quran_9_5_scope__progressive_synthesis, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(qura_tr_t40, observed).
narrative_ontology:measurement(qura_tr_t80, quran_9_5_scope__progressive_synthesis, theater_ratio, 80, 0.27).
narrative_ontology:measurement_basis(qura_tr_t80, observed).
narrative_ontology:measurement(qura_tr_t120, quran_9_5_scope__progressive_synthesis, theater_ratio, 120, 0.34).
narrative_ontology:measurement_basis(qura_tr_t120, observed).
narrative_ontology:measurement(qura_tr_t160, quran_9_5_scope__progressive_synthesis, theater_ratio, 160, 0.4).
narrative_ontology:measurement_basis(qura_tr_t160, projected).
narrative_ontology:measurement(qura_tr_t200, quran_9_5_scope__progressive_synthesis, theater_ratio, 200, 0.44).
narrative_ontology:measurement_basis(qura_tr_t200, projected).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__progressive_synthesis, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t40, quran_9_5_scope__progressive_synthesis, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(qura_be_t40, observed).
narrative_ontology:measurement(qura_be_t80, quran_9_5_scope__progressive_synthesis, base_extractiveness, 80, 0.4).
narrative_ontology:measurement_basis(qura_be_t80, observed).
narrative_ontology:measurement(qura_be_t120, quran_9_5_scope__progressive_synthesis, base_extractiveness, 120, 0.5).
narrative_ontology:measurement_basis(qura_be_t120, observed).
narrative_ontology:measurement(qura_be_t160, quran_9_5_scope__progressive_synthesis, base_extractiveness, 160, 0.58).
narrative_ontology:measurement_basis(qura_be_t160, projected).
narrative_ontology:measurement(qura_be_t200, quran_9_5_scope__progressive_synthesis, base_extractiveness, 200, 0.62).
narrative_ontology:measurement_basis(qura_be_t200, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quran_9_5_scope__progressive_synthesis, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__progressive_synthesis, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__progressive_synthesis, 0.08).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__contextual_defensive).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the quran_9_5_scope kernel (progressive_synthesis, abrogating_universal, contextual_defensive). Each reading is authored as an independent constraint with its own ε per the ε-invariance principle: the same verse, under different interpretive-authority arrangements, produces structurally distinct extraction profiles, beneficiary/victim sets, and classifications. progressive_synthesis (this story) removes the verse from active legal force entirely and is authored as piton (a once-live reformist coordination function increasingly maintained by institutional momentum). abrogating_universal claims the verse as standing universal legal obligation and would be authored with victims among non-Muslim populations and dissenting Muslims — a substantially more extractive and coercive profile. contextual_defensive occupies a narrower middle position, preserving defensive/treaty-based legal force without full supersession. The three form a genealogically connected but structurally distinct family; contamination or legitimacy shifts in one reading's institutional standing propagate pressure onto the others via shared textual and doctrinal terrain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
