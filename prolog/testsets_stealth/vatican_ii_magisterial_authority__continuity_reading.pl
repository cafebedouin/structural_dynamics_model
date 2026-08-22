% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__continuity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vatican_ii_magisterial_authority__continuity_reading
 *   human_readable: Vatican II Continuity Hermeneutic (Continuity Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   The operative constraint is the continuity hermeneutic: the rule,
 *   administered by the Roman magisterium, that the conciliar texts are
 *   authoritative only as read in organic development of the prior
 *   magisterium. Implementation must preserve pre-conciliar doctrine; appeals
 *   to a 'spirit of the council' distinct from the texts carry no standing;
 *   the Latin-preservation mandate of Sacrosanctum Concilium 36 binds; and
 *   apparent conflicts (Dignitatis Humanae with the Syllabus of Errors,
 *   collegiality with Vatican I) are resolved by approved distinctions rather
 *   than admitted as reversals. The rule solves a real coordination problem,
 *   since a teaching body that claimed doctrinal consistency for nineteen
 *   centuries cannot fragment its own interpretive authority without
 *   dissolving that claim, while extracting asymmetrically: it concentrates
 *   certifying power at the center, disciplines both interpretive flanks, and
 *   requires peripheral seats to live inside reconciliations they did not
 *   author. This file instantiates the continuity_reading of the kernel
 *   vatican_ii_magisterial_authority as one clean, epsilon-invariant
 *   constraint; the kernel contest and sibling readings are recorded in
 *   commentary.kernel_context and the omega variables per the committer-frame
 *   rules.
 *
 * KEY AGENTS:
 *   - roman_magisterium: agenda-setter and primary beneficiary (institutional / identity_locked) — the pope and Roman congregations articulate and enforce the hermeneutic; the Church's claim to teach one faith under divine assistance depends on the reading, so the administrator is bound by the rule it administers.
 *   - traditionalist_clergy_and_laity: primary beneficiary (organized / identity_locked) — clergy and laity formed in the pre-conciliar liturgy and doctrinal vocabulary; the hermeneutic declares their inheritance the binding standard rather than a superseded form.
 *   - catholic_faithful: dual beneficiary/payer (powerless / constrained) — receive doctrinal stability and unbroken intergenerational identity; pay by living inside reconciliations they did not author.
 *   - progressive_theologians: primary target (moderate / constrained) — academic and pastoral theologians whose 'spirit of the council' readings carry no magisterial standing and whose reversal-admitting readings are censurable.
 *   - vernacular_liturgical_reformers: target (moderate / constrained) — liturgists and bishops whose vernacular expansion is measured against the binding Latin-preservation mandate of Sacrosanctum Concilium 36.
 *   - diocesan_bishops: dual payer/beneficiary (powerful / constrained) — implement the council under the rule; constrained by it, shielded by it from adjudicating rupture themselves.
 *   - historians_of_doctrine: excluded voice (moderate / mobile) — scholars whose documentation of strained points is admissible only as raw material for reconciliation, never as verdict.
 *   - rupture_traditionalists: excluded voice (organized / identity_locked) — organized bodies holding that the texts themselves break with prior doctrine; their reading is what the hermeneutic rules out, and its enforcement is the structure they live under.
 *   - ecumenical_partners: analytical observer (organized / mobile) — dialogue partners who receive a stable interlocutor whose conciliar concessions are structurally capped by the prior documents the reading preserves.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.62).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.63).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Vatican II Continuity Hermeneutic (Continuity Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, '03efd5e5-e00b-4c5b-b1e5-00e3bb9f625c').
narrative_ontology:cs_kernel_codification('03efd5e5-e00b-4c5b-b1e5-00e3bb9f625c', fixed_text).
narrative_ontology:cs_authority_grounding('03efd5e5-e00b-4c5b-b1e5-00e3bb9f625c', lineage).
narrative_ontology:cs_interpretation_layer_present('03efd5e5-e00b-4c5b-b1e5-00e3bb9f625c').
narrative_ontology:cs_reading_relation('03efd5e5-e00b-4c5b-b1e5-00e3bb9f625c', vatican_ii_magisterial_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('03efd5e5-e00b-4c5b-b1e5-00e3bb9f625c', vatican_ii_magisterial_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('03efd5e5-e00b-4c5b-b1e5-00e3bb9f625c', foundational, magisterium_indefectible_across_councils).
narrative_ontology:cs_axiom_status(magisterium_indefectible_across_councils, holdable).
narrative_ontology:cs_axiom_grounding('03efd5e5-e00b-4c5b-b1e5-00e3bb9f625c', magisterium_indefectible_across_councils, theological).
narrative_ontology:cs_axiom('03efd5e5-e00b-4c5b-b1e5-00e3bb9f625c', foundational, apparent_conciliar_conflicts_reconcilable).
narrative_ontology:cs_axiom_status(apparent_conciliar_conflicts_reconcilable, holdable).
narrative_ontology:cs_axiom_grounding('03efd5e5-e00b-4c5b-b1e5-00e3bb9f625c', apparent_conciliar_conflicts_reconcilable, empirically_contingent).
narrative_ontology:cs_reference_frame('03efd5e5-e00b-4c5b-b1e5-00e3bb9f625c', organic_development_continuum).
narrative_ontology:cs_drift_state('03efd5e5-e00b-4c5b-b1e5-00e3bb9f625c', post_conciliar_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('03efd5e5-e00b-4c5b-b1e5-00e3bb9f625c', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, roman_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, traditionalist_clergy_and_laity).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, catholic_faithful).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, progressive_theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, vernacular_liturgical_reformers).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, historians_of_doctrine).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, rupture_traditionalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, diocesan_bishops).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, catholic_faithful).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, diocesan_bishops).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, hermeneutic_of_reform_in_continuity).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, development_of_doctrine_newman).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, thesis_hypothesis_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The pope and the Roman congregations articulate the continuity hermeneutic and enforce it: they certify which readings of the conciliar texts are authorized, censure readings that admit rupture, and reconcile apparent conflicts (Dignitatis Humanae with the Syllabus of Errors, collegiality with Vatican I) through approved distinctions. The Church's claim to teach one faith under divine assistance across centuries is structurally dependent on this reading; the magisterium cannot admit rupture without falsifying the authority claim by which it teaches at all, so it is bound to the rule it administers.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, roman_magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Clergy, religious, and laypeople formed in the pre-conciliar liturgy and doctrinal vocabulary. The continuity reading declares their inheritance intact and authoritative: their formation is the standard against which implementation is judged, not a superseded form. They bear institutional marginalization in many dioceses, and enforcement waves such as restrictions on the pre-conciliar Mass strike them directly, but the hermeneutic itself validates what they hold.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditionalist_clergy_and_laity, beneficiary,
    organized, generational, identity_locked, global).

% Lay Catholics receive the continuity claim as the frame in which they experience the council: the liturgy changed, the doctrine did not. They benefit from doctrinal stability and an unbroken intergenerational identity; they pay by living inside reconciliations they did not author, holding visible change and proclaimed continuity together. Their individual exit from the Church is costly and identity-laden.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, catholic_faithful, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, catholic_faithful, payer).

% Academic and pastoral theologians whose work follows the council's perceived trajectory. Under the continuity rule their central interpretive moves are unauthorized: appeals to the 'spirit of the council' carry no magisterial standing, and readings that admit reversal are censurable. Their options are recasting their work into reconciliationist form, publishing outside authorization at career and communion risk, or leaving the Church and losing their ecclesial vocation and audience.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, progressive_theologians, payer,
    moderate, biographical, constrained, global).

% Liturgists and bishops who expanded vernacular use beyond the letter of the conciliar mandate. On the strict reading, the Latin-preservation mandate of Sacrosanctum Concilium 36 binds, so the post-conciliar vernacular expansion is measured as implementation exceeding authorization, subject to correction and rollback rather than precedent. Their accomplished work has no settled standing under the rule they live under.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, vernacular_liturgical_reformers, payer,
    moderate, biographical, constrained, global).

% Bishops implement the council in their dioceses under the continuity rule: they may not authorize practices justified by a 'spirit' departing from the strictly read texts, and they answer to Rome for doctrinal discipline. In exchange, the rule shields them from adjudicating rupture themselves: the center certifies the reading, and their obedience is protected from the charge of betraying either the council or tradition.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, diocesan_bishops, payer,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, diocesan_bishops, beneficiary).

% Scholars who document the drafting history and doctrinal development of the conciliar corpus, including the points where continuity is strained. Their findings are admissible in magisterial argument only as raw material for reconciliation, never as verdicts; the authorized conversation is structured so their discipline cannot deliver its natural conclusion inside it. They retain full freedom to publish outside the framework.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, historians_of_doctrine, excluded,
    moderate, biographical, mobile, global).

% Organized traditionalist bodies and communities (the Society of St. Pius X and sedevacantist circles among them) who hold that the conciliar texts themselves break with prior doctrine. Their reading is precisely what the continuity hermeneutic rules out, so they stand outside the authorized conversation by definition; enforcement of the hermeneutic through censures, excommunications, and regularization negotiations is the structure they live under.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, rupture_traditionalists, excluded,
    organized, generational, identity_locked, global).

% Other churches and world religions in dialogue with Rome. The continuity reading fixes what the conciliar ecumenism and interfaith texts can mean: read as development rather than reversal, Nostra Aetate and Unitatis Redintegratio offer partnership bounded by the prior documents the reading preserves. Partners receive a stable interlocutor whose concessions are structurally capped.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, ecumenical_partners, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__continuity_reading, roman_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single authoritative interpretive tradition across generations: the deposit of faith is transmitted as the same faith, each council is read as developing rather than reversing its predecessors, and disputes over the conciliar meaning are adjudicated by one certifying center instead of fragmenting into self-authorizing local readings.
% TRANSFER_FUNCTION: Moves interpretive authority and doctrinal legitimacy upward to the Roman center: readings not certified as continuous lose standing, and certification power concentrates where the rule is administered. It moves validation downward to the traditionalist constituency, whose pre-conciliar inheritance is declared the measure of implementation. It moves costs to unauthorized readers: progressives whose trajectory readings are stripped of standing, liturgical expansion measured against the Latin mandate, historians whose discontinuity findings are inadmissible as verdicts, and rupture-holding bodies living under the enforcement machinery.
% ABSENT_VOICES: Historians of doctrine who document irreducible strain are inside the academy but outside magisterial argument; their findings enter only as raw material for reconciliation. Rupture-holding traditionalists are excluded by definition: their reading is what the hermeneutic exists to rule out. Censured progressive theologians speak, when they are heard, as disciplined parties rather than interpreters. The hermeneutic's authorized articulation is dominated by the seat that benefits most from it, and the unanimity of the official record partly reflects who was in the room.
% DISAPPEARANCE_RATIONALE: If the continuity hermeneutic vanished overnight, if the Church admitted rupture, the magisterium's claim to transmit one unchanging faith under divine assistance would collapse or need re-founding on other grounds; the traditionalist constituency would lose its validation while the progressive constituency gained authorization; every post-conciliar dispute would reopen at the level of first principles; and the Church's self-understanding, its ecumenical posture, and its internal authority structure would all reorganize around whatever replaced the consistency claim.
% FOUNDING_PROBLEM: A council that visibly changed the liturgy, ecumenism, religious liberty, and church governance threatened to falsify the Church's claim that it teaches the same faith it has always taught. The continuity hermeneutic was articulated to hold the conciliar texts and the prior magisterium in one unbroken frame, resolving the crisis of apparent self-reversal before it could dissolve the authority claim that depends on consistency. Its modern formalization is Benedict XVI's 2005 'hermeneutic of reform in continuity' address, with roots in the post-conciliar controversies of the 1960s through 1980s.
% FOUNDING_PROBLEM_CORROBORATION: The problem's liveness is attested from outside the beneficiary set: historians of doctrine, Catholic and secular, document the Dignitatis Humanae and Syllabus tension as a genuine interpretive problem rather than a manufactured one; ecumenical partners note the Church's own difficulty reconciling the conciliar religious-liberty teaching with Mirari Vos and Quanta Cura; and rupture-holding traditionalists, who reject the continuity solution, attest the apparent rupture is real enough to organize a durable schism around. No party in the dispute denies the problem exists; the parties dispute whether the hermeneutic solves it.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__continuity_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end) is reading-indexed: the referent is the continuity hermeneutic as the standing arrangement, assessed by this reading's own lights. The reading holds the rule legitimate and still acknowledges real maintenance costs: the reconciliation labor imposed on theologians, the disciplining of unauthorized readings, and the growing burden of holding the reference frame against accumulated implementation drift. The series rises from 0.28 (1962, pre-operative) to 0.62 as divergence between the texts-read-strictly and actual implementation compounds. Suppression (0.63) is authored as a raw structural property, the enforcement intensity the rule requires (congregational interventions, censures, restrictions on the pre-conciliar Mass), and is deliberately unscaled; only extractiveness is scaled by the engine. Theater_ratio (0.38) tracks the share of continuity work that is procrustean performance rather than achieved reconciliation; it dips in the hermeneutic-of-continuity era around 2005 to 2012, when the reconciliation scholarship was most serious, and rises as divergence accumulates. The series oscillates on pontificate cycles (Paul VI implementation battles, the John Paul II and Ratzinger enforcement peak, Benedictine easing, re-tightening after 2013) rather than drifting monotonically; the oscillation is enforcement-cycling around a fixed claim, not intermittent reinforcement as an extraction mechanism. base_properties are measured at interval end, in the re-tightened phase. Accessibility_collapse (0.55): within the authorized framework alternative readings largely collapse, but they persist at the edges in the academy and traditionalist circles. Resistance (0.60): the rule meets organized resistance from both flanks simultaneously, a profile few constraints exhibit. All three metric series share one time grid (1962 to 2025 at ten points) as the alignment rule requires.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical texts. From the roman_magisterium seat the hermeneutic is the condition of its own authority: enforcement reads as fidelity, and the rule's binding character on the center (it cannot admit rupture without self-falsification) is experienced as obligation rather than cost. From the progressive_theologians seat the same rule operates as censorship of the council's perceived trajectory. From the traditionalist_clergy_and_laity seat it is validation, except when enforcement turns on them (restrictions on the pre-conciliar Mass), when it reads as betrayal of the very continuity it proclaims. The catholic_faithful seat experiences the rule as lived contradiction held together by catechesis. Same-level differentiation: historians_of_doctrine and progressive_theologians hold the same nominal power (moderate), but the historians' mobile exit (publish anywhere) versus the theologians' constrained exit (ecclesial authorization is their vocation) produces different experienced constraint from the same rule. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: roman_magisterium, traditionalist_clergy_and_laity, catholic_faithful. Victim declarations: progressive_theologians, vernacular_liturgical_reformers, historians_of_doctrine, rupture_traditionalists. Two overrides correct derivations the structural data alone would get wrong. First, institutional to d 0.2: the magisterium is the primary beneficiary, but it is identity-locked to the rule it administers and cannot relax the continuity claim without falsifying its own authority, so it bears real constraint costs a pure-beneficiary derivation would miss; it sits nearer symmetric than a subsidy receiver. Second, powerless to d 0.5: the faithful are declared beneficiaries (stability, identity) yet pay real costs (living reconciliations, constrained experience); their position is genuinely symmetric, which a beneficiary-keyed derivation would understate. The remaining seats derive cleanly: victims with constrained exit (theologians, liturgical reformers) sit near the full-target end; the historians' victimhood is damped by mobile exit; rupture_traditionalists, identity-locked victims of the enforcement machinery itself, sit nearest the full-target end; traditionalist beneficiaries, identity-locked into the validated inheritance, sit near the subsidy end. The ecumenical_partners seat is observational and feeds no directionality. Coalition note: the faithful are individually powerless, but their mass assent is the substrate the claim rests on; a class-level shift in lived assent is the one lever that could force renegotiation, which is why the hermeneutic invests heavily in formation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Reading the hermeneutic as pure fidelity (rope) misses the asymmetric extraction: the same structure that coordinates doctrinal unity concentrates certifying power at the center and disciplines both flanks, with identifiable payers, hence tangled_rope rather than rope. Reading it as pure ideological defense (snare) misses the genuine coordination function: a transmission body claiming consistency across centuries faces a real collective-action problem in interpretive stability, and the hermeneutic solves it, so the coordination story is not cover. Mandatrophy: the founding problem (apparent self-reversal threatening the consistency claim) is live, not dead; the hermeneutic's function has not atrophied and it is not a piton. The tracked risk is drift, not atrophy: theater_ratio's rise from 0.10 to 0.38 marks a growing share of continuity work becoming performative reconciliation. If the reconciliations fail on key points (see the reconciliation_success omega) while enforcement persists, the structure would drift toward extraction-without-function; the measurement series exists to catch that transition early.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (continuity_reading) of the kernel vatican_ii_magisterial_authority; would instantiating a sibling reading (rupture_reading or composite_overdetermination_reading) relocate the constraint''s extraction and flip the family''s classification pattern?',
    'Not resolvable within this story: the siblings are separate constraints with their own epsilon, beneficiaries, and victims. Cross-reading comparison requires generating all three files and comparing engine outputs; no in-story data can adjudicate which reading is true of the corpus.',
    'If the rupture reading is structurally correct, this story''s beneficiary/victim assignments are inverted (the texts themselves would be the defect and the enforcement machinery would protect error rather than discipline implementation); if the composite reading is correct, no single hermeneutic binds and this constraint fragments into its ambiguities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Epsilon and structure are reading-indexed; the same corpus instantiates different constraints per reading, and this file is the continuity reading only.').

omega_variable(
    reconciliation_success,
    'Do the specific reconciliations the reading requires (Dignitatis Humanae with the Syllabus via the thesis/hypothesis distinction or development of doctrine; Lumen Gentium collegiality with Vatican I primacy; Sacrosanctum Concilium 36 with post-conciliar vernacular practice) succeed on the texts, or do they require procrustean readings?',
    'Line-by-line hermeneutical scholarship comparing conciliar drafts (relationes, rejected schemas) with the prior magisterial documents, plus the trajectory of magisterial commentary on each contested pair.',
    'If key reconciliations fail, the continuity reading degrades toward the composite reading at those points and the theater_ratio series understates performative maintenance; if they succeed, the reading''s extraction is largely the price of genuine discipline and the rope component is stronger than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconciliation_success, empirical, 'Whether the continuity reading''s reconciliation work achieves continuity or performs it.').

omega_variable(
    enforcement_symmetry,
    'Is the continuity hermeneutic enforced symmetrically against both interpretive flanks (spirit-of-the-council progressives and rupture-holding traditionalists), or asymmetrically in ways that concentrate its costs on one side?',
    'Comparative record of congregational interventions, censures, and disciplinary measures against each flank across the interval: progressive theologians silenced in the 1970s and 1980s versus traditionalist bodies regularized or restricted by pontificate.',
    'Asymmetric enforcement raises effective extraction on the disciplined flank and strengthens the tangled_rope classification over a pure-discipline rope reading; symmetric enforcement would support the reading''s self-description as fidelity rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_symmetry, empirical, 'Whether enforcement of the hermeneutic distributes its costs evenly across the two flanks.').

omega_variable(
    faithful_assent_vs_internalization,
    'Is the catholic_faithful seat''s acceptance of the continuity claim reasoned assent to the reconciliations, or an internalized frame formed by catechesis that pre-empts perceiving discontinuity?',
    'Sociological study of lay doctrinal cognition: whether lay Catholics who learn the drafting history and the Dignitatis Humanae and Syllabus tension revise their continuity judgment.',
    'If internalized, part of the constraint''s stability rests on formation rather than argument, raising its effective suppression and making the continuity claim more brittle under information shock.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(faithful_assent_vs_internalization, empirical, 'Structural versus internalized basis of the faithful seat''s acceptance of the continuity claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vatican_ii_continuity_tr_t1962, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1970, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1978, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1978, 0.26).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1985, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(vatican_ii_continuity_tr_t1992, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1992, 0.3).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2000, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2007, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2007, 0.22).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2013, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2013, 0.27).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2019, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2019, 0.33).
narrative_ontology:measurement(vatican_ii_continuity_tr_t2025, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(vatican_ii_continuity_be_t1962, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1962, 0.28).
narrative_ontology:measurement(vatican_ii_continuity_be_t1970, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement(vatican_ii_continuity_be_t1978, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1978, 0.52).
narrative_ontology:measurement(vatican_ii_continuity_be_t1985, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1985, 0.57).
narrative_ontology:measurement(vatican_ii_continuity_be_t1992, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1992, 0.55).
narrative_ontology:measurement(vatican_ii_continuity_be_t2000, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement(vatican_ii_continuity_be_t2007, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2007, 0.49).
narrative_ontology:measurement(vatican_ii_continuity_be_t2013, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2013, 0.53).
narrative_ontology:measurement(vatican_ii_continuity_be_t2019, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2019, 0.58).
narrative_ontology:measurement(vatican_ii_continuity_be_t2025, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vatican_ii_continuity_su_t1962, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1962, 0.15).
narrative_ontology:measurement(vatican_ii_continuity_su_t1970, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(vatican_ii_continuity_su_t1978, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1978, 0.5).
narrative_ontology:measurement(vatican_ii_continuity_su_t1985, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(vatican_ii_continuity_su_t1992, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1992, 0.5).
narrative_ontology:measurement(vatican_ii_continuity_su_t2000, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(vatican_ii_continuity_su_t2007, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2007, 0.4).
narrative_ontology:measurement(vatican_ii_continuity_su_t2013, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2013, 0.45).
narrative_ontology:measurement(vatican_ii_continuity_su_t2019, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2019, 0.55).
narrative_ontology:measurement(vatican_ii_continuity_su_t2025, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2025, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% vatican_ii_magisterial_authority is a contested kernel: one conciliar corpus, three incompatible readings, three constraint stories. This file instantiates the continuity_reading only; rupture_reading and composite_overdetermination_reading are separate constraints with their own epsilon, beneficiary/victim structure, and classification. The readings differ on where the constraint binds: the continuity reading binds implementation to the texts-read-strictly; the rupture reading locates the defect in the texts themselves; the composite reading dissolves the single-reading assumption. Epsilon differs across the family because each reading locates the extraction differently over the same referent corpus; the family is linked via affects_constraints and via cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__continuity_reading, institutional, 0.2).
constraint_indexing:directionality_override(vatican_ii_magisterial_authority__continuity_reading, powerless, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
