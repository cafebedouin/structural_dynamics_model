% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__expansive_human_rights_reading, []).

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
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: Common Article 3 Expansive Human-Rights Reading: Classification-Invariant Minimum Floor
 *   domain: law/international_humanitarian_law
 *
 * SUMMARY:
 *   The colloquial question 'does Common Article 3 apply?' decomposes into
 *   three structurally distinct constraints corresponding to the three
 *   readings of the kernel common_article_3_scope; this file instantiates the
 *   expansive human-rights reading, under which the article's minimum floor
 *   attaches to any organized armed violence regardless of formal
 *   classification. The epsilon referent is the standing arrangement under
 *   contest — the expansive floor as actually administered through tribunal
 *   jurisprudence, customary-IHL assertion, and monitoring practice —
 *   assessed by this reading's own lights, never the threshold-gated
 *   arrangement the state-centric sibling would substitute. Beneficiary and
 *   victim declarations are structural inputs, not verdicts: detainees,
 *   civilian populations, humanitarian mandates, tribunals, and non-state
 *   armed groups are declared beneficiaries; state security establishments
 *   and military legal advisors are declared victims. The claimed type
 *   (tangled_rope) and the metrics are authored independently — the engine
 *   computes per-seat classifications from the structural data, and any
 *   divergence between the claim and the computed types is the datum the
 *   corpus exists to collect. KEY AGENTS (by structural relationship): -
 *   detained_persons_in_internal_violence: primary beneficiary
 *   (powerless/trapped) — holds protected status, no exit from custody -
 *   civilian_populations_in_internal_violence: primary beneficiary
 *   (powerless/trapped) — protected against indiscriminate harm, effectively
 *   immobile - state_security_establishments: primary target
 *   (powerful/constrained) — bears compliance burden, monitoring exposure,
 *   prosecution risk - state_military_legal_advisors: secondary target
 *   (moderate/identity_locked) — advisory approvals become future prosecution
 *   exposure - international_war_crimes_tribunals: agenda-setter and
 *   beneficiary (institutional/identity_locked) — authors the reading through
 *   case law, collects jurisdiction - un_human_rights_monitoring_bodies:
 *   agenda-setter (institutional/identity_locked) — generates the soft-law
 *   record carrying the reading outward - icrc_and_humanitarian_mandates:
 *   beneficiary (organized/constrained) — access anchored in the floor,
 *   permission-dependent - nonstate_armed_groups: dual-positioned
 *   beneficiary/payer (organized/trapped) — reciprocity gains, equally bound
 *   - communities_below_classification_threshold: excluded voice
 *   (powerless/trapped) — would claim the floor, holds no seat -
 *   publicists_and_legal_scholars: analytical observer
 *   (analytical/analytical) — maps practice, collects nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.55).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.55).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Common Article 3 Expansive Human-Rights Reading: Classification-Invariant Minimum Floor").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "law/international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, '97839a9a-1167-403f-ac06-90252ae59da1').
narrative_ontology:cs_kernel_codification('97839a9a-1167-403f-ac06-90252ae59da1', fixed_text).
narrative_ontology:cs_authority_grounding('97839a9a-1167-403f-ac06-90252ae59da1', lineage).
narrative_ontology:cs_interpretation_layer_present('97839a9a-1167-403f-ac06-90252ae59da1').
narrative_ontology:cs_reading_relation('97839a9a-1167-403f-ac06-90252ae59da1', common_article_3_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('97839a9a-1167-403f-ac06-90252ae59da1', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('97839a9a-1167-403f-ac06-90252ae59da1', foundational, humane_floor_independent_of_classification).
narrative_ontology:cs_axiom_status(humane_floor_independent_of_classification, holdable).
narrative_ontology:cs_axiom_grounding('97839a9a-1167-403f-ac06-90252ae59da1', humane_floor_independent_of_classification, deontological).
narrative_ontology:cs_axiom('97839a9a-1167-403f-ac06-90252ae59da1', foundational, victim_status_grounds_applicability).
narrative_ontology:cs_axiom_status(victim_status_grounds_applicability, holdable).
narrative_ontology:cs_axiom_grounding('97839a9a-1167-403f-ac06-90252ae59da1', victim_status_grounds_applicability, deontological).
narrative_ontology:cs_reference_frame('97839a9a-1167-403f-ac06-90252ae59da1', classification_invariant_humane_floor).
narrative_ontology:cs_drift_state('97839a9a-1167-403f-ac06-90252ae59da1', contemporary_multipolar_enforcement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('97839a9a-1167-403f-ac06-90252ae59da1', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, detained_persons_in_internal_violence).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, civilian_populations_in_internal_violence).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, icrc_and_humanitarian_mandates).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, international_war_crimes_tribunals).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, nonstate_armed_groups).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_security_establishments).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_military_legal_advisors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, nonstate_armed_groups).
narrative_ontology:constraint_vindicates(common_article_3_scope__expansive_human_rights_reading, martens_clause_primacy).
narrative_ontology:constraint_vindicates(common_article_3_scope__expansive_human_rights_reading, effective_control_organization_test).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons held in custody by state forces or armed groups during internal organized violence — suspected insurgents, political detainees, captured fighters. The reading guarantees them minimum treatment, humane conditions, and judicial-process guarantees no matter how the surrounding violence is classified. They exit nothing: custody is total, and their protection depends entirely on external standards binding their captors.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, detained_persons_in_internal_violence, beneficiary,
    powerless, biographical, trapped, global).

% Communities living inside zones of internal organized violence. They receive the floor's protections against indiscriminate harm and are the population from whom humanitarian access is negotiated. Leaving the zone is often impossible or lethal; their protection arrives through standards applied to the forces operating around them.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, civilian_populations_in_internal_violence, beneficiary,
    powerless, biographical, trapped, regional).

% The ICRC and agencies holding humanitarian mandates negotiate access to detainees and conflict zones. The reading gives their access requests a legal anchor — a right of initiative grounded in the floor rather than in each party's momentary consent. Their work continues only where parties admit them, so their leverage is real but permission-dependent.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, icrc_and_humanitarian_mandates, beneficiary,
    organized, generational, constrained, global).

% Ad hoc tribunals and the permanent international court adjudicate conduct in internal conflicts. Their case law authored the expansive scope, and each affirmation enlarges their docket and doctrinal authority. Their existence and legitimacy are bound up with the reading they interpret; they do not stand outside it.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_war_crimes_tribunals, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, international_war_crimes_tribunals, beneficiary).

% Treaty bodies, commissions of inquiry, and special procedures that investigate state security operations and publish findings invoking the floor. They generate the soft-law record that carries the reading into new situations. Their mandate and standing depend on continued interpretive reach; they have no function outside it.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, un_human_rights_monitoring_bodies, agenda_setter,
    institutional, generational, identity_locked, global).

% Armed forces, interior ministries, and intelligence services conducting counterinsurgency and internal security operations. The reading subjects their detention practices, interrogation methods, and targeting to external standards, monitoring, and potential prosecution. They cannot exit the obligations short of denying that the violence is armed conflict at all — a denial that itself invites scrutiny. Offsetting this, the same floor protects their own personnel when captured or hors de combat.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_security_establishments, payer,
    powerful, generational, constrained, global).

% Uniformed and civilian lawyers advising state operations on the law's application. Each expansion of scope raises the stakes of their advice: operational approvals they sign can become prosecution exhibits years later. Their careers and professional identity are built inside the advisory framework the reading defines; leaving the framework would end the role, not escape it.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_military_legal_advisors, payer,
    moderate, biographical, identity_locked, national).

% Insurgent and rebel organizations party to internal conflicts. The reading grants their detainees and wounded the same floor and gives their claims reciprocity weight, but it equally binds them: commanders face prosecution under the same standards, and the obligations arrive without any consent process they took part in.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, nonstate_armed_groups, beneficiary,
    organized, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, nonstate_armed_groups, payer).

% Populations subjected to state violence that governments classify as policing, emergency, or public-order operations below any recognized threshold. Under this reading they would claim the floor's protections; under threshold readings they fall outside it. They hold no seat in the classification debates — their situation is argued about by others.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, communities_below_classification_threshold, excluded,
    powerless, biographical, trapped, regional).

% Academic commentators, military-manual authors, and professional associations that map state practice and argue the reading's boundaries. They collect no rents and bear no burdens; their analyses supply the practice record that both the expansive and the customary readings cite.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, publicists_and_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__expansive_human_rights_reading, diffuse).
narrative_ontology:fixing_cost_class(common_article_3_scope__expansive_human_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the vacuum problem: internal organized violence otherwise leaves detainees, the wounded, and civilians governed by no binding common standard, since each party's obligations would depend on recognition and reciprocity that internal conflicts lack. The floor sets one minimum standard all parties owe simultaneously, giving each side assurance that its own captured and wounded personnel are covered.
% TRANSFER_FUNCTION: Transfers protected status to detainees, the wounded, and civilians in internal violence, and transfers accountability upward and outward: operational discretion over detention and interrogation moves from state security commands to externally defined standards, and adjudication of violations moves partly from domestic systems to international tribunals and monitoring bodies. Prosecution risk and monitoring exposure flow to state security establishments and their advisors; access entitlements flow to humanitarian mandates.
% ABSENT_VOICES: Communities below any classification threshold would claim the floor but have no seat in the debate over where the threshold sits; non-state detaining authorities rarely participate in the interpretive processes that define their obligations; and rank-and-file soldiers whose conduct becomes case law typically learn of the standards binding them only after adjudication.
% DISAPPEARANCE_RATIONALE: If the expansive floor vanished overnight, detention and interrogation practice in internal violence would reorganize around domestic law and momentary reciprocity; humanitarian access negotiations would lose their legal anchor and revert to pure consent bargaining; tribunal dockets covering internal conflicts would close; and the practice record feeding customary-IHL claims would thin out within a decade.
% FOUNDING_PROBLEM: In 1949, internal armed conflicts sat in a legal vacuum: the existing law of armed conflict applied only between states, and the Spanish Civil War and occupied-Europe resistance warfare had shown that detainees and civilians in civil wars could be tortured and executed with no binding minimum standard. Common Article 3 was written to close that vacuum with a floor all parties owed regardless of victory or recognition.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: state military doctrinal manuals — including those of states that formally contest the expansive scope — affirm the core floor and the vacuum it addressed; tribunal preambles and Security Council resolutions recite the same founding rationale; independent historical scholarship documents the pre-1949 vacuum. No comparable outside source attests that the founding problem requires extension to every organized-violence situation below traditional thresholds — the core problem is corroborated; the expansion's necessity is not.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__expansive_human_rights_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__expansive_human_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__expansive_human_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.55: the reading imposes real and growing burdens on state security operations — interrogation limits, detention registration duties, monitoring exposure, prosecution risk — but a large share of that burden is the coordination cost of the floor itself; one cannot guarantee detainee protection without constraining detainers, so net excess extraction is moderate rather than high. Suppression 0.55: persistence rests on active enforcement machinery (ad hoc tribunals, the ICC, universal-jurisdiction cases, commissions of inquiry, ICRC pressure) rather than voluntary assent — states cannot exit customary-IHL assertions short of persistent objection that the treaty's near-universal ratification undercuts — but enforcement is patchy and selective, so not higher. Theater ratio 0.30: the core function (standards articulation, access negotiation, some prosecutions) is real; a growing minority of activity is professed adherence without compliance — ratification rhetoric, reservations gamesmanship, judgments without arrest capability. Accessibility collapse 0.35: alternatives remain fully live — the threshold-gated state-centric reading and domestic-law frames persist as operative positions, and the kernel contest itself defines the field, so understanding this reading does not eliminate its rivals. Resistance 0.65: sustained and explicit — major military powers formally reject application below intensity thresholds or to counterterrorism operations, scholarly opposition is organized, and jurisdiction withdrawals demonstrate active pushback. Suppression is authored as a raw structural property and is not scaled by power or scope; extractiveness is the quantity the engine scales by directionality and scope. The temporal series run on one shared eight-point grid (t=0 corresponds to 1949, t=77 to 2026, approximately one unit per year) so every metric is authored at every examined time point; trajectories show extraction accumulation and enforcement build-up as a monotonic ratchet, with no oscillation requiring cycle modeling.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the detainee and civilian seats the arrangement is experienced as near-pure coordination — a protective floor arriving from outside, costing them nothing, with no alternative available. From the state security seat the same structure is experienced as imposed extraction — obligations arrived at without a consent process the seat recognizes, enforced by institutions the seat did not staff, with prosecution as the sanction. From the tribunal and monitoring seats it is experienced as mandate coordination — each affirmation of the reading enlarges their docket and doctrinal authority. The engine computes this divergence from power, exit, and directional position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainee and civilian seats derive directionality near the beneficiary pole: they bear no extraction and receive the floor's entire protective output; their trapped exit deepens dependence but does not move them toward the target end. Tribunal and monitoring seats derive low-to-moderate d as beneficiaries with agenda-setting power — they collect mandate and jurisdiction, not rents from the governed. The state security seat derives near-full-target d from its victim declaration combined with constrained exit; the authored override (powerful -> 0.80) corrects for the reciprocity offset the derivation cannot see — the same floor protects captured state personnel and imposes delegitimization costs on adversaries who abuse detainees. Non-state armed groups are the residual ambiguity: declared beneficiaries, yet bound and prosecutable under the same floor they invoke. The derivation would read their beneficiary declaration as low d, understating their burden; because overrides key on the power atom and would misapply to the ICRC seat sitting at the same atom, this ambiguity routes to the reciprocity and threshold omegas rather than to an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the legal vacuum in which internal-conflict detainees and civilians had no binding minimum standard — remains live, so this is not a mandate outliving its function and no mandatrophy resolution is declared. The tangled_rope classification prevents two symmetrical errors: reading the state-seat burden as pure extraction (which would erase the floor's protection function, including for the states' own captured personnel) and reading the humanitarian framing as pure coordination (which would erase the consent gap, the enforcement asymmetry, and the concentrated mandate gains of the interpreting institutions). The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no dead-mandate flag — correctly, since the arrangement's function and its contest are both ongoing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel common_article_3_scope; how would the sibling readings (state_centric_reading, icrc_customary_reading) change the structural classification?',
    'Author the sibling files and compare computed classifications across the family. The state-centric reading shrinks the victim set to threshold-crossing conflicts and removes law-enforcement contexts entirely, materially lowering epsilon and moving the state seat toward symmetry; the ICRC customary reading makes scope a moving function of the practice record.',
    'If the state-centric sibling computes as rope while this reading computes as tangled_rope, the kernel''s classification is reading-relative and cross-reading comparison must be conducted at family level, not per file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Reading-relativity of the kernel''s classification across the common_article_3_scope family.').

omega_variable(
    interpretive_expansion_consent_gap,
    'Did the expansive scope form through genuine customary accumulation (state practice plus opinio juris) or through institution-driven interpretation exceeding state consent?',
    'Systematic replication of the ICRC customary-study methodology with adversarial review of the practice record, expressly counting state objections against claimed consensus rather than discounting them as outliers.',
    'If consent-grounded, the state-seat burden is legitimated coordination cost and the reading stabilizes rope-leaning; if institution-imposed, the reading operates as unconsented extraction on the state seat and the snare gradient strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_expansion_consent_gap, empirical, 'Whether the reading''s expansion tracks state consent or institutional law-making.').

omega_variable(
    organized_violence_threshold_indeterminacy,
    '''Any organized armed violence'' still presupposes some organization threshold in application — where does organized violence end and ordinary crime or spontaneous disorder begin?',
    'Comparative tribunal jurisprudence (the Tadic-line organization and intensity criteria) applied to boundary cases: prison riots, gang warfare, urban insurrection, mass civil unrest.',
    'Sets the victim-set boundary: communities_below_classification_threshold enter or exit the protected class, and both epsilon and the excluded-voice finding shift with where the line is drawn.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organized_violence_threshold_indeterminacy, conceptual, 'Indeterminacy of the organization threshold even under the classification-invariant reading.').

omega_variable(
    enforcement_selectivity_asymmetry,
    'Does enforcement fall selectively on weaker or defeated parties, converting a nominally universal floor into asymmetric liability?',
    'Docket analysis across the ad hoc tribunals and the ICC correlated with party power and conflict outcome; compare prosecution rates for state versus non-state parties and for strong versus weak states.',
    'High selectivity would recast the floor as victor''s extraction and sharpen the state seat''s snare-side experience; roughly uniform enforcement would support the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_asymmetry, empirical, 'Selectivity of enforcement across party power and conflict outcome.').

omega_variable(
    reciprocity_protection_offset,
    'How much of the state seat''s burden is offset by the floor''s protection of state personnel captured or hors de combat in internal conflicts?',
    'Incident-level accounting of state personnel protected under Common Article 3-type obligations versus state compliance costs across recent internal conflicts.',
    'A large offset pushes the state seat''s effective directionality further below the authored override value and strengthens the mutual-benefit component; a negligible offset leaves the state seat near full target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_protection_offset, empirical, 'Magnitude of the reciprocity offset damping the state seat''s extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t11, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 11, 0.09).
narrative_ontology:measurement_basis(comm_tr_t11, observed).
narrative_ontology:measurement(comm_tr_t22, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 22, 0.14).
narrative_ontology:measurement_basis(comm_tr_t22, observed).
narrative_ontology:measurement(comm_tr_t33, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 33, 0.2).
narrative_ontology:measurement_basis(comm_tr_t33, observed).
narrative_ontology:measurement(comm_tr_t44, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 44, 0.26).
narrative_ontology:measurement_basis(comm_tr_t44, observed).
narrative_ontology:measurement(comm_tr_t55, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 55, 0.28).
narrative_ontology:measurement_basis(comm_tr_t55, observed).
narrative_ontology:measurement(comm_tr_t66, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 66, 0.29).
narrative_ontology:measurement_basis(comm_tr_t66, observed).
narrative_ontology:measurement(comm_tr_t77, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 77, 0.3).
narrative_ontology:measurement_basis(comm_tr_t77, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t11, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 11, 0.18).
narrative_ontology:measurement_basis(comm_be_t11, observed).
narrative_ontology:measurement(comm_be_t22, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 22, 0.24).
narrative_ontology:measurement_basis(comm_be_t22, observed).
narrative_ontology:measurement(comm_be_t33, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 33, 0.32).
narrative_ontology:measurement_basis(comm_be_t33, observed).
narrative_ontology:measurement(comm_be_t44, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 44, 0.44).
narrative_ontology:measurement_basis(comm_be_t44, observed).
narrative_ontology:measurement(comm_be_t55, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 55, 0.5).
narrative_ontology:measurement_basis(comm_be_t55, observed).
narrative_ontology:measurement(comm_be_t66, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 66, 0.53).
narrative_ontology:measurement_basis(comm_be_t66, observed).
narrative_ontology:measurement(comm_be_t77, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 77, 0.55).
narrative_ontology:measurement_basis(comm_be_t77, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0, 0.04).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t11, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 11, 0.07).
narrative_ontology:measurement_basis(comm_su_t11, observed).
narrative_ontology:measurement(comm_su_t22, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 22, 0.11).
narrative_ontology:measurement_basis(comm_su_t22, observed).
narrative_ontology:measurement(comm_su_t33, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 33, 0.19).
narrative_ontology:measurement_basis(comm_su_t33, observed).
narrative_ontology:measurement(comm_su_t44, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 44, 0.36).
narrative_ontology:measurement_basis(comm_su_t44, observed).
narrative_ontology:measurement(comm_su_t55, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 55, 0.45).
narrative_ontology:measurement_basis(comm_su_t55, observed).
narrative_ontology:measurement(comm_su_t66, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 66, 0.51).
narrative_ontology:measurement_basis(comm_su_t66, observed).
narrative_ontology:measurement(comm_su_t77, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 77, 0.55).
narrative_ontology:measurement_basis(comm_su_t77, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__icrc_customary_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the label 'Common Article 3's scope' covers three structurally distinct claims with different epsilon values, victim sets, and failure modes. This file is the expansive human-rights reading (broad application, maximal victim set, state seats heavily loaded). The state-centric reading (threshold-gated, law-enforcement excluded) is a separate constraint with lower epsilon and a smaller victim set; the ICRC customary reading (practice-tracked scope) is a third. The expansive reading stands upstream of the customary reading in argumentative practice — its assertions become data in the customary tracker — and stands in logical contradiction to the state-centric reading's gating premise. All three files link one another via network.affects_constraints; cross-reading comparison is family-level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__expansive_human_rights_reading, powerful, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
