% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: Common Article 3 Scope — ICRC Customary-Law Tracking Reading
 *   domain: legal/international_humanitarian
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions sets a bare minimum of
 *   humane treatment for non-international armed conflict but contains no
 *   built-in update path, and treaty amendment requires a universal
 *   diplomatic consensus that has never once been achieved. The ICRC
 *   customary reading resolves scope procedurally: what the minimum requires
 *   in a given conflict is whatever the compiled record of state practice and
 *   expressed conviction has established, maintained institutionally through
 *   the ICRC's Customary IHL study, its public database, and the tribunal and
 *   doctrinal practice that consumes it. Per the epsilon-invariance principle
 *   this file authors ONE reading of the common_article_3_scope kernel as a
 *   clean, single-epsilon constraint: the standing arrangement under
 *   assessment is the customary-tracking mechanism itself, measured by this
 *   reading's own lights. The state-centric and expansive human-rights
 *   readings are different constraints with different epsilon values, victim
 *   sets, and failure modes, authored as sibling files and linked through the
 *   network section. The claim and the metrics are independent authored
 *   facts: the type is claimed as tangled_rope because the structure carries
 *   both a genuine coordination function and identifiable asymmetric burden,
 *   while the metric values describe the mechanism's observed operation
 *   without reference to that claim.
 *
 * KEY AGENTS:
 *   - icrc_institution: Agenda-setting tracker (institutional / identity_locked) — compiles and maintains the practice-and-conviction record; its field-access mandate depends on keeping the role
 *   - states_parties_geneva: Dual-positioned principals (institutional / constrained) — supply most of the record, absorb its outputs, and are spared treaty-amendment costs they could not otherwise pay
 *   - non_state_armed_groups: Primary target (moderate / trapped) — bound by rules whose formation they could not join as equals
 *   - niac_protected_persons: Primary beneficiary (powerless / trapped) — receive whatever floor the record has reached when they need it
 *   - national_military_personnel: Target with offsetting protection (organized / identity_locked) — carry doctrine, training, and personal liability load; protected by the same corpus when hors de combat
 *   - international_criminal_tribunals: Secondary beneficiary (institutional / constrained) — adjudicate scope case-by-case; adjudicable subject matter widens with the record
 *   - human_rights_organizations: Advocacy beneficiary (organized / mobile) — feed the record and harvest each new rule as an advocacy lever
 *   - persistent_objector_states: Excluded dissent (institutional / constrained) — file formal objections that are archived but structurally outvoted
 *   - academic_ihl_scholars: Analytical observer (moderate / analytical) — audit the record's methodology and the authenticity of asserted conviction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.42).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.4).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "Common Article 3 Scope — ICRC Customary-Law Tracking Reading").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "legal/international_humanitarian").

domain_priors:requires_active_enforcement(common_article_3_scope__icrc_customary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, 'f40d0b67-1dfe-428d-a608-43718209985a').
narrative_ontology:cs_kernel_codification('f40d0b67-1dfe-428d-a608-43718209985a', fixed_text).
narrative_ontology:cs_authority_grounding('f40d0b67-1dfe-428d-a608-43718209985a', practice).
narrative_ontology:cs_interpretation_layer_present('f40d0b67-1dfe-428d-a608-43718209985a').
narrative_ontology:cs_reading_relation('f40d0b67-1dfe-428d-a608-43718209985a', common_article_3_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('f40d0b67-1dfe-428d-a608-43718209985a', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_axiom('f40d0b67-1dfe-428d-a608-43718209985a', foundational, scope_tracks_aggregated_state_consent).
narrative_ontology:cs_axiom_status(scope_tracks_aggregated_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('f40d0b67-1dfe-428d-a608-43718209985a', scope_tracks_aggregated_state_consent, conventional).
narrative_ontology:cs_axiom('f40d0b67-1dfe-428d-a608-43718209985a', secondary, compilation_not_legislation).
narrative_ontology:cs_axiom_status(compilation_not_legislation, holdable).
narrative_ontology:cs_axiom_grounding('f40d0b67-1dfe-428d-a608-43718209985a', compilation_not_legislation, conventional).
narrative_ontology:cs_reference_frame('f40d0b67-1dfe-428d-a608-43718209985a', evolving_customary_minimum).
narrative_ontology:cs_drift_state('f40d0b67-1dfe-428d-a608-43718209985a', contemporary_post_compilation_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('f40d0b67-1dfe-428d-a608-43718209985a', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, niac_protected_persons).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, states_parties_geneva).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc_institution).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, international_criminal_tribunals).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, human_rights_organizations).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, national_military_personnel).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, states_parties_geneva).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, national_military_personnel).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, customary_ihl_formation_doctrine).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, martens_clause_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the standing record of how states actually behave and what they say they are doing in non-international armed conflicts: field delegations log the treatment of the wounded, detainees, and civilians; legal advisers compile these entries alongside official statements, military manuals, and tribunal findings into periodic studies and a public database, and conduct confidential dialogue with governments and armed groups about conduct. Its worldwide access to detention places and front lines — the foundation of its entire humanitarian operating model — depends on remaining the neutral keeper of this record, and it publicly insists that it proposes nothing and decides nothing.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc_institution, agenda_setter,
    institutional, generational, identity_locked, global).

% Collectively own the treaty system and supply most of the raw material the record is built from. Each widening of the agreed floor reaches them without a ratification vote; in exchange they are spared amendment conferences that have never achieved consensus. Capable militaries' operations and manuals fill far more of the record than weaker states', so the same mechanism that obliges them also lets the largest armed forces shape what becomes obligatory. Denunciation clauses exist on paper; no government treats them as a live option.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states_parties_geneva, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, states_parties_geneva, payer).

% Fight in the very conflicts the arrangement governs but had no part in forming its rules: their battlefield conduct is logged as evidence of what the law is, while their stated objections carry no comparable weight in what the law requires. Commanders answer for conduct under offense lists that lengthen as the record grows, and captured fighters face prosecution under definitions their organization never accepted. No assembly, court, or conference exists where an armed group could decline the obligations or bargain over their content.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, non_state_armed_groups, payer,
    moderate, biographical, trapped, regional).

% Wounded fighters, detainees, and civilians inside internal conflicts receive whatever floor the accumulated record has reached at the moment they need it. They appear in the system only as entries other actors file — delegation reports, forensic investigations, testimony — and cannot speak, object, or organize in the forums where the floor is argued upward or down.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, niac_protected_persons, beneficiary,
    powerless, biographical, trapped, regional).

% Absorb each widening as revised doctrine, added training hours, tighter targeting rules, and — individually — broader personal exposure to war-crimes charges, since the offense catalog they answer under expands with the record. The same corpus guarantees them humane treatment if wounded or captured, and officers build careers on mastering and applying it faithfully, which makes stepping outside the framework unthinkable within the profession.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, national_military_personnel, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, national_military_personnel, beneficiary).

% Adjudicate whether specific conduct in a given conflict falls inside the agreed floor, and their landmark rulings feed straight back into the record as authoritative precedent. Every widening enlarges the catalog of prosecutable conduct and with it the tribunals' adjudicable subject matter; their own legitimacy rests on the breadth and quality of the record they draw upon.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_criminal_tribunals, beneficiary,
    institutional, generational, constrained, global).

% Document violations, submit material that becomes record entries, and campaign for the floor to rise; each newly recognized rule hands them fresh reporting categories and advocacy levers. If the mechanism stalled they could redirect staff and funding to other legal frames without existential loss.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, human_rights_organizations, beneficiary,
    organized, generational, mobile, global).

% File formal objections whenever a proposed rule of the record contradicts their stated position, maintaining that no obligation binds them without express acceptance. Under the prevailing doctrine their objections shield them only going forward, and the objections are themselves archived as record entries — registered, but outvoted by the volume of contrary practice.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, persistent_objector_states, excluded,
    institutional, generational, constrained, global).

% Audit the record from outside: test whether the conviction behind official statements is genuine or ritual, challenge methodology choices in the compiled studies, and publish the critiques that keep the compilation honest. They collect nothing from the mechanism's operation and depend on it only as a research object.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, academic_ihl_scholars, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__icrc_customary_reading, icrc_institution).
narrative_ontology:fixing_cost_class(common_article_3_scope__icrc_customary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives roughly two hundred states a shared, continuously updated evidentiary standard for which minimum-treatment obligations apply in internal armed conflict, so scope revisions propagate through doctrine and jurisprudence without convening amendment conferences that require universal consent and have never succeeded.
% TRANSFER_FUNCTION: Moves compliance obligation and liability exposure onto belligerent organizations and their personnel; converts that burden into an expanding protection floor delivered to wounded, detained, and civilian persons; and moves interpretive authority and institutional standing toward the compiling institution while moving adjudicable subject matter toward international courts.
% ABSENT_VOICES: Armed-group representatives have never been seated in any forum where the floor is argued; persistent-objector states' filings are archived but structurally outvoted; protected populations enter only through intermediary documentation. The record's near-unanimity partly reflects who is entitled to count as a practice-forming and conviction-forming actor.
% DISAPPEARANCE_RATIONALE: Overnight loss of the tracking arrangement would not erase protections already absorbed into doctrine and jurisprudence, but scope evolution would stall: courts would lose the updated record they cite, military manuals would lose their update channel, contested scope questions would be relitigated from raw treaty text, and the field-access that the tracker's neutrality purchases would need replacement before confidential dialogue could resume at scale.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions regulated internal conflict only through a sparse common minimum with no built-in update path, and treaty amendment requires a universal diplomatic consensus that has never been achievable — so protections for internal armed conflict risked freezing at 1949 levels while the forms of conflict kept evolving.
% FOUNDING_PROBLEM_CORROBORATION: Academic international-law scholarship independent of the ICRC documents the continuing gap between the 1949 text and contemporary conflict forms; national military doctrinal publications treat an update channel as necessary; tribunal judges state in rulings that the treaty text alone under-determines scope. The strongest fully external attestation is the scholarly literature; the doctrinal and judicial attestations come from seats that also draw benefit from the mechanism, which is noted rather than concealed.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).
:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits mid-range (0.42 at interval end) because the mechanism delivers a real protection floor through aggregate consent while imposing each widening on parties who never voted on it — most sharply on armed groups, whose compliance is logged as evidence of what the law is while their dissent carries no weight in what the law requires. Suppression (0.40) is structural rather than physical: there is no forum in which a bound party can decline, and enforcement runs through reputation, confidential dialogue, tribunal citation, and listing rather than force; the series rises gently because the interpretive machinery (dedicated compilation teams, the public database, routine judicial reliance) matured and hardened over the interval. Theater (0.26) is low-moderate: the record contains ritual declarations and ceremonial practice entries, but its outputs demonstrably move doctrine, rules of engagement, and prosecutions. Accessibility collapse (0.58) reflects that the treaty-amendment alternative is effectively blocked by the consensus requirement while case-by-case adjudication and rival readings remain live, so alternatives narrow without vanishing. Resistance (0.50) captures persistent-objector filings, formal state rebuttals to the compiled study, and threshold-state pushback — real friction that has slowed but never broken the mechanism. The temporal series run on one shared grid (points 0, 6, 12, 18, 24, 30) so every tracked metric is authored at every examined time point. Suppression is authored as a raw structural property and is deliberately left unscaled; only extractiveness is scaled by the engine through directionality and scope. Coalition note: armed groups' chronic fragmentation forecloses class action among the trapped, which is precisely why their lack of exit stays cheap for the mechanism to maintain.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the tracker seat should compute differently. From the armed-group seat the mechanism is a net cast without consultation: rules lengthen, liability widens, and no bargaining table exists. From the protected-persons and advocacy seats the same structure is protection arriving faster than treaty politics could ever deliver. From the tracker seat it is a neutral service — documenting, proposing nothing, deciding nothing — even though the tracker's access, standing, and operating model all depend on the role continuing. Among nominally equal seats, states diverge sharply: all hold formal sovereignty, but capable militaries write disproportionate shares of the record while weaker states mostly read it, so identical formal position yields different directionalities. Tribunals experience scope questions as resolvable by citation; the groups they prosecute experience the same questions as settled over their heads.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation and no overrides are needed. niac_protected_persons sit at the full-beneficiary end (d near 0): the mechanism subsidizes them entirely and they are trapped, so effective extraction inverts toward subsidy. non_state_armed_groups sit near the full-target end (d near 0.9): they bear the widening burden with zero exit, and trapping amplifies their effective extraction. national_military_personnel sit target-side but offset (d roughly 0.65): they carry liability and doctrine load, yet the same corpus protects them when wounded or captured, and their identity lock keeps them from arbitraging the framework. states_parties_geneva are listed in both the beneficiary and victim arrays because they genuinely occupy both positions — spared amendment costs, burdened with compliance — yielding a mid-range d near 0.5. icrc_institution sits beneficiary-side (d roughly 0.2): the system pays it in access and standing while it supplies the service below cost, and its identity lock dampens any exit-driven correction. international_criminal_tribunals and human_rights_organizations sit clearly beneficiary-side through jurisdiction and advocacy yield respectively. The structural derivation from these declarations plus exit options reproduces the qualitative relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an update path for internal-conflict protections that treaty politics cannot supply — remains live: conflict forms keep evolving faster than consensus diplomacy moves. The mechanism is therefore not a mandate outliving its function: theater is low, the outputs are consumed, and no sunset clause exists or should. The tangled_rope classification guards against both mislabels: reading the arrangement as pure coordination would hide the unrepresented-party burden that grows with every widening; reading it as pure extraction would erase the protection floor that reaches people no treaty negotiation has ever reached in time. mandatrophy is unresolved in the technical sense — the founding problem is live, the function is exercised, and the arrangement earns its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the Common Article 3 scope kernel governs in a given dispute: this customary-tracking reading, the state-centric threshold reading, or the expansive categorical-floor reading?',
    'Comparative adoption across tribunal judgments, national military manuals, and formal state objections: whichever reading a court or doctrine cites when scope is contested is the operative constraint for that case.',
    'Under the state-centric sibling the protected set shrinks to conflicts clearing intensity and organization thresholds and the tracking machinery loses its function; under the expansive sibling the practice-consent filter disappears and obligations bind immediately, raising the burden on belligerents while raising protection at once. This file''s classification holds only while this reading governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this constraint is one of three live readings of the common_article_3_scope kernel; siblings are separate constraint files.').

omega_variable(
    opinio_juris_authenticity,
    'How much of the conviction record is genuine acceptance by states versus ritual or strategically asserted language?',
    'Corpus analysis separating statements made in application contexts (operational orders, post-incident acknowledgments, court submissions) from rhetorical contexts (commemorative addresses, diplomatic protests); persistent-objector behavior as the falsification signal.',
    'If a large share of the record is performative, the consent foundation of the mechanism weakens, the burden on bound parties rises above what the declarations imply, and the theater measure is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opinio_juris_authenticity, empirical, 'Authenticity of the opinio juris half of the custom formula.').

omega_variable(
    practice_record_weight_bias,
    'Does the compiled record overweight the practice of capable militaries, so that aggregated consent tracks capability rather than agreement?',
    'Source-share quantification of the study database by actor type and capability tier; comparison of adoption curves for rules opposed by weaker states but practiced by stronger ones.',
    'If biased, weaker states and armed groups sit nearer the full-target end than the beneficiary/victim declarations alone imply — the burden concentrates on the unrepresented and the coordination claim thins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_record_weight_bias, empirical, 'Representation bias in whose practice counts toward the record.').

omega_variable(
    tracker_identity_lock_reversibility,
    'If the ICRC''s neutral-tracker identity frame broke — a major power openly recasting its compilations as advocacy — would the tracking function migrate to distributed substitutes or collapse?',
    'Observe successor uptake after any major legitimacy rupture: university compilations, UN inquiry commissions, cross-citations in national manuals.',
    'If the function is portable, the arrangement''s persistence does not depend on the tracker seat and enforcement decentralizes; if not, the tracker''s identity fusion is load-bearing for the whole structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tracker_identity_lock_reversibility, conceptual, 'Whether the tracker''s identity fusion is structural or substitutable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__icrc_customary_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t6, common_article_3_scope__icrc_customary_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement_basis(comm_tr_t6, observed).
narrative_ontology:measurement(comm_tr_t12, common_article_3_scope__icrc_customary_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(comm_tr_t12, observed).
narrative_ontology:measurement(comm_tr_t18, common_article_3_scope__icrc_customary_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(comm_tr_t18, observed).
narrative_ontology:measurement(comm_tr_t24, common_article_3_scope__icrc_customary_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement_basis(comm_tr_t24, observed).
narrative_ontology:measurement(comm_tr_t30, common_article_3_scope__icrc_customary_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(comm_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__icrc_customary_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t6, common_article_3_scope__icrc_customary_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement_basis(comm_be_t6, observed).
narrative_ontology:measurement(comm_be_t12, common_article_3_scope__icrc_customary_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement_basis(comm_be_t12, observed).
narrative_ontology:measurement(comm_be_t18, common_article_3_scope__icrc_customary_reading, base_extractiveness, 18, 0.4).
narrative_ontology:measurement_basis(comm_be_t18, observed).
narrative_ontology:measurement(comm_be_t24, common_article_3_scope__icrc_customary_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement_basis(comm_be_t24, observed).
narrative_ontology:measurement(comm_be_t30, common_article_3_scope__icrc_customary_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(comm_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__icrc_customary_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t6, common_article_3_scope__icrc_customary_reading, suppression_requirement, 6, 0.31).
narrative_ontology:measurement_basis(comm_su_t6, observed).
narrative_ontology:measurement(comm_su_t12, common_article_3_scope__icrc_customary_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement_basis(comm_su_t12, observed).
narrative_ontology:measurement(comm_su_t18, common_article_3_scope__icrc_customary_reading, suppression_requirement, 18, 0.36).
narrative_ontology:measurement_basis(comm_su_t18, observed).
narrative_ontology:measurement(comm_su_t24, common_article_3_scope__icrc_customary_reading, suppression_requirement, 24, 0.38).
narrative_ontology:measurement_basis(comm_su_t24, observed).
narrative_ontology:measurement(comm_su_t30, common_article_3_scope__icrc_customary_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement_basis(comm_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, information_standard).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the scope of Common Article 3' conflates three structurally distinct claims about how scope is fixed, and measuring the label one way yields low extraction while measuring it another yields high — the epsilon-invariance signal to decompose. This file authors only the ICRC customary reading: scope as the output of an institutionalized practice-and-opinio-juris tracking procedure, with epsilon assessed on that standing arrangement by the reading's own lights. The state-centric reading (fixed intensity and organization thresholds) and the expansive human-rights reading (categorical floor for any organized armed violence) instantiate different constraints with different epsilon, victim sets, and failure modes, and are authored as sibling files linked here. The readings compete over the same kernel text; they are not one constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
