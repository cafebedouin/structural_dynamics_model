% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dignity-Culture Displacement of Honor-Culture Framework
 *   domain: cultural/social/historical
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the dueling disappearance
 *   kernel: the contraction reading asserts that dueling became culturally
 *   unthinkable not through institutional competition or explicit legal
 *   prohibition, but through the progressive displacement of honor-culture
 *   axioms by dignity-culture axioms at the framework level. Under this
 *   reading, the constraint is a mountain — a shift in what counts as a
 *   intelligible category of meaning so complete that honor-culture
 *   practitioners become unable to reproduce their framework in ways that
 *   transmit to younger cohorts or urban centers. The framework displacement
 *   is experienced as natural inevitability by dignity-culture beneficiaries
 *   and as framework-erasure by honor-culture practitioners. The contraction
 *   reading produces a different victim set and different suppression
 *   mechanism than the institutional-displacement reading, and it forecloses
 *   certain features of the overdetermined-composite reading by asserting a
 *   single primary causal pathway (framework displacement) rather than
 *   multiple sufficient conditions.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: bearers of a framework whose categories become illegible (high exit costs, identity-locked)
 *   - dignity_culture_practitioners: agents of framework institutionalization (beneficiaries, arbitrage exit)
 *   - urban_commercial_centers: organizational beneficiaries of dignity-culture coordination at scale
 *   - legal_institutional_apparatus: institutional beneficiary, primary agent of framework codification
 *   - women_in_honor_culture: excluded from the binary framing, bear diffuse costs under both frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.62).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.71).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity-Culture Displacement of Honor-Culture Framework").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "cultural/social/historical").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, 'bfc385af-895a-4705-9d3a-7e47a549336c').
narrative_ontology:cs_kernel_codification('bfc385af-895a-4705-9d3a-7e47a549336c', distributed).
narrative_ontology:cs_authority_grounding('bfc385af-895a-4705-9d3a-7e47a549336c', diffuse_epistemic).
narrative_ontology:cs_reading_relation('bfc385af-895a-4705-9d3a-7e47a549336c', dueling_disappearance_mechanism__institutional_displacement_reading, forecloses).
narrative_ontology:cs_reading_relation('bfc385af-895a-4705-9d3a-7e47a549336c', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('bfc385af-895a-4705-9d3a-7e47a549336c', foundational, dignity_as_intrinsic_irreversible_framework).
narrative_ontology:cs_axiom_status(dignity_as_intrinsic_irreversible_framework, holdable).
narrative_ontology:cs_axiom_grounding('bfc385af-895a-4705-9d3a-7e47a549336c', dignity_as_intrinsic_irreversible_framework, deontological).
narrative_ontology:cs_axiom('bfc385af-895a-4705-9d3a-7e47a549336c', foundational, honor_culture_framework_logically_incoherent_at_institutional_scale).
narrative_ontology:cs_axiom_status(honor_culture_framework_logically_incoherent_at_institutional_scale, holdable).
narrative_ontology:cs_axiom_grounding('bfc385af-895a-4705-9d3a-7e47a549336c', honor_culture_framework_logically_incoherent_at_institutional_scale, instrumental).
narrative_ontology:cs_reference_frame('bfc385af-895a-4705-9d3a-7e47a549336c', honor_culture_axioms_as_intelligible_framework).
narrative_ontology:cs_drift_state('bfc385af-895a-4705-9d3a-7e47a549336c', dignity_culture_institutional_hegemony_achieved, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('bfc385af-895a-4705-9d3a-7e47a549336c', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, urban_commercial_centers).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, legal_institutional_apparatus).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate within a framework where personal reputation and family honor are the primary currencies of social standing. Dueling is the mechanism by which honor violations are remedied and masculinity is demonstrated. As dignity culture spreads, their framework becomes increasingly illegible to younger cohorts and urban centers; their exit consists of abandoning professional identity, kinship obligation, and social intelligibility itself — a choice presented as moral backward-ness rather than framework-loss. They experience the constraint as the progressive erasure of their categories of meaning.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerful, biographical, identity_locked, national).

% Operate within a framework where individual intrinsic worth is inalienable and cannot be damaged by external insult. Reputation-based remedies are unnecessary and violent enforcement of honor is categorically incompatible with dignity. As this framework becomes institutionally entrenched (constitutional protections, legal personhood doctrines, urban professional norms), it progressively displaces honor-culture axioms from legitimacy. They experience the constraint as moral progress and framework-naturalization.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners, beneficiary,
    institutional, generational, arbitrage, national).

% Require predictable, dignity-based social contracts for commerce and credit to function at scale. Honor-culture feuds introduce unpredictable violence and reputation-destruction in relationships structured around long-term debt and market confidence. Dignity-culture frameworks reduce transaction cost and enable institutional credit, banking, and complex commercial networks. They benefit from the framework displacement.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, urban_commercial_centers, beneficiary,
    organized, generational, arbitrage, regional).

% Requires a population that sees personhood and rights as intrinsic and transferable through institutional channels rather than attached to kinship honor. Dignity-culture frameworks legitimize central legal authority over dispute resolution and make dueling unthinkable as a substitute. They are the primary institutional beneficiary of the constraint.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, legal_institutional_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% Bear the costs of honor-culture violence (family feuds, arranged defenses of family reputation) without voice in defining honor or in the duel itself. Dignity culture's displacement of honor-culture frameworks renders women's honor-damage centrality illegible even as it eliminates some forms of violence. They would argue for frameworks that recognize relational dignity without honor-culture machinery, but this position is foreclosed by the binary framing of honor vs. dignity.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, women_in_honor_culture, excluded,
    powerless, biographical, trapped, regional).

% Examines the constraint from outside: how a cultural-cognitive framework (dignity) can become so pervasively institutionalized that an alternative framework (honor) becomes literally unthinkable, not through explicit prohibition alone but through framework-level displacement that makes the alternative's categories of meaning dissolve.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historical_analyst, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dignity-culture axioms coordinate large-scale institutional societies around the premise that personhood and rights are intrinsic and inalienable. This makes centralized legal authority, market exchange, and professional bureaucracy feasible by decoupling reputation from violence and institutional authority from kinship standing.
% TRANSFER_FUNCTION: Transfers the mechanism of social-status enforcement from kinship-based honor remediation (dueling, feud) to institutional channels (law, reputation, professional standing). The transfer is nearly frictionless from the dignity-culture perspective because the axiom is that no violence is needed — reputation and institutional standing suffice.
% ABSENT_VOICES: Honor-culture practitioners whose frameworks become illegible have no institutional seat at the table where dignity-culture axioms are codified into law, professional ethics, and education. They experience the constraint as framework-erasure rather than coordination, but their objections are classified as moral backwardness or refusal to modernize, not as legitimate framework disputes.
% DISAPPEARANCE_RATIONALE: If dignity-culture axioms disappeared overnight, institutional societies would face a cascade of legitimacy crises: legal authority would be unmoored, personhood doctrines would collapse, and honor-based feuding would return as a normal status-enforcement mechanism. The constraint is not sustained by active enforcement but by the fact that dignity culture has become the only intelligible framework for large-scale coordination — the world would not remain 'unchanged' so much as it would become incomprehensible.
% FOUNDING_PROBLEM: Honor-culture coordination fails at scale: feuds propagate, violence is unpredictable, commercial credit requires stable reputational signals that honor-cycles destabilize. Dignity-culture framework solves this by making personhood and rights intrinsic rather than honor-dependent.
% FOUNDING_PROBLEM_CORROBORATION: Historical societies with institutional scales have almost universally shifted toward dignity-culture frameworks; historians and sociologists outside the honor-culture practitioner set attest that institutional stability improved as dignity axioms were institutionalized (see Pitt-Rivers, Wyatt-Brown, Rediker on honor-to-dignity transitions). Honor-culture practitioners contest this reading, arguing instead that institutional displacement happened through coercive legal prohibition and commercial pressure, not through natural framework superiority.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low at t0 (1750, before widespread institutionalization) and rises sharply through 1825 as dignity-culture axioms are codified into law and professional norms, then plateaus by 1875 as the framework becomes nearly universal. The plateau reflects that the constraint's work is largely complete by 1875 — dueling is culturally unthinkable, not newly unthinkable at every moment. Suppression is measured as the active enforcement required to prevent honor-culture practitioners from reverting to dueling-based remedies; it rises steeply as the framework shift accelerates and new legal penalties are enacted (1800-1850) then stabilizes as the alternative framework becomes emotionally intelligible to the population. Theater is low throughout (0.08-0.18) because the constraint's operation does not depend on performative maintenance once institutionalized — dignity-culture practitioners do not need to repeatedly defend the axiom that personhood is intrinsic; it becomes the unexamined background. This low theater signals mountain-hood even as suppression is substantial.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (honor-culture practitioners) and the beneficiary seats (institutional apparatus, commercial centers, dignity-culture practitioners) compute different types from the same structural data. From the honor-culture seat, this is a Snare: the extraction of their entire framework, the illegibility of their categories, the identity-locking that makes exit unthinkable without abandoning kinship and professional standing. From the beneficiary seats, this is a Mountain: an irreversible shift in what intelligible personhood and coordination require. The engine computes this divergence from the directionality and power atoms. The authored metrics reflect the honor-culture seat's experience of substantial suppression (0.71) even though the constraint operates largely through framework-displacement rather than active policing — the suppression is the cost of framework-illegibility itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-culture practitioners sit at d ≈ 1.0 (full targets): they experience the constraint as extractive of their entire framework, their exit options are identity-locked (leaving means abandoning kinship and professional identity), and they bear the cost of framework-displacement. Dignity-culture institutional beneficiaries sit at d ≈ 0.0-0.15 (beneficiaries): they benefit from the constraint's operation, their exit options include arbitrage (they can choose not to institutionalize dignity-culture if it becomes unprofitable, though institutionalization has made this choice nearly impossible), and they bear no cost. Commercial centers sit at d ≈ 0.15-0.30 (moderate beneficiaries): they benefit from dignity-culture stability but bear some cost of the institutional infrastructure required to maintain it. This reading produces higher directionality variance than the institutional-displacement reading, which would give institutional beneficiaries a more moderate d-value because their role in displacing honor-culture is more active and thus more extractive-looking.
 *
 * MANDATROPHY ANALYSIS:
 *   Under the contraction reading, mandatrophy is NOT present at the outset: the founding problem (honor-culture failure to scale) is still live in 1750, and dignity-culture axioms represent a genuine functional improvement in coordination capacity. By 1875, the founding problem is substantially resolved — dueling has ceased, institutional coordination is stable, commercial networks operate without honor-based feuds. The constraint does NOT outlive its function in the same way a snare does; instead, it persists as the unexamined background because dignity-culture axioms have become so fully institutionalized that alternative frameworks are literally unthinkable. This is different from theater-driven piton maintenance: the theater ratio remains low because no one needs to defend the axiom that personhood is intrinsic. The constraint is a natural law at t=1900 in a way it was not at t=1750.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framework_displacement_vs_institutional_causation,
    'Did dignity-culture axioms displace honor-culture axioms through framework-level cognitive displacement, or through institutional substitution that made honor-remediation legally impossible while dignity-culture remained cognitively optional?',
    'Examination of non-institutional populations (rural areas, frontier societies) where legal prohibition was weak: if dignity-culture still remains cognitively unthinkable in these populations despite legal prohibition, framework-displacement is primary; if honor-culture remediation persists where unpunished, institutional substitution was primary.',
    'If framework displacement, the constraint is a mountain (irreversible cognitive shift) with diffuse suppression cost; if institutional substitution, the constraint is a snare (legal coercion + institutional competition) with concentrated enforcement cost. Type classification swings between mountain and snare depending on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_displacement_vs_institutional_causation, empirical, 'Whether dignity-culture displaced honor-culture through framework-level cognition or institutional coercion.').

omega_variable(
    natural_law_vs_beneficiary_construction,
    'Is dignity-culture framework genuinely a natural law emerging from coordination requirements at institutional scale, or a constructed framework whose naturalization benefits specific institutional actors?',
    'Historical comparison with non-Western institutional societies: do all large-scale societies independently arrive at dignity-culture axioms, or do some successful institutional societies maintain honor-culture frameworks? If the latter, dignity-culture is constructed-and-naturalized rather than natural.',
    'If dignity-culture is truly natural-law-like, the constraint is a genuine mountain; if it is constructed but successfully naturalized, the constraint is a false summit that FSM should flag. This omega is mandatory for mountain classification with declared beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_beneficiary_construction, conceptual, 'Whether dignity-culture is a natural law or a naturalized construction benefiting institutional actors.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.71) structural (honor-culture practitioners are legally prevented from dueling) or internalized (they have incorporated dignity-culture axioms and feel dueling is genuinely wrong)?',
    'Post-exit suppression trajectory: if honor-culture practitioners who successfully exit to isolated communities reconstitute dueling-based remedies, suppression is structural; if they maintain dignity-culture axioms even in isolation, suppression is internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure (targets carry the suppression with them); if structural, the measured 0.71 reflects active enforcement requirements. This affects the type computation from mountain (low suppression required) toward snare (high structural suppression required).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression of honor-culture is structural or internalized.').

omega_variable(
    women_excluded_voice_recovery,
    'Could a framework that honors women''s relational dignity without honor-culture machinery have emerged as an alternative to the dignity-vs-honor binary?',
    'Examination of late 18th-century feminist writings and alternative philosophical frameworks: did any tradition propose a synthesis, or was the binary enforced through foreclosure of synthesis options?',
    'If synthesis was possible but foreclosed, the constraint is partially a snare (alternative frameworks were suppressed); if synthesis was cognitively unavailable, the binary was exhaustive. This affects the assessment of excluded women''s voices — are they suppressed alternatives or genuinely absent options?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(women_excluded_voice_recovery, conceptual, 'Whether alternative frameworks honoring women''s dignity while rejecting honor-culture were foreclosed or cognitively unavailable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1750, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement_basis(duel_tr_t1750, projected).
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement_basis(duel_tr_t1800, observed).
narrative_ontology:measurement(duel_tr_t1825, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1825, 0.14).
narrative_ontology:measurement_basis(duel_tr_t1825, observed).
narrative_ontology:measurement(duel_tr_t1850, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1850, 0.16).
narrative_ontology:measurement_basis(duel_tr_t1850, observed).
narrative_ontology:measurement(duel_tr_t1875, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1875, 0.18).
narrative_ontology:measurement_basis(duel_tr_t1875, observed).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement_basis(duel_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1750, 0.18).
narrative_ontology:measurement_basis(duel_be_t1750, projected).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1800, 0.38).
narrative_ontology:measurement_basis(duel_be_t1800, observed).
narrative_ontology:measurement(duel_be_t1825, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1825, 0.52).
narrative_ontology:measurement_basis(duel_be_t1825, observed).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1850, 0.61).
narrative_ontology:measurement_basis(duel_be_t1850, observed).
narrative_ontology:measurement(duel_be_t1875, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1875, 0.62).
narrative_ontology:measurement_basis(duel_be_t1875, observed).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.62).
narrative_ontology:measurement_basis(duel_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1750, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1750, 0.25).
narrative_ontology:measurement_basis(duel_su_t1750, projected).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1800, 0.42).
narrative_ontology:measurement_basis(duel_su_t1800, observed).
narrative_ontology:measurement(duel_su_t1825, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1825, 0.58).
narrative_ontology:measurement_basis(duel_su_t1825, observed).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1850, 0.68).
narrative_ontology:measurement_basis(duel_su_t1850, observed).
narrative_ontology:measurement(duel_su_t1875, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1875, 0.71).
narrative_ontology:measurement_basis(duel_su_t1875, observed).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1900, 0.71).
narrative_ontology:measurement_basis(duel_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__contraction_reading, 0.12).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% The dueling_disappearance_mechanism kernel decomposes into three constraint stories with divergent ε-values and victim sets: (1) contraction_reading treats dignity-culture as a natural-law-like framework displacement, classifying the constraint as mountain with honor-culture practitioners as targets; (2) institutional_displacement_reading treats dueling's decline as institutional competition (courts, banking, law) that made honor-remediation uncompetitive, classifying as snare with identifiable institutional beneficiaries; (3) overdetermined_composite_reading asserts multiple independent sufficient conditions (legal prohibition, institutional modernization, cultural shift, Civil War trauma), treating the decline as multiply caused and classifying as tangled_rope or snare depending on which causal pathway dominates at which moment. Each story has different ε (contraction: 0.62, reflecting framework-level extraction; institutional: higher, reflecting coercive displacement; composite: variable across causes). The three stories are linked via network.affects_constraints to flag the constraint family and enable cross-reading validation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dueling_disappearance_mechanism__contraction_reading, powerful, 0.95).
constraint_indexing:directionality_override(dueling_disappearance_mechanism__contraction_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
