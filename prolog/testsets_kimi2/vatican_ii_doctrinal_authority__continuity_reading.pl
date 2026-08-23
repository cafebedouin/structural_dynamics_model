% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Vatican II Hermeneutic of Continuity (Organic Development Reading)
 *   domain: ecclesiological/institutional/hermeneutic
 *
 * SUMMARY:
 *   This constraint is the continuity_reading of the
 *   vatican_ii_doctrinal_authority kernel. It instantiates the hermeneutic
 *   that Vatican II represents organic development within unchanging
 *   tradition, where apparent novelties are explications of implicit prior
 *   teaching and ambiguities are prudential adaptations. Sibling readings
 *   include rupture_progressive_reading (the council authorizes ongoing
 *   reform beyond textual limits) and rupture_traditionalist_reading (the
 *   council contains ambiguities and errors enabling heterodoxy). The
 *   continuity reading treats post-conciliar excesses as implementation
 *   errors rather than conciliar intent. As a kernel reading, it is authored
 *   clean: the contest between readings is routed to omega variables and
 *   network links, not folded into this constraint's epsilon.
 *
 * KEY AGENTS:
 *   - magisterial_hierarchy: Primary agenda-setter (institutional/analytical) â controls interpretive authority and enforces the continuity frame
 *   - traditionalist_dissenters: Primary payer (organized/constrained) â bears the cost of suppressed liturgical and doctrinal dissent
 *   - progressive_reformers: Secondary payer (organized/constrained) â bears the cost of suppressed progressive development claims
 *   - diocesan_clergy: Dual-positioned agent (moderate/constrained) â implements the continuity framework while absorbing parish-level friction
 *   - conciliar_historians: Analytical observer (moderate/analytical) â provides external analytical perspective on reception history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.63).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.7).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Vatican II Hermeneutic of Continuity (Organic Development Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiological/institutional/hermeneutic").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, 'c70ffe8d-56dd-47d3-a5dc-a208063ca659').
narrative_ontology:cs_kernel_codification('c70ffe8d-56dd-47d3-a5dc-a208063ca659', fixed_text).
narrative_ontology:cs_authority_grounding('c70ffe8d-56dd-47d3-a5dc-a208063ca659', lineage).
narrative_ontology:cs_interpretation_layer_present('c70ffe8d-56dd-47d3-a5dc-a208063ca659').
narrative_ontology:cs_reading_relation('c70ffe8d-56dd-47d3-a5dc-a208063ca659', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('c70ffe8d-56dd-47d3-a5dc-a208063ca659', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_axiom('c70ffe8d-56dd-47d3-a5dc-a208063ca659', foundational, doctrine_develops_organically_without_contradiction).
narrative_ontology:cs_axiom_status(doctrine_develops_organically_without_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('c70ffe8d-56dd-47d3-a5dc-a208063ca659', doctrine_develops_organically_without_contradiction, theological).
narrative_ontology:cs_axiom('c70ffe8d-56dd-47d3-a5dc-a208063ca659', foundational, magisterium_as_authentic_interpreter_of_continuity).
narrative_ontology:cs_axiom_status(magisterium_as_authentic_interpreter_of_continuity, holdable).
narrative_ontology:cs_axiom_grounding('c70ffe8d-56dd-47d3-a5dc-a208063ca659', magisterium_as_authentic_interpreter_of_continuity, theological).
narrative_ontology:cs_reference_frame('c70ffe8d-56dd-47d3-a5dc-a208063ca659', pre_conciliar_magisterial_continuity).
narrative_ontology:cs_drift_state('c70ffe8d-56dd-47d3-a5dc-a208063ca659', post_conciliar_reception_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c70ffe8d-56dd-47d3-a5dc-a208063ca659', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, magisterial_hierarchy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, diocesan_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_dissenters).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, progressive_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, diocesan_clergy).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, doctrine_of_organic_development).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, magisterial_hermeneutic_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets conciliar documents through the hermeneutic of continuity; issues corrections of rupture readings; controls the canonical and catechetical apparatus that enforces this framing. Receives institutional authority and unified identity in exchange for managing reform without formal doctrinal contradiction.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, magisterial_hierarchy, agenda_setter,
    institutional, generational, analytical, global).

% Reject post-conciliar liturgical novelties and certain doctrinal formulations as ruptures with prior tradition. Bear the cost of canonical irregularity, marginalization, and the suppression of their reading of tradition. Many are identity-locked by sacramental theology and cannot exit without spiritual cost.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_dissenters, payer,
    organized, biographical, constrained, global).

% Advance doctrinal and pastoral developments beyond conciliar texts, justified by a perceived spirit of reform. Are told they exceed the council's intent; their proposals are disciplined as heterodox and their institutional advancement is blocked.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_reformers, payer,
    organized, biographical, constrained, global).

% Receive the continuity hermeneutic as a pastoral framework that justifies liturgical and prudential adaptation without claiming doctrinal rupture. Simultaneously bear the operational cost of implementing changes, managing parish-level dissent, and policing liturgical boundaries on behalf of the hierarchy.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, diocesan_clergy, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, diocesan_clergy, payer).

% Analyze conciliar texts, theological debates, and reception history. Provide external analytical perspective on whether the continuity reading is borne out by the historical record, though their academic discourse is itself constrained by the magisterial hermeneutic.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, conciliar_historians, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__continuity_reading, magisterial_hierarchy).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified interpretive framework that reconciles apparent post-conciliar novelties with prior magisterial teaching, enabling institutional reform without formal doctrinal contradiction and preventing schism during rapid change.
% TRANSFER_FUNCTION: Moves interpretive authority from local, dissenting, or historical-critical readings to the magisterial hierarchy; moves the cost of liturgical and pastoral adaptation from the hierarchy to clergy and laity who must absorb changes while professing continuity.
% ABSENT_VOICES: Traditionalist dissenters and progressive reformers are formally inside the church but their readings are structurally excluded from legitimacy; secular historians and non-Catholic theologians who might frame the council as political or sociological compromise are outside the authoritative conversation entirely.
% DISAPPEARANCE_RATIONALE: If the continuity hermeneutic vanished, the magisterium's primary mechanism for legitimating post-conciliar praxis would collapse. Progressive and traditionalist rupture readings would compete openly for institutional dominance, the delicate ecclesial balance holding diverse factions together would unravel, and the authority structure for interpreting Vatican II would enter acute crisis.
% FOUNDING_PROBLEM: The need to reform Catholic liturgy, ecumenical posture, and pastoral practice in the mid-20th century without endorsing the modernism condemned by Pius X and without formally repudiating prior magisterial teaching on church-state relations, extra ecclesiam nulla salus, and liturgical uniformity.
% FOUNDING_PROBLEM_CORROBORATION: The magisterium and conciliar theologians (inside the beneficiary set) attest to the problem and its framing. External corroboration is weak: secular historians acknowledge mid-century institutional pressures but do not corroborate the theological framing; non-Catholic observers welcomed specific changes but do not attest to the continuity hermeneutic. State plainly that corroboration from outside the benefiting parties is thin.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__continuity_reading, 0.63, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.63) reflects that the continuity hermeneutic coordinates genuine institutional unity but asymmetrically extracts from dissenters by suppressing alternative readings and centralizing interpretive authority. Suppression (0.70) is high because the constraint's persistence depends on actively excluding rupture readings through canonical and catechetical enforcement. Theater ratio (0.50) has risen over the interval because the growing gap between continuity claims and actual praxis requires increasing performative maintenance (reaffirmations of hermeneutic orthodoxy, correction of misreadings). Accessibility collapse (0.60) indicates that while alternative readings exist in the broader culture, they are largely suppressed within the institutional church. Resistance (0.55) captures sustained opposition from both traditionalist and progressive factions.
 *
 * PERSPECTIVAL GAP:
 *   The magisterial hierarchy experiences this constraint as genuine organic development preserving apostolic truth. Traditionalist dissenters experience it as a snare covering doctrinal rupture with continuity rhetoric. Progressive reformers experience it as a rope being withheld, denying the reform they believe the council authorized. The engine computes this divergence from the structural asymmetry in directionality and exit options; the authored claim does not adjudicate the dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterial hierarchy is the structural beneficiary (low directionality: it subsidizes their authority and unity). Traditionalist dissenters and progressive reformers are the structural targets (high directionality: the constraint extracts from them by delegitimizing their readings and blocking their institutional expression). Diocesan clergy sit near symmetric: they benefit from a clear pastoral framework but pay in operational friction and divided congregations. The derivation from beneficiary/victim declarations plus exit options produces this spread without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â managing mid-century reform without modernism â is contested rather than dead, so the scaffold gate does not open. The constraint lacks a sunset clause and has hardened into a permanent hermeneutic rather than a transitional measure. Without active enforcement, rupture readings would proliferate; without genuine coordination value, the institutional fabric would tear. This prevents misclassification as pure snare (the coordination is real) or pure rope (the extraction is enforced and asymmetric).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_doctrinal_epsilon_bifurcation,
    'Does the continuity hermeneutic operate as one constraint across doctrinal and liturgical domains, or do these domains constitute structurally distinct constraints with different extraction profiles?',
    'Decompose into separate kernel readings if empirical analysis shows the hermeneutic''s enforcement mechanisms, beneficiary structures, and suppression patterns are domain-disjoint.',
    'If bifurcated, the current single epsilon obscures a high-extraction liturgical constraint and a low-extraction doctrinal constraint, misclassifying the aggregate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_doctrinal_epsilon_bifurcation, conceptual, 'Domain bifurcation ambiguity in conciliar reception').

omega_variable(
    post_conciliar_excess_attribution,
    'Are post-conciliar crises (liturgical abuse, doctrinal confusion) causally attributable to the conciliar texts and continuity hermeneutic, or to independent implementation failures?',
    'Comparative historical analysis of dioceses and national churches with varying reception speeds and hermeneutic enforcement.',
    'If attributable to the hermeneutic, extraction is higher than authored; if independent, the hermeneutic is a scapegoat and extraction lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_conciliar_excess_attribution, empirical, 'Causal attribution of post-conciliar disorder').

omega_variable(
    suppression_mechanism_nature,
    'Is the suppression of rupture readings structural (canonical penalties, censorship, removal from teaching posts) or internalized (theological guilt, hermeneutic submission, fear of schism)?',
    'Post-exit trajectory analysis of clergy and laity who leave the institutional church: does their adherence to continuity or rupture framings persist after structural coercion is removed?',
    'If internalized, effective suppression exceeds structural measures and the constraint is more deeply embedded; if purely structural, the constraint is more brittle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_nature, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    continuity_reading_logical_status,
    'Does the continuity reading logically foreclose rupture readings within a single commitment framework, or merely coexist with them as competing live options held by different parties?',
    'Formal analysis of the logical relationship between ''organic development without contradiction'' and ''necessary rupture or break with prior teaching''.',
    'If foreclosed, the reading relation should be reclassified, altering coupling analysis and foreclosure computation in the kernel family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_reading_logical_status, conceptual, 'Logical relation to sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 60, 0.5).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(vati_be_t10, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(vati_be_t20, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(vati_be_t30, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(vati_be_t40, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(vati_be_t50, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 60, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(vati_su_t10, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(vati_su_t20, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(vati_su_t30, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(vati_su_t40, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(vati_su_t50, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 60, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, rupture_traditionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vatican_ii_doctrinal_authority kernel. The kernel decomposes into competing hermeneutic readings (continuity, rupture-progressive, rupture-traditionalist) because the conciliar texts underdetermine their own interpretation. Each reading carries distinct epsilon, beneficiary/victim structures, and cs_structure values. The continuity reading influences the legitimacy conditions of its siblings without logically foreclosing them in the public ecclesial discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
