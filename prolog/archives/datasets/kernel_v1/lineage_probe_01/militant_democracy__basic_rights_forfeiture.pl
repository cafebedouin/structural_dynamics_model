% ============================================================================
% CONSTRAINT STORY: militant_democracy__basic_rights_forfeiture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_militant_democracy__basic_rights_forfeiture, []).

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
 *   constraint_id: militant_democracy__basic_rights_forfeiture
 *   human_readable: Article 18 Basic Rights Forfeiture — Militant Democracy's Dormant Threat
 *   domain: constitutional_law/doctrinal
 *
 * SUMMARY:
 *   Article 18 of the German Basic Law (Grundgesetz) declares that anyone who
 *   abuses the freedoms of speech, press, assembly, or association to attack
 *   the free democratic order forfeits those basic rights. This clause
 *   instantiates one reading of a broader militant democracy kernel that
 *   Weimar's collapse imprinted into Bonn's constitutional DNA. The
 *   structural delta is stark: suppression is individual and conditional
 *   (triggered only by specific abuses, not categorical status), dormant in
 *   practice (no successful application in 75 years), and beneficiary is the
 *   constitutional order in theory while the victim set is targeted agitators
 *   in theory. Yet the constraint's extractiveness has declined from initial
 *   promulgation (0.28) to contemporary dormancy (0.18), while its
 *   theater-ratio has risen from 0.65 to 0.82. This inversion — rising
 *   theater as extractiveness declines — is the diagnostic signal of a piton:
 *   the machinery is maintained through institutional inertia and doctrinal
 *   commitment, not through functional necessity or active enforcement. The
 *   unresolved question is whether dormancy is structural (the threat is most
 *   effective when latent) or contingent (application has simply not
 *   occurred, but could).
 *
 * KEY AGENTS:
 *   - Targeted Agitators: Primary victims in doctrine (powerless/trapped) — face legal jeopardy from Article 18 forfeiture but experience doctrine as mostly threat rather than execution
 *   - Constitutional Court (Karlsruhe): Primary institutional beneficiary (institutional/arbitrage) — sole authority to adjudicate forfeiture; maintains the doctrine's legitimacy through non-application and clear threshold-setting
 *   - Constitutional Order: Abstract beneficiary — Article 18 theoretically protects the order's self-preservation capacity
 *   - Organized Opposition Movements: Secondary victims (organized/constrained) — constrained by the threat structure; face suppression risk that varies with political climate
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent doctrinal choice as a logical necessity of liberal order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(militant_democracy__basic_rights_forfeiture, 0.18).
domain_priors:suppression_score(militant_democracy__basic_rights_forfeiture, 0.35).
domain_priors:theater_ratio(militant_democracy__basic_rights_forfeiture, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(militant_democracy__basic_rights_forfeiture, extractiveness, 0.18).
narrative_ontology:constraint_metric(militant_democracy__basic_rights_forfeiture, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(militant_democracy__basic_rights_forfeiture, theater_ratio, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(militant_democracy__basic_rights_forfeiture, piton).
narrative_ontology:human_readable(militant_democracy__basic_rights_forfeiture, "Article 18 Basic Rights Forfeiture — Militant Democracy's Dormant Threat").
narrative_ontology:topic_domain(militant_democracy__basic_rights_forfeiture, "constitutional_law/doctrinal").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(militant_democracy__basic_rights_forfeiture, '88024a72-cb69-4875-a23c-b31cefe0fafc').
narrative_ontology:cs_kernel_codification('88024a72-cb69-4875-a23c-b31cefe0fafc', formalized).
narrative_ontology:cs_authority_grounding('88024a72-cb69-4875-a23c-b31cefe0fafc', lineage).
narrative_ontology:cs_interpretation_layer_present('88024a72-cb69-4875-a23c-b31cefe0fafc').
narrative_ontology:cs_reading_relation('88024a72-cb69-4875-a23c-b31cefe0fafc', militant_democracy__lessons_of_weimar_reading, coexists_with).
narrative_ontology:cs_reading_relation('88024a72-cb69-4875-a23c-b31cefe0fafc', militant_democracy__party_ban_instrument, influences).
narrative_ontology:cs_axiom('88024a72-cb69-4875-a23c-b31cefe0fafc', foundational, individual_rights_forfeiture_conditional).
narrative_ontology:cs_axiom_status(individual_rights_forfeiture_conditional, holdable).
narrative_ontology:cs_axiom_grounding('88024a72-cb69-4875-a23c-b31cefe0fafc', individual_rights_forfeiture_conditional, deontological).
narrative_ontology:cs_axiom('88024a72-cb69-4875-a23c-b31cefe0fafc', secondary, dormancy_as_efficacy).
narrative_ontology:cs_axiom_status(dormancy_as_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('88024a72-cb69-4875-a23c-b31cefe0fafc', dormancy_as_efficacy, instrumental).
narrative_ontology:cs_reference_frame('88024a72-cb69-4875-a23c-b31cefe0fafc', rights_conditional_on_non_abuse).
narrative_ontology:cs_drift_state('88024a72-cb69-4875-a23c-b31cefe0fafc', contemporary_2020s, gap(stable, minor, true)).
narrative_ontology:cs_created_at('88024a72-cb69-4875-a23c-b31cefe0fafc', '').
narrative_ontology:cs_kernel_id(militant_democracy__basic_rights_forfeiture, militant_democracy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(militant_democracy__basic_rights_forfeiture, constitutional_order).
narrative_ontology:constraint_victim(militant_democracy__basic_rights_forfeiture, targeted_agitators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED AGITATOR (PITON) — Subject to Article 18 threat but experiences it as primarily performative. The clause's non-application across 75 years is the operative fact: suppression is latent rather than active. The agitator faces real legal jeopardy in theory, but the historical record shows the doctrine is maintained through threat rather than execution. Theater-ratio dominates — the machinery is preserved but dormant.
constraint_indexing:constraint_classification(militant_democracy__basic_rights_forfeiture, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTITUTIONAL COURT (ROPE) — Sees Article 18 as a coordination mechanism for protecting the constitutional order. The court has the sole adjudicative authority to determine when forfeiture applies. From this perspective, the clause functions as a coordination tool: it clarifies that the order can defend itself without contradicting its foundational commitment to rights. The non-application is not a failure but evidence of successful deterrence through established doctrine.
constraint_indexing:constraint_classification(militant_democracy__basic_rights_forfeiture, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — Views Article 18 as instantiating an immutable paradox of liberal order: a free system must be able to defend itself against those who would use freedom to destroy freedom. This is a logical necessity, not a contingent institutional choice. However, the structural data (75 years of non-application, deterrence through threat rather than execution) suggests this reading naturalizes a doctrinal choice as a logical law. Engine will flag as false summit.
constraint_indexing:constraint_classification(militant_democracy__basic_rights_forfeiture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: OPPOSITION MOVEMENT (ORGANIZED) — Organized agitators see Article 18 as mixed coordination and extraction. The doctrine coordinates the system's self-defense while extracting from their exit options: they can organize and speak, but under the constant shadow of potential forfeiture. The long non-application creates a perverse game: the threat is most effective when dormant. Exit is constrained by the threat structure, not by active enforcement.
constraint_indexing:constraint_classification(militant_democracy__basic_rights_forfeiture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(militant_democracy__basic_rights_forfeiture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(militant_democracy__basic_rights_forfeiture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(militant_democracy__basic_rights_forfeiture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(militant_democracy__basic_rights_forfeiture, TR),
    TR >= 0.70.

:- end_tests(militant_democracy__basic_rights_forfeiture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low and declining. The constraint's extractiveness has fallen over 75 years because the threat-mechanism has not been invoked. The potential extraction (loss of basic rights) is catastrophic in scope but probabilistically near-zero in practice. Measured extractiveness reflects realized enforcement, not doctrinal scope — the clause sits on the books as a deterrent without executing. Suppression (0.35): Moderate. Targeted agitators face real legal exposure, but the long non-application record means suppression operates through latent threat rather than active enforcement. The suppression is conditional (triggered by specific abuses, not categorical status) and individual (applied case-by-case, not to classes). This is lower than the suppression of systems with routine enforcement. Theater ratio (0.82): High and rising. The doctrinal machinery is maintained through continuous reaffirmation (court opinions, constitutional scholarship, political rhetoric defending militant democracy) despite the absence of enforcement. The ritual of commitment to the doctrine increasingly replaces the functional enforcement — this is the definition of piton-level theater. The rising theater trajectory reflects that as the threat recedes in practical likelihood, the symbolic and doctrinal maintenance intensifies.
 *
 * PERSPECTIVAL GAP:
 *   The targeted agitator sees threat-mechanism (piton: mostly theater, latent suppression). The constitutional court sees coordination (rope: the doctrine clarifies the order's self-defense without contradicting rights commitment). The civilizational analytical observer risks seeing logical necessity (mountain: a free system must be able to defend itself), but structural data reveals this as a false summit — it naturalizes a specific doctrinal choice (rights-forfeiture, not office-disqualification or party-ban) as inevitable. The organized opposition sees mixed dynamics (tangled_rope: constrained by threat, yet some agency within legal bounds).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary (constitutional order) and victim (targeted agitators) are structurally asymmetric. The constitutional court, as the sole authority for forfeiture, derives arbitrage-level exit options — it can invoke or withhold application, and this discretion is its power. Targeted agitators have trapped exit options: they face legal jeopardy that they cannot negotiate or arbitrage away. However, the 75-year non-application record means the experienced extractiveness (chi) is much lower than the doctrinal scope would suggest. The piton classification derives from theater_ratio dominance (0.82) — the machinery is preserved through institutional commitment, not functional necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit high mandatrophy because extractiveness is low (0.18) and theater-ratio is high (0.82). The piton classification is stable — the constraint functions through maintenance of threat rather than through execution, and the theater increasingly dominates as actual enforcement recedes. The analytical observer's temptation toward mountain classification (logical necessity) is the only significant mandatrophy risk, and it is resolved by noting that the non-application record is the structural finding — if the doctrine were truly a law of nature, it would be applied; the fact that it is not suggests it is a contingent doctrinal choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dormancy_structural_or_contingent,
    'Is the 75-year non-application of Article 18 a structural feature (the doctrine functions precisely by NOT being invoked) or a contingent historical fact (application has happened to be prevented by prosecutorial caution or political calculation)?',
    'Comparative analysis: examine militant democracy clauses in other jurisdictions (Austria, France, Netherlands) and their application frequencies; interview constitutional court judges on decision thresholds for invoking Article 18; analyze failed or near-miss proceedings that did not result in forfeiture.',
    'If structural: the piton classification is correct — the constraint is maintained through non-use, and the theater dominates. If contingent: application is possible and the suppression metric understates actual extraction risk, moving the constraint toward tangled_rope or snare territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_structural_or_contingent, empirical, 'Whether dormancy is a structural feature or contingent historical fact').

omega_variable(
    threat_efficacy_deterrence,
    'Does the Article 18 threat actually deter anti-constitutional organizing, or has organizing intensity remained constant and the threat merely provides post-hoc legitimation for suppression if it occurs?',
    'Historical timeline analysis: track organizing intensity, recruitment, and manifesto publication by anti-order movements pre-and-post Article 18''s promulgation and entrenchment; compare suppression rates across legal systems with and without militant democracy clauses; analyze whether movements explicitly cite Article 18 in their strategic calculus.',
    'If deterrent: suppression metric is correct; the doctrine''s non-application reflects that it successfully prevents extreme organizing attempts. If illusory: suppression is lower than claimed, and the constraint is closer to piton-level performance (mostly theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_efficacy_deterrence, empirical, 'Whether Article 18 threat achieves deterrence or provides post-hoc legitimation').

omega_variable(
    basic_rights_vs_fundamental_rights_distinction,
    'Is the forfeiture of Grundrechte (basic/fundamental rights) a logical consequence of the Schranken-Schranken doctrine (limits on limiting constitutional rights) or a contingent doctrinal choice with alternatives?',
    'Comparative constitutional analysis: examine how other systems handle anti-order actors (revocation of voting rights, office disqualification, party dissolution, criminal penalties vs. rights suspension); trace the genealogy of Article 18 through German constitutional theory to identify the specific choices (why rights-forfeiture rather than office-disqualification alone?).',
    'If logical: Article 18 is a necessary structural feature (mountain-adjacent). If contingent: it is a specific doctrinal choice that could be replaced by alternative enforcement mechanisms, and the piton classification shifts toward rope (the machinery maintains a particular doctrinal framework rather than serving a coordination function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(basic_rights_vs_fundamental_rights_distinction, conceptual, 'Whether rights-forfeiture follows necessarily from constitutional logic or is contingent doctrinal choice').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is this story instantiating the ''basic rights forfeiture'' reading, or collapsing the distinctions between the three sibling readings (lessons_of_weimar, party_ban_instrument) that inhabit the same kernel?',
    'Retrospective authoring clarity: does the narrative and structural data privilege the forfeiture doctrine itself (its mechanism, application thresholds, dormancy) as the organizing constraint, or does it collapse into a broader ''militant democracy as anti-totalitarian defense'' narrative that equally applies to party-ban procedures? The three readings have distinct ε values and distinct beneficiary/victim structures — if this story conflates them, it violates ε-invariance.',
    'If distinct: this story is correctly scoped to forfeiture alone. If collapsed: should decompose into three separate constraint stories (one per reading) with network links per the ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this story correctly isolates basic rights forfeiture as distinct reading or conflates with sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(militant_democracy__basic_rights_forfeiture, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mili_tr_t0, militant_democracy__basic_rights_forfeiture, theater_ratio, 0, 0.65).
narrative_ontology:measurement(mili_tr_t30, militant_democracy__basic_rights_forfeiture, theater_ratio, 30, 0.78).
narrative_ontology:measurement(mili_tr_t75, militant_democracy__basic_rights_forfeiture, theater_ratio, 75, 0.82).

% Extraction over time
narrative_ontology:measurement(mili_be_t0, militant_democracy__basic_rights_forfeiture, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mili_be_t30, militant_democracy__basic_rights_forfeiture, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(mili_be_t75, militant_democracy__basic_rights_forfeiture, base_extractiveness, 75, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(militant_democracy__basic_rights_forfeiture, enforcement_mechanism).
narrative_ontology:affects_constraint(militant_democracy__basic_rights_forfeiture, militant_democracy__lessons_of_weimar_reading).
narrative_ontology:affects_constraint(militant_democracy__basic_rights_forfeiture, militant_democracy__party_ban_instrument).

% DUAL FORMULATION NOTE:
% Article 18 (basic rights forfeiture), Article 21 (party ban), and the historical lessons-of-Weimar framing form a constraint family. All three inhabit the militant_democracy kernel. This story isolates the forfeiture reading; siblings instantiate the Weimar-lessons and party-ban readings. Separate stories required because forfeiture has ε≈0.18 (dormant piton), party bans have ε≈0.35+ (active institutional mechanism), and the Weimar-lessons reading has ε≈0.25 (doctrinal commitment). The ε-invariance principle mandates separate stories — the observable (mechanism type) changes the structural properties measurably.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
