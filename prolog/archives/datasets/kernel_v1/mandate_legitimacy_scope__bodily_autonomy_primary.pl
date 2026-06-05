% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__bodily_autonomy_primary, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Medical Mandate as Bodily Integrity Violation (Autonomy-Primary Reading)
 *   domain: public_health/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primary reading of
 *   the mandate_legitimacy_scope kernel. From this reading, medical
 *   intervention without informed consent violates fundamental bodily
 *   integrity as a categorical normative principle, regardless of collective
 *   public health benefit. When mandates are enforced — preventing
 *   employment, education, or healthcare access for the unvaccinated — the
 *   state becomes a rights violator, and the constraint exhibits pure
 *   extraction (snare) with high suppression. This reading does NOT dispute
 *   that vaccines are effective at preventing disease or that herd immunity
 *   has collective benefits. Rather, it asserts that these goods do not
 *   justify overriding an individual's right to refuse medical procedures on
 *   their own body. The constraint's extractiveness (0.62) reflects that the
 *   state extracts compliance through coercive mechanisms (employment loss,
 *   social exclusion, healthcare denial) while providing benefit to the
 *   public health system, not to the coerced individual. Suppression (0.78)
 *   is high because alternatives to compliance are costly and effectively
 *   unavailable for those dependent on employment or state services. Theater
 *   ratio (0.35) is relatively low because mandate enforcement is direct and
 *   functional — the coercion works, not through performative ritual but
 *   through tangible penalties.
 *
 * KEY AGENTS:
 *   - Vaccine-hesitant or medically autonomous individuals: Primary victims (powerless/trapped) — face employment loss, educational exclusion, healthcare access barriers. Provide no consent; bear all costs.
 *   - Medical autonomy rights advocates: Organized victims (organized/constrained) — patient rights organizations, libertarian/conservative political movements, some bioethicists. Can organize resistance but cannot exit the constraint system.
 *   - Public health authorities: Beneficiaries (institutional/arbitrage) — experience mandate as legitimate coordination mechanism for achieving herd immunity. Derive authority and compliance from mandate enforcement.
 *   - Marginalized communities with historical medical trauma: Organized victims (organized/constrained) — Tuskegee study survivors, forced sterilization victims, groups with documented distrust of medical systems. Experience mandates as renewed state coercion, activating historical wounds.
 *   - Medical autonomy principle itself: Victim (powerless/analytical) — abstract rights holder with no institutional agency; treated as overridable by state authority.
 *   - Analytical observer: Witnesses (analytical/analytical) — sees the constraint as a fundamental rights violation from the bodily_autonomy_primary frame, but observes that other readings (public_health_primary, proportionality_reading) coexist in institutional and political discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.62).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.78).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Medical Mandate as Bodily Integrity Violation (Autonomy-Primary Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, '838bef2d-a124-4a94-84ff-a001b34c3d49').
narrative_ontology:cs_kernel_codification('838bef2d-a124-4a94-84ff-a001b34c3d49', formalized).
narrative_ontology:cs_authority_grounding('838bef2d-a124-4a94-84ff-a001b34c3d49', extraction).
narrative_ontology:cs_interpretation_layer_present('838bef2d-a124-4a94-84ff-a001b34c3d49').
narrative_ontology:cs_reading_relation('838bef2d-a124-4a94-84ff-a001b34c3d49', mandate_legitimacy_scope__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('838bef2d-a124-4a94-84ff-a001b34c3d49', mandate_legitimacy_scope__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('838bef2d-a124-4a94-84ff-a001b34c3d49', foundational, bodily_integrity_categorical).
narrative_ontology:cs_axiom_status(bodily_integrity_categorical, holdable).
narrative_ontology:cs_axiom_grounding('838bef2d-a124-4a94-84ff-a001b34c3d49', bodily_integrity_categorical, deontological).
narrative_ontology:cs_axiom('838bef2d-a124-4a94-84ff-a001b34c3d49', foundational, informed_consent_nonnegotiable).
narrative_ontology:cs_axiom_status(informed_consent_nonnegotiable, holdable).
narrative_ontology:cs_axiom_grounding('838bef2d-a124-4a94-84ff-a001b34c3d49', informed_consent_nonnegotiable, deontological).
narrative_ontology:cs_reference_frame('838bef2d-a124-4a94-84ff-a001b34c3d49', consent_primacy_framework).
narrative_ontology:cs_drift_state('838bef2d-a124-4a94-84ff-a001b34c3d49', contemporary_mandate_enforcement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('838bef2d-a124-4a94-84ff-a001b34c3d49', '2026-02-26T14:30:00Z').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, medical_autonomy_rights_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COERCED INDIVIDUAL (SNARE) — Faces direct bodily integrity violation with no exit available. Cannot refuse medical intervention without employment loss, educational exclusion, or healthcare denial. Structural coercion without genuine alternative. High suppression (loss of livelihood, social stigma); high extractiveness (state enforces medical procedure regardless of consent). Full target position.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__bodily_autonomy_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MEDICAL AUTONOMY RIGHTS BEARERS (SNARE) — Possess abstract rights to bodily integrity but face high costs of exercising them (career disruption, social exclusion, healthcare access barriers). Exit exists but is prohibitively expensive. Extraction mechanism: state transfers all costs of noncompliance to individual while claiming public health benefit.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__bodily_autonomy_primary, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — From this perspective, the mandate is pure coordination: achieving herd immunity threshold requires collective action; the state is solving a collective action problem. This perspective benefits from mandate enforcement and experiences it as legitimate authority. But from the bodily_autonomy_primary reading, this perspective naturalizes coercion as coordination.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__bodily_autonomy_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDICAL AUTONOMY ADVOCATES (TANGLED ROPE) — Powerful institutional actors (patient rights organizations, libertarian think tanks, some medical ethicists, constitutional scholars) coordinate against mandates while also benefiting from the medical infrastructure mandates protect. Can exit (by relocating, by shifting policy advocacy), but extraction is real: forced to spend resources on resistance. See genuine coordination failure: state could achieve vaccination without coercion (access improvement, incentives, targeted protection), but instead chooses coercive mechanism.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__bodily_autonomy_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL RIGHTS (MOUNTAIN) — From a civilizational/universal perspective grounded in fundamental bodily integrity as an irreducible human right, medical coercion is categorically impermissible. Bodily autonomy is treated as an immutable floor: no collective benefit justifies override. This is the reading's foundational claim. However, this perspective risks naturalizing what is actually a chosen normative principle as an inherent law — the false summit detector will flag this.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__bodily_autonomy_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: MARGINALIZED COMMUNITIES (ORGANIZED SNARE) — Historical medical coercion (Tuskegee, forced sterilization, medical experimentation without consent) creates justified distrust. Mandates activate this trauma and are experienced as renewed state coercion. Organized resistance is possible but constrained by lack of institutional power. High suppression: state authority backed by criminal/civil penalties. High extractiveness: communities forced to participate in medical regime that historically victimized them, while state claims public health benefit.
constraint_indexing:constraint_classification(mandate_legitimacy_scope__bodily_autonomy_primary, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mandate_legitimacy_scope__bodily_autonomy_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mandate_legitimacy_scope__bodily_autonomy_primary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62, rising to 0.62 by t4): Moderate-high and increasing. The state extracts compliance (vaccination or acceptance of severe penalties) from those who would not consent if exemption were available. The extraction is not total because some individuals have exit options (relocation, remote work, medical exemptions where available) — hence 0.62 rather than 0.72+. But for those dependent on employment, education, or state healthcare, exit is not real. The trajectory shows rising extractiveness as mandates persist: initial hesitation (t0: 0.35, mostly voluntary compliance with some coercion) → enforcement ramp (t2: 0.50, coercion mechanisms activate) → full enforcement (t4: 0.62, all compliance pathways collapse except mandate acceptance). Suppression (0.78, rising from 0.55): High and rising. Mechanisms include job loss (most severe for low-wage workers), educational exclusion (affects children and students), healthcare access denial (affects health outcomes), social stigma (community exclusion). These are not equal across populations: individuals with financial reserves, remote-work options, or private healthcare can more readily absorb penalties. Individuals dependent on state employment, public education, or state healthcare face near-total suppression of alternatives. Theater ratio (0.35, rising from 0.20): Low. Mandate enforcement is direct and functional — state deploys employment law, education policy, and healthcare access restrictions to produce compliance. There is some performative element (press conferences, emergency declarations, public health messaging) but the coercive mechanisms work without theater. The rise from 0.20 to 0.35 reflects increasing use of performance (social shame campaigns, media messaging) to reduce explicit enforcement costs, but the underlying coercion remains structural.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the coerced individual (Snare, powerless/trapped, high experienced extraction) and the public health authority (Rope, institutional/arbitrage, low experienced extraction). From the individual's perspective, the state is extracting bodily compliance through coercion. From the public health authority's perspective, the state is solving a collective action problem — without mandates, free-riders would refuse vaccination and prevent herd immunity. The analytical observer sees this as a genuine conflict between two normative principles (bodily autonomy vs. collective health protection) rather than as a factual dispute about vaccine efficacy. The bodily_autonomy_primary reading asserts that the individual's perspective is correct — bodily integrity is not overridable by collective benefit. The public_health_primary reading would assert the opposite — collective protection justifies mandate. The proportionality_reading tries to split the difference: mandates are legitimate ONLY if alternatives are genuinely unavailable, disease severity justifies override, and less restrictive means have been exhausted. This constraint story instantiates the first reading only, showing how it classifies the constraint as snare from the individual perspective and rope (or false summit) from the authority perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes how much each agent bears the constraint's extraction flow. The coerced individual has d ≈ 0.95 (full target): they receive no benefit, bear all costs, have no exit. The public health authority has d ≈ 0.05 (full beneficiary): it derives authority and compliance, experiences the constraint as solving its coordination problem, has arbitrage exit (can choose to enforce or not). The medical autonomy advocate has d ≈ 0.85 (near-full target but with some exit): they bear costs (resource expenditure on resistance) but have some power to relocate or shift policy advocacy, so not completely trapped. The marginalized communities with historical trauma have d ≈ 0.90 (full target): they bear psychological and health costs from mandate enforcement while deriving no benefit, with minimal exit options. These directionality values feed the sigmoid f(d) to compute effective extractiveness (chi). Higher d produces higher f(d), amplifying the experienced extraction. The snare classification emerges from high d + high ε + high suppression, without any meaningful coordination function for the victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint requires mandatrophy analysis because ε (0.62) > 0.46. The mandatrophy is: 'Does the constraint provide any genuine coordination function, or is it pure extraction masked as public health?' From the bodily_autonomy_primary reading, the answer is clear: the constraint provides no coordination to the victims. The victims derive zero benefit from herd immunity protection (they gain immunity either by vaccination or infection, not by mandate). The beneficiaries (public health authorities, public who free-ride on herd immunity) derive the benefit. From the public_health_primary reading, the answer is opposite: the constraint IS a coordination mechanism — it solves the free-rider problem where individuals would refuse vaccination while benefiting from others' vaccination. This constraint story resolves mandatrophy by being explicit about which reading it instantiates. The bodily_autonomy_primary reading does not claim the constraint provides coordination to victims; it claims the constraint violates a higher-order right (bodily integrity) that supersedes coordination benefits. If coordination benefits were present, the reading would have to address them (by claiming coordination does not justify bodily violation, or by contesting that true coordination exists). The resolution is: this reading treats bodily autonomy as a foundational principle that forecloses coordination-based justifications. Empirically, the constraint either provides real coordination (in which case proportionality_reading applies) or it provides extraction (in which case snare classification holds).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bodily_autonomy_absolutism_vs_threshold,
    'Is bodily integrity an absolute categorical constraint, or does it permit thresholds (severe disease, imminent harm, vulnerable population protection)?',
    'Normative principle clarification through legal and bioethical scholarship; examination of whether the reading permits ANY exceptions and under what criteria. If threshold admits exceptions, reading collapses into proportionality_reading. If absolute, reading holds categorically.',
    'If absolute: bodily_autonomy_primary reading maintains independence; mandates are categorically illegitimate. If threshold-based: reading forecloses into proportionality_reading; no independent constraint exists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bodily_autonomy_absolutism_vs_threshold, conceptual, 'Whether bodily autonomy is absolute or permits severity-based thresholds').

omega_variable(
    coercion_vs_incentive_distinction,
    'Is the distinction between coercion (mandate with enforcement) and incentives (benefit/penalty for compliance) structurally significant, or does it collapse under scrutiny?',
    'Analysis of: (1) whether individuals with minimal resources experience ''incentive'' as effectively coercive (medical access denial = coercion for those depending on state healthcare); (2) whether penalty distribution differs by socioeconomic status (if yes: coercion is present). Examine actual compliance mechanisms: job loss, educational exclusion, social stigma — are these incentives or coercion?',
    'If collapsed: extraction is higher than 0.62; suppression may exceed 0.85 (mountain boundary). If distinct: extractiveness and suppression values hold; snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_vs_incentive_distinction, empirical, 'Whether coercion and incentive are structurally distinct or collapsed mechanisms').

omega_variable(
    state_authority_vs_rights_violation_foreclosure,
    'Does acceptance of state public health authority logically foreclose the bodily_autonomy_primary reading, or can both claims coexist in different normative frameworks?',
    'Examination of whether public_health_primary and bodily_autonomy_primary can be held simultaneously by a single institutional actor or framework. If an actor (e.g., a court) accepts state public health authority AND bodily autonomy as coequal principles, readings coexist. If actor must choose one, readings foreclose each other.',
    'If coexist: reading_relations entries are coexists_with. If foreclose: reading_relations are forecloses (one-directional: does bodily_autonomy_primary foreclose public_health_primary, or vice versa?). This determines whether the kernel contest is genuinely triadic or whether one reading eliminates another.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_vs_rights_violation_foreclosure, conceptual, 'Whether state public health authority and bodily autonomy can coexist as principles').

omega_variable(
    alternative_public_health_mechanisms_availability,
    'Are non-coercive public health mechanisms (access improvement, targeted protection, voluntary incentives without enforcement, risk-based tiering) functionally sufficient to achieve public health goals, or do mandates provide unique epidemiological value?',
    'Empirical comparison: vaccination rates achieved through mandate vs rates achieved through access+incentive+transparency across jurisdictions. Analysis of disease control outcomes with/without coercive mechanisms. Do non-mandate regimes achieve equivalent herd immunity thresholds?',
    'If non-coercive mechanisms are sufficient: snare classification is strengthened (state chose coercion despite alternatives); extractiveness and suppression values confirmed. If mandates uniquely enable disease control: constraint shifts toward tangled_rope or scaffold (genuine coordination function, sunset possible if alternatives mature). Empirical result directly changes ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_public_health_mechanisms_availability, empirical, 'Whether alternative public health mechanisms can achieve equivalent outcomes without coercion').

omega_variable(
    reading_applicability_scope_ambiguity,
    'Does bodily_autonomy_primary apply universally to all medical interventions, or only to vaccines/novel interventions/reversible procedures? What distinguishes in-scope from out-of-scope interventions?',
    'Textual analysis of the reading''s own scope claims. Does the reading say ''all medical intervention'' (maximalist) or ''non-emergency, non-life-saving, voluntary-benefit interventions'' (minimal)? Survey bioethicists/legal scholars holding this reading on scope boundaries.',
    'If maximalist: mandate legitimacy applies to all contexts (cancer treatment mandates, surgical requirements) — reading becomes implausible. If minimal: reading applies narrowly to vaccines/prophylaxis — constraint scope is narrower than currently modeled. Scope affects ε calculation and perspectives'' applicability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_applicability_scope_ambiguity, conceptual, 'Applicability scope of bodily autonomy principle across intervention types').

omega_variable(
    committer_kernel_contest,
    'Which reading of mandate_legitimacy_scope is instantiated by this constraint story? Is this reading bodily_autonomy_primary (unvaccinated-coerced in victim set when mandates present; state as rights violator) or proportionality_reading or public_health_primary?',
    'This is a kernel reading problem. The constraint story instantiates bodily_autonomy_primary: medical intervention without informed consent violates bodily integrity regardless of collective benefit. The sibling readings (proportionality_reading, public_health_primary) are OTHER constraint stories (not written here). This omega documents the committer axis: which reading IS this, what are the siblings, and what structural relationships hold between readings.',
    'This is informational for the committer frame. No classification impact — the reading is declared in header.constraint_id. The impact is traceability: observers understand this story as ONE instantiation of a contested kernel, not as the kernel itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_contest, conceptual, 'Committer axis: this constraint instantiates bodily_autonomy_primary reading of mandate_legitimacy_scope kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mandate_ba_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mandate_ba_tr_t2, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 2, 0.28).
narrative_ontology:measurement(mandate_ba_tr_t4, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 4, 0.35).

% Extraction over time
narrative_ontology:measurement(mandate_ba_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mandate_ba_be_t2, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(mandate_ba_be_t4, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 4, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(mandate_ba_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mandate_ba_su_t2, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 2, 0.72).
narrative_ontology:measurement(mandate_ba_su_t4, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 4, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% mandate_legitimacy_scope is a contested kernel with three structurally distinct readings. This constraint story (bodily_autonomy_primary) has ε=0.62 (snare). The sibling stories (public_health_primary, proportionality_reading) will have different ε values reflecting different structural relationships. All three stories are affected by the same empirical facts (vaccine efficacy, disease severity, compliance mechanisms) but interpret these facts through different normative frameworks. The network links document that these are not three independent constraints but three readings of a single kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__bodily_autonomy_primary, institutional, 0.05).
constraint_indexing:directionality_override(mandate_legitimacy_scope__bodily_autonomy_primary, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
