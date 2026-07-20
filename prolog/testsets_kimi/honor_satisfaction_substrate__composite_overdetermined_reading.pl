% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor Satisfaction Substrate (Composite Overdetermined Reading)
 *   domain: historical/sociological/legal
 *
 * SUMMARY:
 *   The honor satisfaction substrate governed elite masculine conduct in
 *   early modern Europe and its colonial extensions, requiring gentlemen to
 *   defend personal and collective honor through ritualized violence. Under
 *   the composite overdetermined reading, the substrate's disappearance
 *   between the late eighteenth and early twentieth centuries was caused by
 *   the simultaneous, non-independent operation of exogenous legal
 *   suppression and endogenous honor-code transformation. The constraint
 *   coordinated status and dispute resolution among the gentleman class while
 *   extracting physical risk and social death from participants; it required
 *   active enforcement through ostracism and duel administration. This
 *   reading treats the substrate as a tangled rope that experienced both
 *   rope-breaking (coordination collapse under legal pressure) and mountain
 *   erosion (delegitimation of the honor code itself), with the two
 *   mechanisms causally entangled rather than additive.
 *
 * KEY AGENTS:
 *   - gentleman_class: Agenda-setter and beneficiary (powerful/continental/identity_locked) â collectively enforces the honor code and captures status rents
 *   - compelled_duelists: Primary payer (moderate/continental/identity_locked) â bears physical risk and coercion to participate
 *   - legal_state: Analytical observer (institutional/national/analytical) â enacts exogenous suppression through criminalization
 *   - church_institution: Analytical observer (institutional/continental/analytical) â contributes to endogenous delegitimation through moral condemnation
 *   - women_and_family_members: Excluded victims (powerless/local/trapped) â bear costs of violence without voice in the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.82).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.8).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor Satisfaction Substrate (Composite Overdetermined Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical/sociological/legal").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '165c37e6-4dc0-444b-9db1-26053cb6453b').
narrative_ontology:cs_kernel_codification('165c37e6-4dc0-444b-9db1-26053cb6453b', distributed).
narrative_ontology:cs_authority_grounding('165c37e6-4dc0-444b-9db1-26053cb6453b', practice).
narrative_ontology:cs_interpretation_layer_present('165c37e6-4dc0-444b-9db1-26053cb6453b').
narrative_ontology:cs_reading_relation('165c37e6-4dc0-444b-9db1-26053cb6453b', honor_satisfaction_substrate__practice_decline_reading, influences).
narrative_ontology:cs_reading_relation('165c37e6-4dc0-444b-9db1-26053cb6453b', honor_satisfaction_substrate__cultural_contraction_reading, influences).
narrative_ontology:cs_axiom('165c37e6-4dc0-444b-9db1-26053cb6453b', foundational, honor_decline_overdetermined).
narrative_ontology:cs_axiom_status(honor_decline_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('165c37e6-4dc0-444b-9db1-26053cb6453b', honor_decline_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('165c37e6-4dc0-444b-9db1-26053cb6453b', foundational, legal_cultural_entanglement).
narrative_ontology:cs_axiom_status(legal_cultural_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('165c37e6-4dc0-444b-9db1-26053cb6453b', legal_cultural_entanglement, empirically_contingent).
narrative_ontology:cs_reference_frame('165c37e6-4dc0-444b-9db1-26053cb6453b', classical_gentlemanly_honor_order).
narrative_ontology:cs_drift_state('165c37e6-4dc0-444b-9db1-26053cb6453b', modern_legal_monopoly_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('165c37e6-4dc0-444b-9db1-26053cb6453b', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, gentleman_class).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, compelled_duelists).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, women_and_family_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively sets and enforces the terms of honorable conduct through social ostracism, seconds, and informal courts of honor; benefits from the status boundary that excludes non-gentlemen and secures elite solidarity.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, gentleman_class, agenda_setter,
    powerful, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, gentleman_class, beneficiary).

% Individual gentlemen who, having been insulted or challenged, are compelled by social pressure to risk their lives in a duel; refusal means ostracism and loss of status, making exit synonymous with social death.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, compelled_duelists, payer,
    moderate, biographical, identity_locked, continental).

% Enacts and enforces laws criminalizing dueling, treating it as murder or assault; seeks to monopolize legitimate violence and substitute centralized courts for private honor satisfaction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, legal_state, observer,
    institutional, generational, analytical, national).

% Morally condemns dueling as sinful and forbidden; denies Christian burial to duelists; contributes to endogenous delegitimation by reclassifying honor violence as morally unacceptable.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, church_institution, observer,
    institutional, generational, analytical, continental).

% Bear the consequences of duelingâwidowhood, loss of male relatives, economic ruinâwithout any voice in the honor code's operation or in the duel itself.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, women_and_family_members, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__composite_overdetermined_reading, gentleman_class).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__composite_overdetermined_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates dispute resolution and status maintenance among elite men in the absence of a strong state monopoly on legitimate violence; provides a self-help mechanism for offenses against honor and personal reputation that formal law cannot adjudicate.
% TRANSFER_FUNCTION: Moves physical risk, social ostracism, and honor validation among gentlemen; transfers the monopoly on legitimate honor-violence to the aristocratic class while excluding women, lower classes, and legal alternatives.
% ABSENT_VOICES: Women, family members, religious dissenters, lower classes, and legal professionals who advocated for court-centered dispute resolution were excluded from the honor discourse; they would object to the violence and exclusivity but were not in the room.
% DISAPPEARANCE_RATIONALE: If the honor satisfaction substrate vanished, gentlemen could no longer use dueling to settle disputes or signal status; social order would reorganize around legal courts, bureaucratic offices, and commercial wealth as status markers, as historically observed.
% FOUNDING_PROBLEM: How to maintain social cohesion and status hierarchy among armed elite men in weak-state contexts where centralized law cannot reach insults to honor or personal reputation.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (Spierenburg, Wiener) and sociologists (Elias, Nisbett and Cohen) attest the founding problem from outside the benefiting parties; the legal state's consistent opposition and the church's moral condemnation confirm the problem was class-specific and contested rather than universal.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint compelled participants to risk death, injury, and property loss for social standing. Suppression is high (0.80) because the constraint maintained itself through active social ostracism and the elimination of non-violent alternatives to honor satisfaction. Theater is low (0.15) at peak operation because the duel was a functional coordination mechanism, though measurements show rising theatricality as the constraint declined. Accessibility collapse is high (0.85) because, for gentlemen, courts and apology were socially inaccessible alternatives to the duel. Resistance is substantial (0.60) because legal and religious institutions actively opposed the practice throughout its lifecycle. The measurement series tracks the overdetermined decline: extraction declines monotonically as legal suppression and cultural delegitimation entangle, while theater_ratio rises as remaining dueling becomes increasingly performative and vestigial.
 *
 * PERSPECTIVAL GAP:
 *   The gentleman_class seat perceives the constraint as necessary social order and legitimate self-help; the compelled_duelist seat perceives it as coercive violence backed by social death. The legal_state seat perceives it as criminal disorder to be suppressed. These divergences are structural, not perspectival illusions: the same arrangement is coordination from the agenda-setter seat and extraction from the payer seat. The engine computes this divergence from the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The gentleman_class is declared as both agenda_setter and beneficiary, deriving a low directionality (subsidy side). Compelled_duelists and women_and_family_members are declared as payer and victim, deriving high directionality (target side). The legal_state and church_institution are observer seats with analytical exit, deriving neutral directionality. No overrides are necessary because the structural derivation matches the historical relationships: those who enforced and benefited from the honor order faced low effective extraction, while those compelled to duel or excluded from the discourse faced high effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â maintaining order among armed elites without centralized law â is dead. The constraint persisted for decades after the problem was substantially solved by state formation and legal centralization, exhibiting mandatrophy. However, the composite reading prevents mislabeling the substrate as a pure piton: it was not merely inertial performance. Rather, it was actively enforced (tangled_rope) until both rope-breaking (legal suppression) and mountain erosion (honor delegitimation) simultaneously dissolved it. The high theater_ratio at interval end signals terminal piton-like behavior, but the constraint's historical classification remains tangled_rope because its active phase was genuinely functional coordination plus asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exogenous_endogenous_entanglement,
    'Are the exogenous legal suppression and endogenous honor code transformation causally separable mechanisms, or do they constitute a single overdetermined process?',
    'Comparative historical analysis across jurisdictions with varying legal suppression timing; if honor delegitimation precedes legal change in some regions and follows in others, mechanisms are partially separable; if they move in lockstep everywhere, entanglement is confirmed.',
    'If separable, the constraint decomposes into two distinct constraints (legal suppression and cultural transformation); if entangled, the composite reading is structurally necessary and sibling readings are partial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_endogenous_entanglement, empirical, 'Causal separability of exogenous and endogenous decline mechanisms').

omega_variable(
    honor_substrate_naturalness,
    'Was the honor satisfaction substrate experienced by participants as an immutable social fact (mountain-like) or as a contingent coordinating convention (rope-like)?',
    'Discourse analysis of gentlemanly correspondence and conduct literature: references to honor as ''natural'' versus ''customary'' indicate perceived modality.',
    'If experienced as mountain, the decline mechanism includes mountain erosion; if as rope, the decline is pure rope-breaking. This determines whether the composite reading''s mountain component is descriptively accurate or a post-hoc analytical frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_substrate_naturalness, conceptual, 'Perceived naturalness of the honor code to participants').

omega_variable(
    gentleman_class_beneficiary_ambiguity,
    'Does identifying the gentleman class as beneficiary of the honor substrate falsely naturalize a constructed status order?',
    'Examine whether honor codes were constructed to benefit a specific class versus emerging from broader social necessity.',
    'If constructed for class benefit, the constraint''s coordination function is cover for class extraction, shifting toward snare classification; if emergent, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gentleman_class_beneficiary_ambiguity, conceptual, 'Class interest versus emergent function of honor norms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1750, 0.15).
narrative_ontology:measurement(hono_tr_t1775, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1775, 0.2).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(hono_tr_t1825, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1825, 0.45).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1850, 0.65).
narrative_ontology:measurement(hono_tr_t1875, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1875, 0.8).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1900, 0.92).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1750, 0.85).
narrative_ontology:measurement(hono_be_t1775, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1775, 0.83).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1800, 0.78).
narrative_ontology:measurement(hono_be_t1825, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1825, 0.65).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1850, 0.45).
narrative_ontology:measurement(hono_be_t1875, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1875, 0.25).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1750, 0.8).
narrative_ontology:measurement(hono_su_t1775, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1775, 0.78).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(hono_su_t1825, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1825, 0.55).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1850, 0.35).
narrative_ontology:measurement(hono_su_t1875, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1875, 0.2).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1900, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_substrate kernel decomposes into three readings: composite_overdetermined_reading (both mechanisms entangled), practice_decline_reading (exogenous only), and cultural_contraction_reading (endogenous only). Each reading has a distinct epsilon and stakeholder structure; they form a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
