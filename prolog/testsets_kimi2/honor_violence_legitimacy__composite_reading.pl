% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Aristocratic Honor Violence Legitimacy (Composite Decline Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story models the aristocratic honor violence legitimacy
 *   structureâthe social code requiring violent redress for insultâunder
 *   the composite historiographical reading. The kernel
 *   'honor_violence_legitimacy' is contested between three readings: drop
 *   (external costs alone explain the decline), contraction (conceptual
 *   redefinition alone), and composite (both mechanisms operated
 *   simultaneously). The composite reading holds that the decline was
 *   overdetermined: external costs (legal prosecution, professional
 *   disqualification, mortality) raised the price of compliance, while
 *   conceptual redefinition progressively excluded violence from the meaning
 *   of honor. The contraction edge was structurally necessary because cost
 *   mechanisms alone were insufficientâthey suppressed practice without
 *   dissolving legitimacy. The interval covers the long nineteenth century
 *   (1800â1900) during which these dual mechanisms jointly degraded the
 *   constraint from a functional coordination-extraction structure to a
 *   theatrical remnant.
 *
 * KEY AGENTS:
 *   - aristocratic_hierarchy (agenda_setter/beneficiary): Maintains the code and captures status-boundary benefits through courts of honor and social ostracism.
 *   - gentleman_duellists (payer): Young aristocrats bearing physical and legal costs of the drop mechanism; identity-locked into the violent honor code.
 *   - traditionalist_gentry (payer): Provincials bearing obsolescence costs of conceptual contraction as honor is redefined around civic virtue.
 *   - state_apparatus (agenda_setter): Raises external costs through prosecution and legal reform, enforcing the drop mechanism.
 *   - women_and_dependents (excluded): Bear familial and economic costs while structurally excluded from honor discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.25).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.2).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Aristocratic Honor Violence Legitimacy (Composite Decline Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, 'd5cf2bd0-d39c-468c-a56d-ea4620342d5c').
narrative_ontology:cs_kernel_codification('d5cf2bd0-d39c-468c-a56d-ea4620342d5c', fixed_text).
narrative_ontology:cs_authority_grounding('d5cf2bd0-d39c-468c-a56d-ea4620342d5c', practice).
narrative_ontology:cs_interpretation_layer_present('d5cf2bd0-d39c-468c-a56d-ea4620342d5c').
narrative_ontology:cs_reading_relation('d5cf2bd0-d39c-468c-a56d-ea4620342d5c', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('d5cf2bd0-d39c-468c-a56d-ea4620342d5c', honor_violence_legitimacy__contraction_reading, influences).
narrative_ontology:cs_axiom('d5cf2bd0-d39c-468c-a56d-ea4620342d5c', foundational, overdetermined_decline).
narrative_ontology:cs_axiom_status(overdetermined_decline, holdable).
narrative_ontology:cs_axiom_grounding('d5cf2bd0-d39c-468c-a56d-ea4620342d5c', overdetermined_decline, empirically_contingent).
narrative_ontology:cs_axiom('d5cf2bd0-d39c-468c-a56d-ea4620342d5c', secondary, conceptual_contraction_structural_necessity).
narrative_ontology:cs_axiom_status(conceptual_contraction_structural_necessity, holdable).
narrative_ontology:cs_axiom_grounding('d5cf2bd0-d39c-468c-a56d-ea4620342d5c', conceptual_contraction_structural_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('d5cf2bd0-d39c-468c-a56d-ea4620342d5c', violent_aristocratic_honor).
narrative_ontology:cs_drift_state('d5cf2bd0-d39c-468c-a56d-ea4620342d5c', late_nineteenth_century, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('d5cf2bd0-d39c-468c-a56d-ea4620342d5c', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, aristocratic_hierarchy).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, gentleman_duellists).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, traditionalist_gentry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the terms of honorable conduct through courts of honor, etiquette manuals, and social ostracism. Captures class-boundary maintenance and intra-elite discipline. In the composite reading, it both administers the old code and gradually accommodates conceptual redefinition as honor is detached from violence.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, aristocratic_hierarchy, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, aristocratic_hierarchy, beneficiary).

% Young men of gentle birth compelled to fight to maintain social standing. Under the drop mechanism, they bear mortality, injury, and legal prosecution costs. Their social identity is constituted through the violent honor code; refusal means dissolution of self and status.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, gentleman_duellists, payer,
    moderate, biographical, identity_locked, national).

% Provincial gentlemen whose standing depends on the old violent honor code. Under conceptual contraction, they suffer status degradation as honor is redefined around civic and commercial virtue. They cannot exit without abandoning the identity that grounds their local authority.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, traditionalist_gentry, payer,
    moderate, biographical, identity_locked, regional).

% Legal and administrative bodies that impose external costs on dueling through criminal prosecution, military discipline, and professional disqualification. They enforce the drop mechanism without deriving benefit from the honor code itself, representing a separate enforcement layer.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, state_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Excluded from the masculine honor discourse but bear the familial and economic consequences of injury, death, and property loss. Their structural exclusion is constitutive of the code's operation; they have no voice in honor deliberations.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, women_and_dependents, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regulates intra-elite conflict and verifies gentlemanly status by providing a mechanism for proving willingness to defend one's name, thereby coordinating membership boundaries in an aristocratic society without centralized personal-reputation adjudication.
% TRANSFER_FUNCTION: Moves physical risk, legal jeopardy, and social obsolescence from junior and traditionalist members to the aristocratic collective's boundary-maintenance function, while the hierarchy collects status monopoly and intra-elite discipline.
% ABSENT_VOICES: Women and non-combatant family members who bore the economic and emotional costs of injury and death were excluded from honor deliberations; bourgeois commercial classes who rejected violent honor were present in society but not admitted to the aristocratic discourse that set the terms of legitimate conduct.
% DISAPPEARANCE_RATIONALE: The aristocratic class would lose a primary mechanism of intra-elite discipline and boundary policing; masculine socialization would shift from arms-bearing to civic and commercial virtue; legal systems would no longer need to adjudicate honor-based violence; and the identity structure of traditionalist gentry would collapse without its organizing code.
% FOUNDING_PROBLEM: In an aristocratic society without centralized state enforcement of personal reputation, there was no mechanism to regulate intra-elite conflict and verify gentlemanly status.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists outside the aristocratic beneficiary class attest that state commercial law and professional reputation systems have replaced the duel's coordination function. Bourgeois commentators and religious reformers from the eighteenth century onward corroborated that the founding problem of stateless elite conflict was solved by institutional modernization.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__composite_reading, 0.25, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).
:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness falls from 0.78 to 0.25 across the interval because the dual mechanisms progressively hollow out the constraint's material force. Theater_ratio rises from 0.20 to 0.80, indicating that by 1900 the constraint persists primarily as performance and memory rather than functional violence. Suppression_requirement falls from 0.85 to 0.20 because the social ostracism that enforced compliance loses its grip as conceptual contraction advances. Resistance rises to 0.85 by interval end, reflecting consolidated state, religious, and bourgeois opposition. Accessibility_collapse is low (0.30) at interval end because alternatives to honor violence (legal remedy, commercial reputation) are widely visible. The metrics and claim are independently authored: the constraint is claimed as tangled_rope because it always combined genuine coordination (intra-elite boundary maintenance) with asymmetric extraction (life and identity from junior members), while the metrics honestly capture its terminal degradation.
 *
 * PERSPECTIVAL GAP:
 *   The aristocratic hierarchy and the state apparatus both hold institutional power but compute different types: the hierarchy experiences the constraint as a coordination mechanism it administers and benefits from, while the state experiences it as an object to be suppressed through external costs. The gentleman_duellist and traditionalist_gentry seats both pay, but through different mechanismsâphysical jeopardy versus identity obsolescenceâproducing divergent directionality despite similar power levels. The engine computes this asymmetry from the structural data: identity_locked exit amplifies effective extraction for both payer seats, while the hierarchy's constrained exit and beneficiary role damp it.
 *
 * DIRECTIONALITY LOGIC:
 *   The aristocratic_hierarchy is the structural beneficiary (low directionality): it collects status-boundary maintenance and intra-elite discipline without paying the physical or identity costs. The gentleman_duellists and traditionalist_gentry are the structural targets (high directionality): they bear the mortality, legal, and obsolescence costs. The state_apparatus sits near symmetricâit does not collect from the constraint's operation but imposes costs on it from outside. Women_and_dependents are excluded from directionality derivation (trapped, powerless) because the constraint structurally silences them rather than extracting from them through the honor mechanism itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The dueling code began as a coordination mechanism for stateless elite conflict regulation (a rope or tangled rope) but its founding problem was solved by state commercial law and professional reputation systems. The constraint persisted beyond its functional mandate, becoming increasingly theatrical. Declaring mandatrophy_resolved prevents mislabeling the late-phase constraint as active coordination; the high theater_ratio (0.80) and low suppression (0.20) at interval end confirm that what remains is inertia and performance, not genuine regulatory function. The composite reading specifically guards against the mandatrophy error of attributing decline solely to external costs (drop) or solely to conceptual change (contraction); by asserting both, it captures that the coordination function atrophied through simultaneous material and ideational pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    composite_reading_scope,
    'Is the composite reading''s overdetermination claim a genuine third constraint distinct from drop and contraction, or does it decompose into a sequential application of the sibling constraints?',
    'Temporal decomposition: if the historical record shows clear phase separation (first drop, then contraction), composite dissolves into sequence; if both mechanisms operate in every phase, composite is irreducibly distinct.',
    'If decomposable, the composite reading should be split into two linked constraints (drop_phase and contraction_phase); if irreducible, the dual-mechanism structure justifies the single composite story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_reading_scope, conceptual, 'Whether composite reading is irreducible or sequential').

omega_variable(
    dual_victim_overlap,
    'Do the gentleman_duellists (drop victims) and traditionalist_gentry (contraction victims) represent overlapping cohorts or structurally distinct agent classes?',
    'Cohort analysis of dueling populations 1800â1900: if the same individuals suffered both physical costs and identity obsolescence, the victim sets merge; if distinct generations or regions suffered each, they remain separate.',
    'Merged victim sets concentrate extraction and may elevate computed severity; distinct sets indicate diffuse extraction across multiple seats with different exit profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_victim_overlap, empirical, 'Overlap between drop and contraction victim sets').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s suppression structural (external social and legal penalties) or internalized (the gentleman''s honor ethic compelling violence regardless of external threat)?',
    'Post-legislation behavioral trajectory: if dueling ceased immediately when laws changed, suppression was structural; if secret dueling or psychological distress persisted, suppression was internalized.',
    'Internalized suppression raises effective extraction beyond structural measures and may reclassify late-phase behavior as identity-locked rather than merely constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__composite_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hono_tr_t25, honor_violence_legitimacy__composite_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(hono_tr_t50, honor_violence_legitimacy__composite_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(hono_tr_t75, honor_violence_legitimacy__composite_reading, theater_ratio, 75, 0.65).
narrative_ontology:measurement(hono_tr_t100, honor_violence_legitimacy__composite_reading, theater_ratio, 100, 0.8).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__composite_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(hono_be_t25, honor_violence_legitimacy__composite_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(hono_be_t50, honor_violence_legitimacy__composite_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(hono_be_t75, honor_violence_legitimacy__composite_reading, base_extractiveness, 75, 0.4).
narrative_ontology:measurement(hono_be_t100, honor_violence_legitimacy__composite_reading, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__composite_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(hono_su_t25, honor_violence_legitimacy__composite_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(hono_su_t50, honor_violence_legitimacy__composite_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(hono_su_t75, honor_violence_legitimacy__composite_reading, suppression_requirement, 75, 0.35).
narrative_ontology:measurement(hono_su_t100, honor_violence_legitimacy__composite_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the honor_violence_legitimacy kernel. The composite reading holds that drop (external costs) and contraction (conceptual redefinition) operated simultaneously and are structurally inseparable in explaining the decline. See sibling constraints for the single-mechanism readings and their distinct Îµ profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
