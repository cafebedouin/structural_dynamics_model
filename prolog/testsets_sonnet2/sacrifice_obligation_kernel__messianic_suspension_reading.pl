% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Messianic Suspension Reading of Sacrifice Obligation
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This story instantiates the messianic_suspension_reading of the
 *   sacrifice_obligation_kernel: the claim that the commandment to offer
 *   sacrifices is divinely suspended — held in legal abeyance by external
 *   circumstance (no Temple, no functioning priesthood, no available purity
 *   apparatus) — rather than transformed into a different obligation or
 *   reduced to symbolic memory. On this reading, study of the sacrificial
 *   laws functions instrumentally: it maintains the community's operational
 *   capacity to resume performance at messianic restoration, without itself
 *   constituting fulfillment of the mitzvah. The reading therefore authors
 *   low extractiveness: no one is currently required to perform an impossible
 *   act, so no one is failing, being coerced, or paying a penalty for
 *   non-performance. The beneficiary is a temporally displaced population —
 *   future generations at a hypothetical restoration — for whom present-day
 *   study preserves usable legal-operational knowledge. This is a scaffold:
 *   the coordination function (preserve operational readiness) is explicitly
 *   transitional, oriented toward a declared future condition (restoration)
 *   rather than toward the steady state of ongoing study-as-such.
 *
 * KEY AGENTS:
 *   - yeshiva_study_communities
 *   - future_generations_at_restoration
 *   - halakhic_authorities
 *   - individual_observant_jews
 *   - sibling_reading_advocates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.08).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Messianic Suspension Reading of Sacrifice Obligation").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority").

narrative_ontology:has_sunset_clause(sacrifice_obligation_kernel__messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, 'feec6ab1-56a1-4beb-9cf5-3299a76db5b0').
narrative_ontology:cs_kernel_codification('feec6ab1-56a1-4beb-9cf5-3299a76db5b0', fixed_text).
narrative_ontology:cs_authority_grounding('feec6ab1-56a1-4beb-9cf5-3299a76db5b0', lineage).
narrative_ontology:cs_interpretation_layer_present('feec6ab1-56a1-4beb-9cf5-3299a76db5b0').
narrative_ontology:cs_reading_relation('feec6ab1-56a1-4beb-9cf5-3299a76db5b0', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('feec6ab1-56a1-4beb-9cf5-3299a76db5b0', sacrifice_obligation_kernel__study_as_exercise_reading, influences).
narrative_ontology:cs_reading_relation('feec6ab1-56a1-4beb-9cf5-3299a76db5b0', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('feec6ab1-56a1-4beb-9cf5-3299a76db5b0', foundational, obligation_remains_binding_in_principle_during_suspension).
narrative_ontology:cs_axiom_status(obligation_remains_binding_in_principle_during_suspension, holdable).
narrative_ontology:cs_axiom_grounding('feec6ab1-56a1-4beb-9cf5-3299a76db5b0', obligation_remains_binding_in_principle_during_suspension, deontological).
narrative_ontology:cs_axiom('feec6ab1-56a1-4beb-9cf5-3299a76db5b0', foundational, study_is_instrumental_not_substitutive).
narrative_ontology:cs_axiom_status(study_is_instrumental_not_substitutive, holdable).
narrative_ontology:cs_axiom_grounding('feec6ab1-56a1-4beb-9cf5-3299a76db5b0', study_is_instrumental_not_substitutive, conventional).
narrative_ontology:cs_reference_frame('feec6ab1-56a1-4beb-9cf5-3299a76db5b0', temple_era_operative_sacrificial_law).
narrative_ontology:cs_drift_state('feec6ab1-56a1-4beb-9cf5-3299a76db5b0', post_destruction_diaspora_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('feec6ab1-56a1-4beb-9cf5-3299a76db5b0', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_at_restoration).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, yeshiva_study_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, individual_observant_jews).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, individual_observant_jews).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, divine_suspension_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, operational_continuity_of_temple_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study the laws of sacrifice (Kodashim, sections of Zevachim, Menachot, Mishneh Torah's Sefer Avodah) as an ongoing curricular practice, treating this study as maintaining institutional and legal readiness rather than as fulfilling the sacrificial mitzvah itself. Their exit from the practice would mean exit from the halakhic tradition's self-understanding, not merely a behavioral change; but nothing coercive holds them to it beyond communal and educational structures they largely built themselves.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, yeshiva_study_communities, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, yeshiva_study_communities, agenda_setter).

% A not-yet-existing population who, on this reading, will need the sacrificial order to be operationally re-instantiable at messianic restoration. They cannot act now; they are represented only through present-day custodianship of the knowledge. Nothing is extracted from them during the suspension since the obligation itself is inactive, not merely unenforced.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_at_restoration, beneficiary,
    powerless, civilizational, analytical, global).

% Rabbinic decisors who adjudicate that the sacrifice obligation is suspended by circumstance (absence of the Temple, ritual impurity, lack of a functioning altar and priesthood) rather than abrogated or transformed. They administer the doctrine, determine what counts as adequate 'study as readiness,' and could in principle declare it changed, but their authority rests on continuity with prior rulings, not on collecting rents from the suspension.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Live under a legal system that formally still contains the sacrificial commandments as binding-in-principle, even though no performance is currently possible or required. They bear a mild cost in devotional attention and communal expectation (some study or acknowledgment of these laws is customary) but pay no material extraction and are not coerced into any performance, since performance is categorically impossible and not demanded.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, individual_observant_jews, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, individual_observant_jews, beneficiary).

% Adherents of the performance-only, study-as-exercise, or symbolic-archive readings who would contest that suspension (rather than substitution, fulfillment-through-study, or cultural memory) is the correct account of what is happening. They are not part of this constraint's internal operation; their objections belong to the kernel-level dispute, not to this reading.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, sibling_reading_advocates, excluded,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__messianic_suspension_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__messianic_suspension_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a religious community's relationship to an obligation it cannot currently perform: it tells adherents what stance to take (neither abandon the law nor treat it as satisfied by proxy) and directs communal resources (study time, curricular structure) toward preserving the operational knowledge needed if the obligation reactivates.
% TRANSFER_FUNCTION: Moves attention and pedagogical labor from present devotional alternatives toward sustained study of sacrificial law; moves nothing material from any present victim, since the reading holds no one is currently obligated to perform and therefore no one is currently failing or being extracted from.
% ABSENT_VOICES: Advocates of the performance-only reading would object that calling the obligation 'suspended' rather than 'unfulfillable-but-still-binding' softens the law's force; symbolic-archive advocates would object that treating the material as operationally live overstates its current legal status. Neither voice is present inside this constraint — they belong to sibling readings.
% DISAPPEARANCE_RATIONALE: If the suspension doctrine vanished overnight, halakhic authorities and study communities dispute what would follow: some hold the obligation would simply be judged permanently inapplicable (closer to symbolic_archive), others hold it would collapse into pure aspiration without doctrinal grounding, straining the operational-readiness rationale for continued study. Communities structured around Kodashim study would face an identity question, but no material arrangement (temple, priesthood, sacrifice) would change, since none currently exists.
% FOUNDING_PROBLEM: After the Temple's destruction, the tradition needed an account of a commandment it could no longer perform: is the law gone, transformed into something else, or merely paused? The suspension reading answers that the divine command remains fully binding-in-principle but is inoperative pending conditions (Temple, purity, priesthood) outside human control, with study preserving the capacity to resume.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic and later halakhic authorities within the tradition attest the founding problem as live (the sacrificial order is expected to resume at restoration). Outside corroboration is harder to locate structurally: historians of religion and comparative legal scholars note the doctrine's function in preserving institutional memory without adjudicating its theological truth, which is the closest thing to an outside-the-benefiting-parties attestation available; no purely secular or adversarial party corroborates the doctrine's substantive claim.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).
:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.08) because on this reading nothing is being extracted from anyone during the suspension: the obligation is inert, not merely unenforced, so there is no gap between what is owed and what is delivered that could constitute a transfer. Suppression is low (0.12) because no one is coerced into study or performance; participation in study communities is largely voluntary and identity-affirming rather than compelled. Theater ratio is modest and rises slowly over the interval (0.10 to 0.20) reflecting that as centuries pass without restoration, an increasing share of study activity risks becoming performative continuity-maintenance rather than genuinely operational preparation — a slow drift worth tracking, not a claim that the practice has become hollow. Accessibility collapse is moderate (0.35): alternative readings (performance-only, study-as-exercise, symbolic-archive) remain live and contested, so this reading has not foreclosed its alternatives the way a mountain would. Resistance is low (0.15) since the reading is widely held within traditional communities and meets little internal contest, though sibling-reading advocates outside this constraint's operation would resist its framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Study communities and halakhic authorities are coded as beneficiaries/agenda-setters: they administer and benefit from a doctrine that gives coherent shape to their ongoing curricular practice, but they do not extract rents from anyone by doing so — their 'benefit' is meaning and institutional continuity, not material transfer. Future generations at restoration are the deepest beneficiary but are structurally powerless now (they do not exist yet) and bear no current cost. Individual observant Jews are coded payer/beneficiary jointly: they carry a mild devotional-attentional cost (customary study obligations, communal expectation) but also share in the benefit of the coherent legal self-understanding the doctrine provides — the two roles nearly cancel, consistent with the low ε.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification, contingent on a sunset condition (messianic restoration) rather than an indefinite steady state, prevents this reading from being mislabeled as pure extraction: unlike a snare, there is no victim set being actively drained, and unlike a tangled rope, there is no asymmetric beneficiary collecting rents through enforced compliance. The risk this reading watches for is not extraction but drift into pure performance (rising theater_ratio) if the 'operational readiness' framing outlives any plausible functional connection to an eventual restoration — that would be a mandatrophy question for a much later measurement point, not evident in the current record.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_alternative_readings_indeterminacy,
    'Is ''divine suspension pending restoration'' the correct account of the sacrificial obligation''s current status, or is one of the sibling readings (performance-only, study-as-exercise, symbolic-archive) the more accurate account of what is actually happening within the tradition?',
    'No empirical resolution mechanism exists internal to the framework; the question is adjudicated within halakhic discourse by appeal to textual precedent, communal consensus, and theological commitment. Cross-tradition comparison (how other legal systems handle categorically unperformable but nominally binding obligations) could inform but not settle the question.',
    'If study_as_exercise_reading were correct instead, present-day study would itself constitute fulfillment, eliminating the ''in-abeyance, no current fulfillment'' structure this reading depends on and closing the gap between obligation and performance that currently keeps extractiveness low. If symbolic_archive_reading were correct, the doctrine would carry no binding legal force at all, undermining this reading''s central axiom that the obligation remains binding-in-principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suspension_vs_alternative_readings_indeterminacy, conceptual, 'Which reading of the kernel is doctrinally correct is irreducibly contested across the four sibling readings.').

omega_variable(
    restoration_condition_definiteness,
    'Is ''messianic restoration'' a definite future condition that could in principle obtain (making the suspension genuinely temporary, i.e., scaffold-like), or is it functionally indefinite/perpetually deferred, making the ''transitional'' framing indistinguishable from a permanent steady state?',
    'Track whether communities that hold this reading treat restoration-preparedness as an active practical project (e.g., detailed contingency planning, active priestly-lineage tracking) versus a purely notional horizon with no operational content beyond the study itself.',
    'If restoration is functionally indefinite, the scaffold classification is less secure — a sunset clause that can never trigger functions more like a permanent constraint, which would push the classification toward piton (a preserved form whose function has become mostly performative) despite the low measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_condition_definiteness, conceptual, 'Whether the declared sunset condition (restoration) is a genuine temporal bound or a perpetually deferred horizon.').

omega_variable(
    study_instrumentality_vs_intrinsic_value,
    'Is study genuinely instrumental to operational readiness (as this reading claims), or has it become intrinsically valued as a devotional practice independent of any restoration-preparedness function, which would blur this reading into study_as_exercise_reading from the inside?',
    'Examine whether curricular emphasis and pedagogical justification within study communities foreground operational/legal-technical mastery (supporting instrumentality) or devotional/spiritual framing (supporting drift toward the exercise reading).',
    'If study has become intrinsically valued rather than instrumentally justified, the theater_ratio trend already visible in the measurements (0.10 to 0.20) would be expected to continue rising, and the scaffold''s claimed transitional character would be increasingly nominal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_instrumentality_vs_intrinsic_value, empirical, 'Whether study''s function within this reading remains instrumental or has drifted toward intrinsic/substitutive value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(sacr_tr_t0, projected).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 300, 0.13).
narrative_ontology:measurement_basis(sacr_tr_t300, projected).
narrative_ontology:measurement(sacr_tr_t700, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 700, 0.16).
narrative_ontology:measurement_basis(sacr_tr_t700, observed).
narrative_ontology:measurement(sacr_tr_t1100, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1100, 0.18).
narrative_ontology:measurement_basis(sacr_tr_t1100, observed).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1500, 0.19).
narrative_ontology:measurement_basis(sacr_tr_t1500, observed).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement_basis(sacr_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(sacr_be_t0, projected).
narrative_ontology:measurement(sacr_be_t300, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 300, 0.06).
narrative_ontology:measurement_basis(sacr_be_t300, projected).
narrative_ontology:measurement(sacr_be_t700, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 700, 0.07).
narrative_ontology:measurement_basis(sacr_be_t700, observed).
narrative_ontology:measurement(sacr_be_t1100, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1100, 0.07).
narrative_ontology:measurement_basis(sacr_be_t1100, observed).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement_basis(sacr_be_t1500, observed).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1900, 0.08).
narrative_ontology:measurement_basis(sacr_be_t1900, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__messianic_suspension_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__messianic_suspension_reading, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the natural-language label 'is Torah study of sacrifice law a fulfillment of the sacrificial mitzvah, and what is the current status of the obligation.' Each reading (messianic_suspension, performance_only, study_as_exercise, symbolic_archive) has a distinct beneficiary/victim structure and a distinct ε: this reading authors ε near zero because the obligation is held inert rather than violated; performance_only_reading would author higher resistance/accessibility_collapse tension since it denies study can ever substitute; study_as_exercise_reading would author a coordination function collapsing into the same activity this reading treats as merely instrumental; symbolic_archive_reading would author near-zero extractiveness for a structurally different reason (no binding legal claim at all, rather than a binding-but-suspended claim). All four are linked as siblings of sacrifice_obligation_kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
