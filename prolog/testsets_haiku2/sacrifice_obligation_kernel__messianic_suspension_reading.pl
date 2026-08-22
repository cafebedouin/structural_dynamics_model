% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Sacrifice Obligation Messianic Suspension Reading
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   The sacrifice obligation kernel contains a contested claim: what is the
 *   halakhic status of the obligation to perform sacrifice during the
 *   historical period when the Temple is not standing? The messianic
 *   suspension reading holds that the obligation is divinely suspended
 *   (paused, not violated) until messianic restoration occurs. During this
 *   suspension, study of sacrifice law maintains the operational readiness
 *   necessary for performance when restoration comes. This reading contrasts
 *   with: (1) performance-only reading (study does not fulfill the mitzvah;
 *   the community remains in violation), (2) study-as-exercise reading (study
 *   itself constitutes the exercise of the mitzvah), and (3) symbolic-archive
 *   reading (study preserves cultural memory but makes no halakhic claim).
 *   The suspension reading is low-extractiveness because the obligation is in
 *   abeyance; no party is compelled to perform an impossible act, and no
 *   victim set emerges during the suspension period. The beneficiary is
 *   future generations who will inherit a preserved knowledge tradition at
 *   restoration. Study practitioners shoulder the transmission obligation,
 *   but the reading frames this as preserving capacity, not substituting for
 *   performance.
 *
 * KEY AGENTS:
 *   - Study practitioners: rabbinic scholars and communities maintaining sacrifice law study as ongoing practice
 *   - Future generations at restoration: the anticipated beneficiaries who will inherit operational knowledge and the restored obligation
 *   - Rabbinic authority establishment: institutional seats that adjudicate and enforce the suspension reading
 *   - Performance-reading advocates: excluded by the suspension frame; hold that study does not fulfill the mitzvah
 *   - Theological interpreters: observers who study how the reading manages institutional continuity through historical rupture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.18).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Sacrifice Obligation Messianic Suspension Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority").

narrative_ontology:has_sunset_clause(sacrifice_obligation_kernel__messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, '6d3782f1-dd16-4f38-84fd-49f043d95791').
narrative_ontology:cs_kernel_codification('6d3782f1-dd16-4f38-84fd-49f043d95791', fixed_text).
narrative_ontology:cs_authority_grounding('6d3782f1-dd16-4f38-84fd-49f043d95791', lineage).
narrative_ontology:cs_interpretation_layer_present('6d3782f1-dd16-4f38-84fd-49f043d95791').
narrative_ontology:cs_reading_relation('6d3782f1-dd16-4f38-84fd-49f043d95791', sacrifice_obligation_kernel__study_as_exercise_reading, influences).
narrative_ontology:cs_reading_relation('6d3782f1-dd16-4f38-84fd-49f043d95791', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('6d3782f1-dd16-4f38-84fd-49f043d95791', sacrifice_obligation_kernel__symbolic_archive_reading, influences).
narrative_ontology:cs_axiom('6d3782f1-dd16-4f38-84fd-49f043d95791', foundational, obligation_divinely_suspended_not_transformed).
narrative_ontology:cs_axiom_status(obligation_divinely_suspended_not_transformed, holdable).
narrative_ontology:cs_axiom_grounding('6d3782f1-dd16-4f38-84fd-49f043d95791', obligation_divinely_suspended_not_transformed, deontological).
narrative_ontology:cs_axiom('6d3782f1-dd16-4f38-84fd-49f043d95791', foundational, study_maintains_operational_readiness).
narrative_ontology:cs_axiom_status(study_maintains_operational_readiness, holdable).
narrative_ontology:cs_axiom_grounding('6d3782f1-dd16-4f38-84fd-49f043d95791', study_maintains_operational_readiness, instrumental).
narrative_ontology:cs_reference_frame('6d3782f1-dd16-4f38-84fd-49f043d95791', temple_functional_sacrifice_obligation_state).
narrative_ontology:cs_drift_state('6d3782f1-dd16-4f38-84fd-49f043d95791', contemporary_post_temple_destruction_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6d3782f1-dd16-4f38-84fd-49f043d95791', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_at_restoration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, rabbinic_authority_establishment).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, divinely_authorized_suspension).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, continuity_through_interruption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic scholars and communities that maintain the practice of studying sacrifice law during the period of suspension. They interpret and transmit the textual tradition, argue interpretive questions, and teach the next generation. They do not perform sacrifice but hold that their study activity preserves operational readiness for the anticipated restoration. Their exit from study would amount to abandoning the obligation itself during the suspension period.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, study_practitioners, agenda_setter,
    organized, generational, constrained, global).

% Will inherit the obligation to perform sacrifice when messianic restoration occurs. Their ability to perform will depend on whether the knowledge tradition was maintained during the suspension. They are not present to contest or negotiate; their interest is structural — the reading preserves their capacity to discharge the obligation they will inherit.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_at_restoration, beneficiary,
    powerless, civilizational, analytical, universal).

% Formalized authority structure (rabbinic courts, academy leadership) that adjudicates and enforces the suspension reading and the study obligation. They benefit from maintaining authority over halakhic interpretation; they also benefit from the reading's provision that the obligation is suspended (not violated) — this protects community from mandatrophy claims that study is an inadequate substitute.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, rabbinic_authority_establishment, agenda_setter,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, rabbinic_authority_establishment, beneficiary).

% Communities or scholars who hold that sacrifice obligation requires physical performance and cannot be met through study alone. They are excluded from this reading's framing because the suspension reading pre-empts performance claims by declaring the obligation divinely paused, not transformable. Their objection — 'study does not fulfill the mitzvah' — is incompatible with the suspension frame but remains a live position in the broader kernel contest.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, performance_reading_advocates, excluded,
    moderate, generational, constrained, regional).

% Comparative-religion scholars and historical theologians who study how religious traditions handle suspended or deferred obligations. They observe the suspension reading as one mechanism for maintaining continuity through institutional disruption, compare it to analogous structures in other traditions, and examine the empirical trajectory of the reading's authority.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, theological_interpreters, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the knowledge and interpretive tradition necessary for sacrifice performance at a future restoration, during a present period when performance is impossible or prohibited. Solves the problem: 'How do we maintain a complex, embodied practice across a historical rupture where the practice cannot be performed?'
% TRANSFER_FUNCTION: Transfers interpretive and pedagogical responsibility from the present generation to the next, and stores operational knowledge (sacrifice law details, interpretive methods, textual analysis) in written and oral transmission for use at an indefinite future moment of restoration.
% ABSENT_VOICES: Communities that have abandoned the sacrifice tradition entirely (secular populations, competing theological readings) are structurally absent from this reading's framing. Performance-requirement advocates are explicitly excluded by the suspension frame — their objection is not that the reading is wrong but that it transforms the obligation rather than suspending it. Voices arguing that the restoration will never occur are absent as well.
% DISAPPEARANCE_RATIONALE: If the suspension reading disappeared and were replaced by the performance-only reading, communities would face mandatrophy (the obligation cannot be performed, so the reading would declare ongoing violation). If replaced by study-as-exercise, the obligation would be deemed satisfied by study alone (a different reading, not disappearance). The contestation is real: the suspension reading's disappearance would force a choice among the sibling readings; the world does not rearrange independently.
% FOUNDING_PROBLEM: The destruction of the Temple ended sacrifice performance; the rabbis needed to preserve the obligation and the knowledge tradition for the anticipated messianic restoration without either abandoning the mitzvah or declaring the community in permanent violation.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic texts and halakhic authorities from the Talmudic period onward attest this as the founding problem. Historians and theologians outside the benefiting parties confirm that the suspension reading arose directly in response to the Temple destruction and the need for institutional continuity. The founding problem status is live because the reading remains in active use and the messianic restoration has not occurred.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Base extractiveness is low (0.18) because the reading operates in a domain where the primary obligation (sacrifice performance) is suspended, not transformed. There is no coerced transfer: study practitioners engage in study as a continuity practice, not under external extraction. The cost of study is borne by practitioners voluntarily; no victim set bears the cost of the reading itself. Theater ratio is moderate (0.22) because study carries both genuine instrumental value (maintaining knowledge for restoration) and performative value (demonstrating continuity and fidelity to the tradition). As centuries pass without restoration, the ratio may drift slightly upward — the instrumental justification weakens as the restoration recedes, but the performative justification (maintaining identity and group cohesion) intensifies. Suppression is minimal (0.12) because the reading does not suppress alternatives through coercion; it frames an authoritative interpretation that is then defended through institutional authority, not through active suppression of dissent. Performance-reading advocates remain in the conversation; they are not silenced, merely positioned as a different reading. Accessibility collapse is moderate (0.65) because once the suspension frame is established and internalized, alternative readings become harder to adopt — the theological infrastructure supporting the frame is substantial — but practitioners can still contest the reading or adopt siblings without formal barriers.
 *
 * PERSPECTIVAL GAP:
 *   Study practitioners see the reading as providing permission to maintain a sacred practice and knowledge tradition without incurring mandatrophy or violation. Rabbinic authorities see the reading as resolving a crisis of obligation-preservation through a theologically sound mechanism. Performance advocates see the reading as evacuating the substance of the obligation through a merely semantic suspension. Theological observers see the reading as a case study in how authority structures manage institutional rupture. The engine computes the perspective gaps from the power and exit positions: practitioners and authorities have medium power and constrained exit (they are embedded in the tradition); advocates have less power and are excluded; observers have analytical distance. Each seat perceives different extraction and suppression dynamics, but the claim and metrics remain constant across seats — the reading itself is low-extractiveness because the obligation is in abeyance, not transformed.
 *
 * DIRECTIONALITY LOGIC:
 *   The study practitioners occupy a symmetric position (d near 0.5): they benefit from the reading's provision that study maintains the obligation (rather than violates it), but they also bear the ongoing cost of teaching and preserving the tradition. The rabbinic authority establishment occupies a complex position: they benefit from being the institutional adjudicators of the reading (d weighted toward beneficiary), but they also bear the administrative and interpretive load of maintaining the framework (d weighted back toward symmetric). Future generations occupy a beneficiary position (d near 0.0): they inherit a preserved knowledge tradition with no cost during their non-existent period; when restoration comes, their cost shifts upward as they assume the obligation itself, but during the suspension that is a future state. Performance-reading advocates are excluded rather than targeted; the reading does not extract from them, but it does foreclose their interpretive frame. No agent sits in a pure target position — the structure is not extractive in the conventional sense because the obligation itself is suspended. The low directionality scores reflect this: no powerful agent is targeting a powerless agent for extraction; rather, an institutional authority is adjudicating an obligation structure, and the main distributional question is whether the burden of preservation falls justly on present practitioners.
 *
 * MANDATROPHY ANALYSIS:
 *   The suspension reading directly resolves mandatrophy by declaring the obligation divinely suspended (paused) rather than violated. The founding problem (the Temple is destroyed; sacrifice cannot be performed) would generate mandatrophy for any reading that claims the obligation is still binding on present performance. The suspension reading avoids this by locating the obligation in a future state (restoration) and redefining the present obligation as maintenance of readiness. Study becomes instrumental to this future obligation, not a substitute for present performance. This framing prevents the reading from collapsing into snare (where the obligation would be unperformable and enforced anyway) or rope (where study would be offered as a genuine alternative). The scaffold classification is appropriate because the reading explicitly carries a sunset clause: when messianic restoration occurs, the suspension ends and the obligation to perform returns. The interim obligation (to study) is justified entirely by the anticipated transition. The theater ratio's slow drift upward over centuries reflects the growing tension between the instrumental justification (preparing for restoration that may not come) and the performative justification (maintaining identity and continuity). At some critical point (unmeasured in this interval, projected beyond 2000), if restoration does not occur, the reading would face pressure to transform into the study-as-exercise reading (study constitutes the mitzvah) or the symbolic-archive reading (study is cultural memory without halakhic claim). That pressure is captured in the omega variables, not in the base metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restoration_contingency_assumption,
    'Does the suspension reading depend on a factual assumption that restoration will occur, or is it valid independent of restoration outcomes?',
    'If restoration occurs, the reading is vindicated; if centuries pass with no restoration, the reading faces pressure to transform into the study-as-exercise reading or to be reframed as symbolic-archive. The resolution point is empirical (will restoration occur) combined with normative (if not, what does the reading become).',
    'If the reading depends on an assumption of eventual restoration, the constraint is conditionally valid — it transforms into a different type if the assumption fails. If the reading is independent (study maintains the obligation valid-regardless-of-restoration), the type shifts from scaffold to rope, and the beneficiary shifts from future-generations to present-practitioners.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_contingency_assumption, empirical, 'Whether the suspension reading''s validity depends on messianic restoration occurring.').

omega_variable(
    study_instrumentality_drift,
    'As centuries pass without restoration, does study remain instrumental (maintaining capacity for future performance) or does it gradually become performative (maintaining identity and group cohesion)?',
    'Historical-textual analysis of how rabbis and communities justify study obligation over time. Early justifications emphasize readiness for restoration; later justifications increasingly emphasize identity and continuity. The drift point is where the performative justification overtakes the instrumental justification in institutional discourse.',
    'If drift is substantial and acknowledged, the theater ratio should increase more sharply (performative work is now the primary function, not secondary). If the drift is denied or suppressed, the theater ratio remains moderate and the reading approaches mandatrophy (instrumental justification weakens but the obligation is not formally transformed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_instrumentality_drift, empirical, 'Whether study''s function shifts from instrumental (readiness) to performative (identity maintenance) as restoration recedes.').

omega_variable(
    suspension_vs_transformation_boundary,
    'Is there a halakhic or theological distinction between suspension (the obligation paused, awaiting restoration) and transformation (the obligation modified into study)?',
    'Textual and doctrinal analysis of how the reading and its siblings are argued in rabbinic and medieval sources. If texts consistently distinguish suspension from transformation, the boundary is real; if the distinction blurs in practice, suspension may be performative framing for transformation.',
    'If the boundary is real and maintained, the suspension reading remains scaffold (low extractiveness, sunset clause, instrumental interim obligation). If the boundary blurs, the reading drifts toward study-as-exercise (moderate extractiveness, no sunset, study itself becomes the obligation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_vs_transformation_boundary, conceptual, 'Whether suspension (paused obligation) and transformation (modified obligation) are halakhically/theologically distinguishable or functionally equivalent.').

omega_variable(
    reading_framing_exclusion_mechanism,
    'Does the suspension reading''s authority exclude performance-reading advocates through logical foreclosure, or through institutional power?',
    'If performance advocates can hold their reading while acknowledging the validity of the suspension reading (both true in different authority frameworks), the exclusion is institutional. If the readings are logically incompatible within any single framework, the exclusion is foreclosure.',
    'If exclusion is foreclosure, the reading_relations should record forecloses. If institutional, the relation is coexists_with (different parties hold different readings; neither logically rules out the other). The impact on classification is minimal but important for understanding how the reading maintains authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_exclusion_mechanism, conceptual, 'Whether the suspension reading logically forecloses the performance-only reading or merely displaces it institutionally.').

omega_variable(
    future_generations_beneficiary_asymmetry,
    'Is the beneficiary (future generations at restoration) a real beneficiary of the constraint''s operation, or is naming future generations a narrative device to justify present study?',
    'If restoration occurs and future generations inherit a preserved knowledge tradition, the beneficiary designation is confirmed empirically. If restoration does not occur, the naming becomes purely performative — future generations do not exist at the constraint''s termination point, so calling them beneficiaries is narratively true but structurally empty.',
    'If the beneficiary is narrative-only, the constraint has no real beneficiary during the suspension period — only present practitioners bearing the transmission burden. The classification would shift from scaffold (justified by future benefit) to piton (maintained by institutional inertia and identity performance). This is the key contingency differentiating the reading''s validity from its viability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_beneficiary_asymmetry, empirical, 'Whether naming future generations as beneficiary is empirically grounded or narratively constructed to justify present transmission burden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t250, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 250, 0.19).
narrative_ontology:measurement_basis(sacr_tr_t250, observed).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 500, 0.2).
narrative_ontology:measurement_basis(sacr_tr_t500, observed).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1000, 0.22).
narrative_ontology:measurement_basis(sacr_tr_t1000, observed).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1500, 0.23).
narrative_ontology:measurement_basis(sacr_tr_t1500, observed).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(sacr_tr_t2000, projected).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t250, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 250, 0.16).
narrative_ontology:measurement_basis(sacr_be_t250, observed).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 500, 0.17).
narrative_ontology:measurement_basis(sacr_be_t500, observed).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1000, 0.18).
narrative_ontology:measurement_basis(sacr_be_t1000, observed).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1500, 0.19).
narrative_ontology:measurement_basis(sacr_be_t1500, observed).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement_basis(sacr_be_t2000, projected).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(sacr_su_t0, observed).
narrative_ontology:measurement(sacr_su_t250, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 250, 0.09).
narrative_ontology:measurement_basis(sacr_su_t250, observed).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 500, 0.1).
narrative_ontology:measurement_basis(sacr_su_t500, observed).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1000, 0.12).
narrative_ontology:measurement_basis(sacr_su_t1000, observed).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1500, 0.11).
narrative_ontology:measurement_basis(sacr_su_t1500, observed).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement_basis(sacr_su_t2000, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_kernel constraint family consists of four readings, each instantiating a different constraint from the same contested kernel (the status of the obligation during Temple absence). The messianic_suspension_reading (this constraint) has low extractiveness because the obligation is suspended; it influences the study_as_exercise_reading (which makes the same obligation satisfy through study) and coexists_with the performance_only_reading (which holds study is preparatory but not fulfilling). All four readings share the same referent (the standing sacrifice obligation during Temple absence) but diverge in their structural consequences (ranging from zero extractiveness at symbolic-archive to mandatrophy at performance-only if restoration never occurs).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
