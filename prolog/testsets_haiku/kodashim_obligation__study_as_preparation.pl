% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Obligation: Study as Preparation for Messianic Restoration
 *   domain: religious/legal/theological
 *
 * SUMMARY:
 *   After the destruction of the Temple in 70 CE, Torah law specified
 *   hundreds of commandments governing animal sacrifice and priestly
 *   ritual—laws that became impossible to perform for nearly 1,900 years and
 *   counting. The preparation reading frames this as: the law remains
 *   eternally binding and divinely commanded; study substitutes for and
 *   prepares toward performance; when cosmic restoration (messianic
 *   redemption, rebuilding of the Temple) occurs, the knowledge preserved
 *   through study will enable correct performance to resume. The beneficiary
 *   of the obligation is not the current generation but a future restoration;
 *   the current generation bears the cost. This reading is one of three
 *   competing interpretations of the same halakhic kernel—all three coexist
 *   in institutional discourse without final adjudication.
 *
 * KEY AGENTS:
 *   - Rabbinic authority structure: interprets and enforces the preparation frame across diaspora communities; treats study as legal substitution
 *   - Male Jewish community (obligated learners): bear time and attention cost under the preparation rationale; exit is identity-dissolving
 *   - Alternative reading communities: advocate study_as_archive or study_as_performance; marginalized within orthodox discourse
 *   - Halakhic philosophers: observe the logical gap between the law's performative structure and its deferred justification
 *   - Messianic future restoration: non-agent beneficiary whose existence is theologically posited, not empirically grounded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.28).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.15).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.28).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Obligation: Study as Preparation for Messianic Restoration").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious/legal/theological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, '64789a46-bb7c-417f-b708-0eb1fb245c42').
narrative_ontology:cs_kernel_codification('64789a46-bb7c-417f-b708-0eb1fb245c42', fixed_text).
narrative_ontology:cs_authority_grounding('64789a46-bb7c-417f-b708-0eb1fb245c42', lineage).
narrative_ontology:cs_interpretation_layer_present('64789a46-bb7c-417f-b708-0eb1fb245c42').
narrative_ontology:cs_reading_relation('64789a46-bb7c-417f-b708-0eb1fb245c42', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_reading_relation('64789a46-bb7c-417f-b708-0eb1fb245c42', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('64789a46-bb7c-417f-b708-0eb1fb245c42', foundational, divine_command_eternally_binding_independent_of_performance_possibility).
narrative_ontology:cs_axiom_status(divine_command_eternally_binding_independent_of_performance_possibility, holdable).
narrative_ontology:cs_axiom_grounding('64789a46-bb7c-417f-b708-0eb1fb245c42', divine_command_eternally_binding_independent_of_performance_possibility, deontological).
narrative_ontology:cs_axiom('64789a46-bb7c-417f-b708-0eb1fb245c42', foundational, messianic_restoration_will_occur_enabling_performance_resumption).
narrative_ontology:cs_axiom_status(messianic_restoration_will_occur_enabling_performance_resumption, holdable).
narrative_ontology:cs_axiom_grounding('64789a46-bb7c-417f-b708-0eb1fb245c42', messianic_restoration_will_occur_enabling_performance_resumption, empirically_contingent).
narrative_ontology:cs_axiom('64789a46-bb7c-417f-b708-0eb1fb245c42', secondary, study_preserves_technical_knowledge_for_future_correct_performance).
narrative_ontology:cs_axiom_status(study_preserves_technical_knowledge_for_future_correct_performance, holdable).
narrative_ontology:cs_axiom_grounding('64789a46-bb7c-417f-b708-0eb1fb245c42', study_preserves_technical_knowledge_for_future_correct_performance, instrumental).
narrative_ontology:cs_reference_frame('64789a46-bb7c-417f-b708-0eb1fb245c42', eternal_divine_command_binding).
narrative_ontology:cs_drift_state('64789a46-bb7c-417f-b708-0eb1fb245c42', post_diaspora_era_contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('64789a46-bb7c-417f-b708-0eb1fb245c42', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_redemption_cosmos).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, future_temple_restoration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, male_jewish_community_current_generation).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, divine_command_binding_independent_of_performance).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, torah_temporality_spans_exile_and_return).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the interpretive framework treating sacrificial law as eternally binding and unperformable, study as legal substitution and cosmic preparation. Resolves conflicts between literal law and post-Temple reality through the preparation reading. Administers the obligation within institutional communities and defends the reading against competing framings (archive, performance).
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, rabbinic_authority_structure, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Obligated to study kodashim law daily or at regular intervals; recites liturgical references to Temple sacrifice; commits time and intellectual effort to mastering technical details of procedures they cannot and will not perform. Exit from the obligation is identity-dissolving within traditional communities—abandoning the study means abandoning observant Jewish practice. The benefit of their study (correct performance in messianic future) will not accrue to them but to a future generation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, male_jewish_community_current_generation, payer,
    organized, biographical, identity_locked, global).

% The textual tradition preserves technical knowledge about sacrificial procedures, Temple architecture, priestly regulations, and ritual purity. This knowledge is maintained in libraries, schools, and oral transmission but produces no functional output in the present era. Preservation is a side effect of the preparation obligation, not its declared purpose within this reading.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, knowledge_preservation_ecosystem, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_preparation, knowledge_preservation_ecosystem).

% The theological claim that messianic restoration will rebuild the Temple and require correct performance of sacrificial law. The beneficiary is a cosmic eventuality, not a collective agent. The preparation reading asserts this future is necessary (divinely ordained) and that study in the present will have preserved the competency needed then.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, messianic_restoration_cosmos, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_preparation, messianic_restoration_cosmos).

% Analyzes the logical and theological coherence of the preparation reading against alternatives. Questions whether the obligation can remain binding when unperformable, whether the future benefit justifies current cost, and whether knowledge preservation actually requires 1,900 years of continuous obligation. Produces meta-level analysis of the constraint rather than participating in it.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, halakhic_philosophers_contemporary, observer,
    institutional, biographical, analytical, global).

% Argues for study_as_archive (historical preservation, genealogical identity, not preparation) or study_as_performance (study itself constitutes cosmic sacrifice, no future restoration needed). Marginalized within ultra-Orthodox institutions despite having textual arguments and tradition-internal support in Conservative and Reform Judaism. Their objections to the preparation reading are heard but not treated as authoritative by the agenda-setting rabbinic structure.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, alternative_reading_advocates, excluded,
    organized, biographical, constrained, regional).

% Do not accept the preparation reading's theological premises (messianic restoration, binding divine command) and may participate in the obligation through cultural identity rather than doctrinal commitment. From this seat, the obligation appears extractive of time for a cosmological premise they reject. Their voices are excluded from halakhic authority structures by definition.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, secular_jewish_intellectuals, observer,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_preparation, secular_jewish_intellectuals, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_preparation, diffuse).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_preparation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a continuous chain of technical knowledge transmission across diaspora centuries, ensuring that if sacrificial performance becomes cosmically possible again, the procedural competency to do it correctly will have been preserved through unbroken textual study and interpretation.
% TRANSFER_FUNCTION: Transfers intellectual effort, study time, and cognitive labor from the current obligated generation to an indefinite future that may receive the benefit (if restoration occurs) or may never arrive. The transfer is to a collective future actor (restored Jewish state/Temple) whose probability of existence is theologically asserted but empirically uncertain.
% ABSENT_VOICES: Non-Jewish comparative religions scholars would attest that knowledge loss is inevitable in multi-century diaspora conditions (see Vedic sacrifice degradation, Zoroastrian ritual loss). Heterodox Jewish movements (Conservative, Reform, Reconstructionist) that reject the preparation reading entirely are present in the broader discourse but excluded from the ultra-Orthodox institutional authority that sets the preparation frame. Future generations who would inherit the obligation are by definition absent from the conversation about whether the present generation should bear it.
% DISAPPEARANCE_RATIONALE: If the obligation to study kodashim disappeared, the current generation would be liberated from a time-intensive duty with no contemporary functional output. The constraint's disappearance is contested because: (1) orthodox theology asserts the law remains divinely binding regardless of practical consequences, abandonment would constitute covenant breach; (2) knowledge loss would be irrecoverable if restoration ever occurred; (3) the preparation reading's justification cannot be empirically validated, so its abandonment cannot be proven wrong; (4) alternative readings (archive, performance) would claim the same or greater importance. The world would not physically rearrange, but the theological status of the obligated community would shift—either validating one of the alternative readings or creating a new relationship to covenant obligation.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, Torah law specified hundreds of commandments governing animal sacrifice—laws that became impossible to physically perform for the foreseeable future and indefinitely thereafter. The founding problem: how can a divine commandment remain binding and obligatory when it is structurally unperformable?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested in rabbinic literature (Talmud, Mishna) and medieval halakhic responses across all major Jewish traditions (Ashkenazi, Sephardi, Mizrahi). The preparation reading specifically is corroborated by medieval authorities (Maimonides, Tosafists) who cite messianic restoration as the justification for maintaining the law's binding status. However, this corroboration is internal to the beneficiary frame (authorities who uphold the preparation reading). External corroboration from non-halakhic sources: historians of religion (E.P. Sanders, Shaye Cohen, Jacob Neusner) document that rabbinic Judaism responded to Temple destruction by reframing sacrifice as textual study, but they do not resolve whether the law remained binding. Comparative evidence from other diaspora religions (Zoroastrians after Islamic conquest, Hindu Vedic practitioners in modern diaspora) shows similar patterns of obligation preservation without resolution of whether binding status persists. The founding problem itself remains live and unresolved because the three readings coexist without mutual foreclosure.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).
:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.28 at interval end) because the constraint has minimal coercive structure and no identifiable institutional beneficiary collecting rents in the present. The obligation flows from divine command and community identity, not from coercive apparatus. However, extractiveness is NOT negligible (not a mountain) because: (1) the current generation bears a genuine cost (time, intellectual labor) for a benefit they will not experience, (2) the beneficiary (messianic future) is contingent on a theological event, (3) the preparation reading's justification cannot be empirically validated—it rests on an assertion about a future that may never arrive. Suppression is very LOW (0.15 at interval end) because the constraint operates through voluntary participation in a tradition, not through coercive machinery. Resistance rises and falls with historical periods of diaspora intensity and communal coherence. Theater_ratio is stable and moderate (0.20–0.24): some portion of the study practice is genuine knowledge preservation (functional), some portion is performative assertion of unbroken tradition despite unperformability (theatrical). The measurement series show extractiveness declining slowly over the medieval period as the preparation frame was consolidated into doctrine, then rising slightly in the modern period as skepticism about messianic restoration increased among secular-adjacent communities. Suppression shows similar decay as enforcement mechanisms weakened outside tightly-controlled halakhic institutions.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat experiences unperformable obligation and deferred benefit; the agenda-setter seat experiences divine command and preparation for restoration; the observer seat measures the gap between current cost and future (uncertain) benefit. The engine should compute rope from the identity-locked seats (coordination function: knowledge preservation; voluntary participation in tradition; beneficiary is collective future), but should flag in contention analysis that the break of identity-lock would expose extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The obligated community is the structural target of the constraint (bearers of cost); d is modulated downward toward beneficiary because identity_locked exit dominates the power/exit calculation—abandoning the obligation dissolves the agent's identity and community membership, making exit impossibly costly. The rabbinic authority structure sits at the origin (neither collects nor pays in material terms) but is the constraint's administrator and its principal defender. Messianic restoration is not an agent and carries no d—it is a non-actor beneficiary that appears in the 'vindicated_propositions' array instead. The analytical observer seat has d ≈ 0.5 (symmetric: neither collecting nor constrained, but potentially beneficiary of preserved knowledge if restoration occurs).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how does binding law remain binding when unperformable?) is LIVE, not dead, because messianic restoration is a live theological proposition in orthodox Judaism, even if empirically unverifiable. The preparation reading avoids mandatrophy verdict by asserting that the cosmic eventuality (restoration) will restore the law's performability—the mandate has not died, only been deferred. However, the reading is vulnerable to mandatrophy reclassification if: (1) the messianic restoration becomes universally treated as symbolic rather than literal (would shift to study_as_performance or study_as_archive readings), or (2) the current generation's study obligation is empirically shown to be insufficient for knowledge preservation across the actual span of exile (would undermine the instrumental preparation justification). The preparation frame explicitly avoids mandatrophy by anchoring the constraint's continued necessity to a future event; it is vulnerable to mandatrophy detection if that event is reconceptualized as impossible or metaphorical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_restoration_contingency,
    'Is the preparation reading''s justification empirically undermined if messianic restoration becomes universally reinterpreted as metaphorical or symbolic rather than literal rebuilding?',
    'Historical analysis of shifts in Orthodox vs. Conservative vs. Reform theological commitments to literal restoration; textual analysis of how contemporary halakhic authorities describe the relationship between study and actual restoration.',
    'If restoration is reframed as purely spiritual/symbolic, the instrumental preparation justification evaporates and the constraint becomes mandatrophic—study cannot prepare for a non-literal event. The reading would collapse into study_as_performance or study_as_archive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_restoration_contingency, empirical, 'Whether the preparation reading''s coherence depends on literal messianic restoration as an empirical future event.').

omega_variable(
    identity_lock_rupture_dynamics,
    'For obligated community members whose identity-lock with Jewish tradition has weakened (secular Jews, critical scholars, diaspora communities with low institutional cohesion), does the constraint recalculate as extractive snare rather than low-extraction rope?',
    'Comparative study of how different Jewish communities (ultra-Orthodox vs. secular vs. cultural-identity vs. diaspora intellectuals) experience the obligation''s burden. Measurement of identity-lock strength via exit-cost analysis (what does abandoning the obligation cost in community standing, self-concept, belonging?).',
    'If identity-lock is weaker for substantial subsets of the obligated population, the constraint''s actual directionality diverges sharply from the calculation. Some seats would measure as snare (high extraction, weak exit, identity-lock broken), others as rope (voluntary, identity-constituted). The constraint would be genuinely contested across the obligated population, not abstractly contested across different reading traditions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_rupture_dynamics, empirical, 'Whether identity-lock erosion produces seat-level classification divergence within the obligated community.').

omega_variable(
    knowledge_preservation_sufficiency,
    'Over nearly 1,900 years of diaspora, has textual study actually preserved the technical knowledge required for sacrificial performance, or has knowledge degradation occurred such that the law''s performability would require reconstruction beyond what study preserved?',
    'Comparative analysis of technical complexity in other abandoned religious systems (Vedic sacrifice, Mesopotamian temple ritual) where knowledge preservation was attempted through study vs. where practice was abandoned. Textual analysis of halakhic literature to identify which technical details have been forgotten or become ambiguous across centuries.',
    'If knowledge degradation is substantial, the instrumental preparation rationale (study preserves competency for future performance) becomes undermined. The constraint would persist but its justification would shift: no longer preparation toward known-performability, but rather speculative hope that restoration would include divine revelation to compensate for knowledge loss. This would weaken the preparation reading''s coherence relative to the archive and performance readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_preservation_sufficiency, empirical, 'Whether the preparation reading''s instrumental claim (study preserves knowledge) is empirically supported across the actual historical span of diaspora.').

omega_variable(
    cosmological_necessity_vs_theological_assertion,
    'Does the preparation reading treat messianic restoration as cosmologically NECESSARY (a structural feature of reality that will inevitably occur) or merely as theologically ASSERTED (a doctrinal claim that may not be actualized)?',
    'Textual analysis of rabbinic and medieval halakhic sources: do authorities describe restoration as inevitable divine action or as contingent theological hope? Analysis of how the preparation reading interacts with modern historical consciousness (where 1,900-year deferral has become empirically visible as an extremely long wait with no progress toward restoration).',
    'If restoration is treated as necessary/inevitable, the obligation is instrumentally justified and the constraint remains rope. If restoration is contingent/hoped-for, the constraint becomes extractive of current labor for an uncertain future—classification would shift toward snare. The distinction is the difference between ''study for something that will definitely happen'' vs. ''study in hope of something that probably won''t happen in our lifetimes''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmological_necessity_vs_theological_assertion, conceptual, 'Whether the preparation reading''s justification depends on treating messianic restoration as necessary or merely contingent.').

omega_variable(
    reading_foreclosure_impossibility,
    'Can the three readings of the kodashim_obligation kernel (archive, performance, preparation) coexist indefinitely without one foreclosing the others, or are there logical thresholds at which coexistence becomes impossible?',
    'Formal logical analysis of the premises: does asserting one reading''s core premise necessarily deny the others'' premises, or can all three be held simultaneously by different parties without contradiction? Historical analysis of whether the readings have been explicitly formalized as foreclosing or coexisting.',
    'If the readings are logically independent (coexist_with), the constraint-family structure holds indefinitely. If one reading logically forecloses another, the kernel has an inherent structure of competitive resolution that may eventually adjudicate toward one reading. This affects the long-term stability of the preparation reading''s claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_impossibility, conceptual, 'Whether the three readings of kodashim_obligation are logically independent or contain inherent foreclosure relationships.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_preparation, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t300, kodashim_obligation__study_as_preparation, theater_ratio, 300, 0.19).
narrative_ontology:measurement_basis(koda_tr_t300, observed).
narrative_ontology:measurement(koda_tr_t600, kodashim_obligation__study_as_preparation, theater_ratio, 600, 0.21).
narrative_ontology:measurement_basis(koda_tr_t600, observed).
narrative_ontology:measurement(koda_tr_t900, kodashim_obligation__study_as_preparation, theater_ratio, 900, 0.2).
narrative_ontology:measurement_basis(koda_tr_t900, observed).
narrative_ontology:measurement(koda_tr_t1200, kodashim_obligation__study_as_preparation, theater_ratio, 1200, 0.23).
narrative_ontology:measurement_basis(koda_tr_t1200, observed).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_preparation, theater_ratio, 1500, 0.24).
narrative_ontology:measurement_basis(koda_tr_t1500, observed).
narrative_ontology:measurement(koda_tr_t1900, kodashim_obligation__study_as_preparation, theater_ratio, 1900, 0.22).
narrative_ontology:measurement_basis(koda_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_preparation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t300, kodashim_obligation__study_as_preparation, base_extractiveness, 300, 0.32).
narrative_ontology:measurement_basis(koda_be_t300, observed).
narrative_ontology:measurement(koda_be_t600, kodashim_obligation__study_as_preparation, base_extractiveness, 600, 0.29).
narrative_ontology:measurement_basis(koda_be_t600, observed).
narrative_ontology:measurement(koda_be_t900, kodashim_obligation__study_as_preparation, base_extractiveness, 900, 0.27).
narrative_ontology:measurement_basis(koda_be_t900, observed).
narrative_ontology:measurement(koda_be_t1200, kodashim_obligation__study_as_preparation, base_extractiveness, 1200, 0.26).
narrative_ontology:measurement_basis(koda_be_t1200, observed).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_preparation, base_extractiveness, 1500, 0.25).
narrative_ontology:measurement_basis(koda_be_t1500, observed).
narrative_ontology:measurement(koda_be_t1900, kodashim_obligation__study_as_preparation, base_extractiveness, 1900, 0.28).
narrative_ontology:measurement_basis(koda_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_preparation, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(koda_su_t0, observed).
narrative_ontology:measurement(koda_su_t300, kodashim_obligation__study_as_preparation, suppression_requirement, 300, 0.18).
narrative_ontology:measurement_basis(koda_su_t300, observed).
narrative_ontology:measurement(koda_su_t600, kodashim_obligation__study_as_preparation, suppression_requirement, 600, 0.14).
narrative_ontology:measurement_basis(koda_su_t600, observed).
narrative_ontology:measurement(koda_su_t900, kodashim_obligation__study_as_preparation, suppression_requirement, 900, 0.13).
narrative_ontology:measurement_basis(koda_su_t900, observed).
narrative_ontology:measurement(koda_su_t1200, kodashim_obligation__study_as_preparation, suppression_requirement, 1200, 0.12).
narrative_ontology:measurement_basis(koda_su_t1200, observed).
narrative_ontology:measurement(koda_su_t1500, kodashim_obligation__study_as_preparation, suppression_requirement, 1500, 0.13).
narrative_ontology:measurement_basis(koda_su_t1500, observed).
narrative_ontology:measurement(koda_su_t1900, kodashim_obligation__study_as_preparation, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement_basis(koda_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, resource_allocation).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_preparation, 0.18).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_archive).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, diaspora_identity_preservation__textual_transmission).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, messianic_theology__divine_command_binding).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel. The kodashim_obligation kernel has three structurally distinct readings with different epsilon values and beneficiary structures: (1) study_as_archive (epsilon ~0.12, knowledge preservation, genealogical function, minimal extraction), (2) study_as_performance (epsilon ~0.25, spiritual efficacy through study, moderate extraction due to deferral from physical practice), (3) study_as_preparation (epsilon ~0.28, instrumental preparation for future restoration, low-moderate extraction due to contingent future benefit). All three coexist in halakhic discourse without final adjudication. This story constrains to reading #3 only. See sibling stories for readings #1 and #2.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_obligation__study_as_preparation, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
