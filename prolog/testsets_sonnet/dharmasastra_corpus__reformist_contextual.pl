% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra as Historically-Conditioned Ethical Core (Reformist-Contextual Reading)
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This story generates the reformist-contextual reading of the Dharmasastra
 *   kernel: the claim that the corpus's ethical core (dharma as righteous
 *   conduct) is separable from its time-bound, historically-conditioned caste
 *   and gender prescriptions, which can be reinterpreted as artifacts of
 *   specific social conditions rather than eternal revealed truth. This is
 *   one of three sibling readings of the same kernel (orthodox_literalist,
 *   abolitionist_rejection are separate constraint stories, not part of this
 *   one). The reformist reading reduces the victim set relative to the
 *   literalist reading (hierarchy is softened/reinterpreted rather than
 *   mandated) but does not eliminate extraction: symbolic status
 *   differentials, occupational stigma, and gender-role retention persist
 *   under a re-narrated ethical vocabulary. Extraction is measured as
 *   declining over the interval as the interpretive move gains institutional
 *   traction and enforcement of strict prescriptions recedes, while
 *   theater_ratio rises as the gap widens between the reformist framing
 *   (ethical universalism) and the persisting, unaddressed material hierarchy
 *   the abolitionist reading identifies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.42).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.38).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.42).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra as Historically-Conditioned Ethical Core (Reformist-Contextual Reading)").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, '04d401ca-4691-4836-b74f-9cddc9973936').
narrative_ontology:cs_kernel_codification('04d401ca-4691-4836-b74f-9cddc9973936', fixed_text).
narrative_ontology:cs_authority_grounding('04d401ca-4691-4836-b74f-9cddc9973936', lineage).
narrative_ontology:cs_interpretation_layer_present('04d401ca-4691-4836-b74f-9cddc9973936').
narrative_ontology:cs_reading_relation('04d401ca-4691-4836-b74f-9cddc9973936', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('04d401ca-4691-4836-b74f-9cddc9973936', dharmasastra_corpus__abolitionist_rejection, influences).
narrative_ontology:cs_axiom('04d401ca-4691-4836-b74f-9cddc9973936', foundational, ethical_core_severable_from_social_prescription).
narrative_ontology:cs_axiom_status(ethical_core_severable_from_social_prescription, holdable).
narrative_ontology:cs_axiom_grounding('04d401ca-4691-4836-b74f-9cddc9973936', ethical_core_severable_from_social_prescription, conventional).
narrative_ontology:cs_axiom('04d401ca-4691-4836-b74f-9cddc9973936', foundational, caste_prescriptions_are_historically_contingent).
narrative_ontology:cs_axiom_status(caste_prescriptions_are_historically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('04d401ca-4691-4836-b74f-9cddc9973936', caste_prescriptions_are_historically_contingent, empirically_contingent).
narrative_ontology:cs_reference_frame('04d401ca-4691-4836-b74f-9cddc9973936', classical_smriti_social_order).
narrative_ontology:cs_drift_state('04d401ca-4691-4836-b74f-9cddc9973936', post_colonial_reform_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('04d401ca-4691-4836-b74f-9cddc9973936', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_religious_authorities).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, upper_caste_communities_retaining_symbolic_status).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, hindu_institutions_seeking_modern_legitimacy).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, lower_caste_communities_under_softened_hierarchy).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, dalit_reform_advocates_seeking_full_repudiation).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, women_under_reinterpreted_but_retained_gender_norms).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, dharma_as_universal_ethical_conduct).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, textual_tradition_retains_moral_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach and publish interpretations that separate dharma's ethical core from caste-specific prescriptions, framing varna as originally about aptitude or spiritual stage rather than birth. They retain institutional standing as custodians of a reformed but still-authoritative tradition, collecting deference and legitimacy from both traditionalist and modernizing audiences.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_religious_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from a reading that preserves ritual and symbolic precedence associated with historically dominant varna categories while shedding the reading's most defensible liabilities. They can present themselves as reformed while retaining much of the social capital the hierarchy conferred.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, upper_caste_communities_retaining_symbolic_status, beneficiary,
    organized, generational, mobile, national).

% Temples, seminaries, and diaspora organizations adopt the contextualist reading to present the tradition as compatible with human rights norms and pluralist democracy, protecting the textual corpus's authority and their own standing as its interpreters against wholesale rejection.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, hindu_institutions_seeking_modern_legitimacy, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, hindu_institutions_seeking_modern_legitimacy, agenda_setter).

% Experience hierarchy re-described as spiritual stage or historical artifact rather than eliminated in practice — endogamy, occupational stigma, and temple-access friction persist in attenuated form even as the textual rationale is softened. Exit means leaving the interpretive community entirely, which carries social and familial cost.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, lower_caste_communities_under_softened_hierarchy, payer,
    moderate, biographical, constrained, regional).

% Argue that any retained textual authority relegitimizes the same corpus that encoded their historical subordination, and that the contextualist move launders the tradition rather than repairing harm. Their position is acknowledged in academic and activist discourse but rarely incorporated into the reformist institutions' own self-description.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, dalit_reform_advocates_seeking_full_repudiation, excluded,
    organized, generational, constrained, national).

% Gender-restrictive prescriptions (on ritual participation, inheritance framing, marital duty) are reread as culturally contingent but frequently retained in practice under a rebranded ethical vocabulary. They bear the persisting asymmetry while the textual justification is presented as reformed.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, women_under_reinterpreted_but_retained_gender_norms, payer,
    moderate, biographical, constrained, regional).

% Study how the corpus's historical layers were composed under specific socio-political conditions and evaluate whether the ethical-core/caste-prescription separation is textually supportable or a modern retrofit. Their findings feed both reformist and abolitionist arguments without institutional stake in either outcome.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__reformist_contextual, reformist_religious_authorities).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__reformist_contextual, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for retaining continuity with a long textual and ritual tradition — shared ethical vocabulary, communal identity, and institutional structures for religious practice — while disavowing the tradition's most indefensible historical prescriptions.
% TRANSFER_FUNCTION: Moves social legitimacy and interpretive authority to reformist custodians of the tradition, while moving the practical burden of unresolved hierarchy onto lower-caste communities and women whose material conditions are reframed rather than materially changed.
% ABSENT_VOICES: Dalit reform advocates seeking full repudiation of the corpus's authority are largely outside the reformist institutions' own deliberative processes; their objection — that separating 'ethical core' from 'caste prescription' still re-legitimizes the source text — is engaged in academic literature but rarely shapes institutional doctrine.
% DISAPPEARANCE_RATIONALE: If the reformist-contextual reading disappeared, reformist institutions and the communities that identify with a modernized tradition would need to either adopt orthodox literalism or abolitionist rejection — both a live rearrangement for institutional legitimacy and communal identity. Whether the underlying social hierarchy would rearrange with it is disputed: abolitionists argue the hierarchy persists materially regardless of which textual reading prevails, while reformists argue the reading itself constitutes meaningful change.
% FOUNDING_PROBLEM: The corpus emerged to codify righteous conduct (dharma) and social order within specific historical polities; the reformist-contextual reading was built later, in the 19th–20th centuries, to reconcile a text carrying caste and gender hierarchy with emerging norms of equality and human rights, preserving the tradition's authority under modern conditions.
% FOUNDING_PROBLEM_CORROBORATION: Reformist authorities and diaspora institutions attest the ethical-core separation succeeds and the founding problem (reconciling tradition with modern equality) is substantially resolved. Independent historians of religion and Dalit scholars, outside the reformist institutions, corroborate that the textual separation is interpretively defensible in parts but contest that it resolves lived caste and gender asymmetry, which they document as persisting through non-textual social enforcement independent of which reading is preached.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, contested).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).
:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate-high (0.58) reflecting a still-substantial caste and gender residue at the reading's historical origin, and declines to 0.42 as strict enforcement of caste prescriptions recedes under the reformist framing's social influence — but does not approach zero because status differentials and gendered role expectations persist in attenuated form. Theater ratio rises over the same interval (0.30 to 0.45) because an increasing share of the reading's public presentation is ethical-universalist rhetoric that outpaces the actual dismantling of hierarchy-linked practice — the gap between claimed reform and material change widens even as raw extraction falls. Suppression declines modestly (0.50 to 0.38) as strict textual enforcement is replaced by softer social and institutional pressure to accept the reformed reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist authorities and modernizing institutions are structural beneficiaries: they retain interpretive authority and legitimacy while shedding the tradition's most exposed liabilities, so they sit near the beneficiary end of directionality. Upper-caste communities retaining symbolic status are secondary beneficiaries — coordination gains without the burden of the underlying inequality. Lower-caste communities and women bear the persisting, re-narrated asymmetry with constrained exit (leaving the interpretive community carries real social cost), placing them toward the target end. Dalit reform advocates are excluded rather than coordinated or paid — their objection that the reading re-legitimizes rather than repairs is the excluded voice the six-questions interview is built to surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The reformist-contextual reading is a live attempt to resolve mandatrophy: it explicitly argues the original founding problem (codifying righteous conduct within a historical social order) can be separated from the now-obsolete caste-hierarchy machinery, allowing the ethical core to persist while the extractive scaffolding is retired. Whether this resolution succeeds or merely re-labels the same extraction under updated vocabulary is exactly the omega this story cannot settle internally — it is the central point of contest with the abolitionist sibling reading, which holds that no separation is possible because the hierarchy was never merely contingent packaging around the ethics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethical_core_separability,
    'Is the dharma-as-righteous-conduct ethical core textually and historically separable from the varna/jati caste prescriptions, or are they so interwoven in the source texts that any ''separation'' is a modern interpretive imposition rather than a recovery of original meaning?',
    'Philological and historical-critical analysis of textual strata across the Dharmasastra corpus (Manusmriti, Yajnavalkya Smriti, etc.) to determine whether caste prescriptions are structurally load-bearing for the ethical framework or genuinely severable later accretions tied to specific historical polities.',
    'If separable, the reformist reading has genuine textual warrant and functions closer to authentic recovery of an ethical core; if inseparable, the reformist reading is a constructed retrofit that uses the appearance of textual fidelity to launder continued institutional authority — pushing this constraint''s computed type toward snare rather than tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethical_core_separability, conceptual, 'Whether the reformist ethical-core/caste-prescription separation is textually supportable or a modern retrofit.').

omega_variable(
    reformist_reading_kernel_contest,
    'Among the three declared readings of the dharmasastra_corpus kernel (orthodox_literalist, reformist_contextual, abolitionist_rejection), does the reformist reading''s persistence create structural pressure that forecloses or merely delays the abolitionist reading''s uptake by giving the tradition continued institutional cover?',
    'Track institutional and legal outcomes (temple-access litigation, caste-based discrimination case law, religious-institution governance reform) to see whether reformist framing precedes and enables abolitionist-style legal remedies, or substitutes for them and suppresses their uptake.',
    'If the reformist reading functions as a pressure-release valve that delays structural remedy, its effective extraction is understated by this story''s declining epsilon trajectory; if it genuinely opens the door to further reform, the declining trajectory is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_reading_kernel_contest, empirical, 'Whether the reformist reading enables or forestalls the abolitionist reading''s material remedies.').

omega_variable(
    material_change_vs_rhetorical_change,
    'Does the declining base_extractiveness trajectory authored in this story reflect genuine reduction in caste- and gender-linked material harm, or does it reflect only a shift in rhetorical framing (rising theater_ratio) while lived-practice hierarchy persists at similar levels?',
    'Longitudinal sociological data on intermarriage rates, occupational mobility, temple-access incidents, and gendered ritual participation across the same interval, compared against the rhetorical shift in reformist institutional publications.',
    'If material change lags rhetorical change substantially, the true extraction trajectory is flatter than authored here and the theater_ratio rise documented is closer to pure substitution (Goodhart drift) than genuine improvement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_change_vs_rhetorical_change, empirical, 'Whether declining extraction reflects real change or rhetorical substitution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__reformist_contextual, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dhar_tr_t10, dharmasastra_corpus__reformist_contextual, theater_ratio, 10, 0.33).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__reformist_contextual, theater_ratio, 20, 0.36).
narrative_ontology:measurement(dhar_tr_t30, dharmasastra_corpus__reformist_contextual, theater_ratio, 30, 0.39).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__reformist_contextual, theater_ratio, 40, 0.42).
narrative_ontology:measurement(dhar_tr_t50, dharmasastra_corpus__reformist_contextual, theater_ratio, 50, 0.44).
narrative_ontology:measurement(dhar_tr_t60, dharmasastra_corpus__reformist_contextual, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__reformist_contextual, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(dhar_be_t10, dharmasastra_corpus__reformist_contextual, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__reformist_contextual, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(dhar_be_t30, dharmasastra_corpus__reformist_contextual, base_extractiveness, 30, 0.46).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__reformist_contextual, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(dhar_be_t50, dharmasastra_corpus__reformist_contextual, base_extractiveness, 50, 0.43).
narrative_ontology:measurement(dhar_be_t60, dharmasastra_corpus__reformist_contextual, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__reformist_contextual, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(dhar_su_t10, dharmasastra_corpus__reformist_contextual, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__reformist_contextual, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(dhar_su_t30, dharmasastra_corpus__reformist_contextual, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__reformist_contextual, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(dhar_su_t50, dharmasastra_corpus__reformist_contextual, suppression_requirement, 50, 0.39).
narrative_ontology:measurement(dhar_su_t60, dharmasastra_corpus__reformist_contextual, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__reformist_contextual, 0.1).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the dharmasastra_corpus kernel. orthodox_literalist claims the prescriptions are eternal revealed truth (full victim set, high epsilon, high suppression, claimed_type likely tangled_rope-to-snare). abolitionist_rejection claims no legitimate authority survives the corpus's historical role in caste oppression (total victim framing, authority itself rejected). This reformist_contextual reading claims the ethical core is separable and reduces the victim set and extraction relative to the literalist reading while retaining medium extraction and a coordination/extraction hybrid structure — hence tangled_rope rather than rope or snare. Each story carries its own stable epsilon; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
