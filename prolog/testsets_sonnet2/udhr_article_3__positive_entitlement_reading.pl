% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: Article 3 as Positive Entitlement to Material Security (Welfare State Reading)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This story authors the positive-entitlement reading of UDHR Article 3 as
 *   a single, ε-invariant constraint: under this reading, 'security of
 *   person' is read to require the state to provide the material
 *   preconditions of life — welfare, healthcare, housing — via taxation,
 *   mandates, and (in some jurisdictions) associated restrictions on property
 *   and certain expression framed as threats to dignity/security. This is one
 *   reading among three of a contested kernel; the negative-liberty reading
 *   (state violence prohibition only) and the procedural-hybrid reading
 *   (due-process guarantees without resolving the substantive contest) are
 *   separate constraints, not alternative measurements of this one. The ε
 *   authored here (0.61 at interval end) is intrinsic to THIS reading's
 *   operation — the redistributive and speech-restrictive apparatus it
 *   licenses — not an average across readings.
 *
 * KEY AGENTS:
 *   - low_income_households: primary beneficiary (powerless/trapped) — receives entitlement transfers
 *   - the_state: agenda_setter (institutional/analytical) — administers taxation, mandates, enforcement
 *   - high_net_worth_taxpayers: primary payer (powerful/constrained) — funds transfers
 *   - private_property_owners: payer (moderate/constrained) — bears property curtailment
 *   - speech_restricted_dissenters: payer (powerless/trapped) — bears expressive cost
 *   - constitutional_courts: agenda_setter/observer (institutional/analytical) — constructs the reading's reach
 *   - negative_liberty_advocates: excluded (organized/constrained) — contests the reading's legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.61).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.52).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "Article 3 as Positive Entitlement to Material Security (Welfare State Reading)").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, 'a5974e1c-8af3-487d-8ba9-3bd881de9b54').
narrative_ontology:cs_kernel_codification('a5974e1c-8af3-487d-8ba9-3bd881de9b54', fixed_text).
narrative_ontology:cs_authority_grounding('a5974e1c-8af3-487d-8ba9-3bd881de9b54', distributed).
narrative_ontology:cs_reading_relation('a5974e1c-8af3-487d-8ba9-3bd881de9b54', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5974e1c-8af3-487d-8ba9-3bd881de9b54', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('a5974e1c-8af3-487d-8ba9-3bd881de9b54', foundational, material_deprivation_constitutes_insecurity).
narrative_ontology:cs_axiom_status(material_deprivation_constitutes_insecurity, holdable).
narrative_ontology:cs_axiom_grounding('a5974e1c-8af3-487d-8ba9-3bd881de9b54', material_deprivation_constitutes_insecurity, deontological).
narrative_ontology:cs_axiom('a5974e1c-8af3-487d-8ba9-3bd881de9b54', foundational, state_has_affirmative_provision_duty).
narrative_ontology:cs_axiom_status(state_has_affirmative_provision_duty, holdable).
narrative_ontology:cs_axiom_grounding('a5974e1c-8af3-487d-8ba9-3bd881de9b54', state_has_affirmative_provision_duty, conventional).
narrative_ontology:cs_reference_frame('a5974e1c-8af3-487d-8ba9-3bd881de9b54', postwar_atrocity_prevention_charter).
narrative_ontology:cs_drift_state('a5974e1c-8af3-487d-8ba9-3bd881de9b54', contemporary_welfare_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a5974e1c-8af3-487d-8ba9-3bd881de9b54', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, low_income_households).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, uninsured_populations).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, housing_insecure_residents).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, disability_dependent_populations).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, high_net_worth_taxpayers).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, private_property_owners).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, employers_subject_to_mandates).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, speech_restricted_dissenters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive welfare transfers, subsidized healthcare, and public housing allocations justified as constitutional entitlements flowing from the right to security. Depend on the state apparatus continuing to fund and administer these programs; have no exit from needing them and little power over how they are structured.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, low_income_households, beneficiary,
    powerless, biographical, trapped, national).

% Gain access to mandated healthcare coverage or public health systems on the theory that healthcare is a life-security entitlement. Their access is contingent on continued political and fiscal commitment to the reading; a change in judicial interpretation could withdraw the guarantee.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, uninsured_populations, beneficiary,
    powerless, biographical, trapped, national).

% Benefit from public housing mandates and tenant protections derived from the entitlement reading. Their situation is materially improved but the improvement is administratively fragile and dependent on continued state capacity.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, housing_insecure_residents, beneficiary,
    powerless, biographical, trapped, national).

% Administers the tax-and-transfer apparatus, healthcare mandates, housing programs, and the enforcement machinery (courts, regulatory agencies, tax authorities) that gives the entitlement reading legal force. Sets policy, adjudicates disputes, and bears no personal cost from extraction — it channels resources rather than surrendering them.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, the_state, agenda_setter,
    institutional, generational, analytical, national).

% Fund the material entitlements through progressive taxation justified by the positive-rights reading of Article 3. Can lobby, litigate, or relocate assets/domicile at real but nontrivial cost; cannot avoid the underlying tax obligation without exit from the jurisdiction entirely.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, high_net_worth_taxpayers, payer,
    powerful, biographical, constrained, national).

% Face rent controls, eminent domain for public housing, and land-use mandates justified as necessary to realize the housing component of the entitlement. Their property rights are curtailed in service of a constitutional welfare claim they did not consent to as a limiting principle on ownership.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, private_property_owners, payer,
    moderate, biographical, constrained, national).

% Bear healthcare mandate costs, minimum-provision requirements, and payroll levies that fund the entitlement structure. Larger employers can absorb or pass through costs; smaller employers experience the mandate as a binding constraint on hiring and margins.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, employers_subject_to_mandates, payer,
    powerful, biographical, constrained, national).

% Face hate-speech and dignitary-harm restrictions justified under the same security-of-the-person logic that grounds the material entitlements — the reading treats certain expression as itself a threat to life/security warranting suppression. They experience this as a direct cost of the constitutional reading with no comparable material offset.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, speech_restricted_dissenters, payer,
    powerless, biographical, trapped, national).

% Adjudicate the scope of the entitlement reading, deciding case by case how far Article 3 extends into welfare, healthcare, and housing policy, and how far it licenses restriction of property and speech. Their rulings both interpret and actively construct the reading's practical reach.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__positive_entitlement_reading, constitutional_courts, agenda_setter).

% Hold that Article 3 only prohibits state violence and arbitrary deprivation, not that it mandates redistribution or speech restriction. Their reading is treated in this constraint's operation as a defeated or marginalized alternative rather than a live co-equal interpretation; they contest the entitlement reading's legitimacy from outside the coalition that benefits from it.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, negative_liberty_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__positive_entitlement_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_article_3__positive_entitlement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools resources through taxation and mandates to guarantee a floor of material security — healthcare, housing, subsistence — for those who cannot secure it through markets alone, on the theory that security of the person is meaningless without security of material conditions.
% TRANSFER_FUNCTION: Moves tax revenue, mandated employer contributions, and curtailed property rights from higher-wealth and property-holding actors to low-income, uninsured, and housing-insecure populations, mediated by state administration; also moves expressive latitude from dissenting speakers to the state's security-of-dignity enforcement apparatus.
% ABSENT_VOICES: Negative-liberty advocates and strict proceduralists are structurally treated as having lost the interpretive contest within this reading's operation — their objection that Article 3 was drafted as a shield against state violence, not a sword for redistribution, is not represented in the entitlement apparatus's own justificatory account of itself.
% DISAPPEARANCE_RATIONALE: If the positive-entitlement reading were abandoned overnight, welfare transfers, healthcare mandates, and public housing programs grounded in constitutional (rather than purely legislative) authority would lose their strongest legal foundation; courts would need to re-derive them from ordinary statute, taxpayers would gain standing to challenge redistribution as unconstrained legislative preference rather than constitutional obligation, and hate-speech restrictions grounded in security-of-dignity logic would lose a key doctrinal anchor.
% FOUNDING_PROBLEM: The drafters of the UDHR sought to prevent a repeat of state violence and the atrocities of WWII by securing 'life, liberty and security of person' against government power; the entitlement reading extends this into an affirmative duty to provide the material preconditions of a secure life, addressing mass poverty and deprivation as their own form of insecurity.
% FOUNDING_PROBLEM_CORROBORATION: Welfare-state constitutional scholars and social-rights NGOs attest the material-deprivation problem remains live and that Article 3 was always meant to encompass it. Negative-liberty legal historians and drafting-history researchers outside the beneficiary coalition attest the drafting record shows Article 3 was aimed principally at state violence (extrajudicial killing, arbitrary detention) and that the entitlement extension is a later interpretive layering not corroborated by the original travaux préparatoires.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects a substantial but not overwhelming transfer: taxation and mandates are compulsory and enforced, but recipients are genuinely made better off in a way that is not pure rent extraction — there is a real coordination function (pooling resources against destitution) riding alongside the transfer. Suppression (0.52) is moderate: the mechanism relies on tax enforcement and, in the speech-restriction component, on content-based limits that meet organized resistance from free-expression advocates and property-rights litigants — this is not a frictionless natural order. Accessibility collapse is comparatively low (0.4) because exit via political contestation, constitutional amendment, or judicial reinterpretation remains genuinely available, unlike a mountain where alternatives are foreclosed. Resistance is high (0.72) because taxpayers, property owners, and speech advocates actively litigate and lobby against the reading's expansions.
 *
 * PERSPECTIVAL GAP:
 *   From the state's and courts' seat, this reading looks like coordination: solving mass material deprivation as a security problem the negative-liberty reading leaves unaddressed. From the taxpayer, property-owner, and dissenting-speaker seats, the same structure looks like enforced extraction wearing a constitutional-rights justification. The engine's per-seat computation should register both: a beneficiary seat trending toward rope-like coordination, and a payer seat trending toward tangled-rope or snare-like extraction, from the identical structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income, uninsured, and housing-insecure populations are declared beneficiaries: the material entitlement structure exists to transfer resources and services to them, so their derived directionality sits near the full-beneficiary end despite their low nominal power (the entitlement, not their power, is what shields them from extraction under this reading). High-net-worth taxpayers, property owners, employers, and speech-restricted dissenters are declared victims/payers: resources and rights move from them to fund or accommodate the entitlement structure, placing their derived directionality near the full-target end, moderated somewhat by their real (if costly) exit options — relocation, restructuring, litigation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as contested rather than resolved: the entitlement reading's proponents hold the founding problem (mass deprivation as insecurity) is still live and the state apparatus is a live, necessary response; historians of the drafting record hold the original founding problem (state violence) has been substantially addressed by other instruments and that the entitlement extension is a distinct, later-grafted mandate whose own founding problem (poverty as constitutional violation) was never corroborated by the 1948 drafters' own stated intent. Classifying this as tangled_rope rather than snare or rope preserves both halves: a genuine coordination function (pooling against destitution) coexists with asymmetric extraction (compulsory transfer from a narrower payer class, backed by active enforcement) — collapsing it to either pole would mislabel either the real welfare gain or the real coercive cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drafting_intent_vs_extension,
    'Does the 1948 drafting history of Article 3 support reading ''security of person'' as encompassing affirmative material provision, or only protection from state violence?',
    'Systematic review of the travaux préparatoires and contemporaneous drafting committee debates; comparison with the separate, later-drafted economic and social rights articles (22-27) which explicitly address welfare, suggesting Article 3 was not intended to duplicate that function.',
    'If the drafting record clearly supports a violence-only reading, the positive-entitlement reading is a constructed extension rather than an interpretation, strengthening the case that this constraint''s coordination framing is cover for redistributive and speech-restrictive extraction; if the record is genuinely ambiguous or supports a broader reading, the coordination function is more substantively grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drafting_intent_vs_extension, empirical, 'Whether drafting history supports the positive-entitlement extension of Article 3.').

omega_variable(
    coordination_vs_redistribution_separability,
    'Is the material-security coordination function (pooling against destitution) separable from the specific mechanism of compulsory progressive taxation and property/speech restriction this reading authorizes, or does realizing the entitlement necessarily require those particular extractive mechanisms?',
    'Comparative study of welfare-state designs that achieve comparable material floors through less property-restrictive or speech-restrictive means (e.g., universal basic income funded by consumption taxes, versus means-tested transfers funded by wealth taxes and paired with hate-speech law).',
    'If separable, much of the measured extraction and suppression is contingent implementation choice rather than intrinsic to the entitlement reading, and a lower-ε implementation of the same reading would be a distinct constraint; if inseparable, the extraction is closer to structurally necessary to this reading as such.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_redistribution_separability, conceptual, 'Whether the entitlement reading''s coordination function requires its particular extractive mechanisms.').

omega_variable(
    kernel_disagreement_locus,
    'Where exactly do the three readings of Article 3 diverge — is it the scope of ''security'' (material vs. purely bodily), the identity of the duty-bearer (affirmative state duty vs. negative restraint), or the enforceability mechanism (justiciable entitlement vs. aspirational guidance)?',
    'Structural comparison of the three constraint stories'' beneficiary/victim sets and enforcement declarations against comparative constitutional case law (e.g., South African socio-economic rights jurisprudence vs. U.S. negative-rights jurisprudence vs. European Convention procedural rulings).',
    'Locating the disagreement precisely determines which axiom is doing the foreclosing work in cs_structure.axioms, and clarifies whether the three readings are genuinely incompatible (forecloses) or merely emphasize different aspects of the same text (coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_locus, conceptual, 'Precise structural location of the kernel disagreement among the three Article 3 readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__positive_entitlement_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(udhr_tr_t8, udhr_article_3__positive_entitlement_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(udhr_tr_t16, udhr_article_3__positive_entitlement_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(udhr_tr_t24, udhr_article_3__positive_entitlement_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(udhr_tr_t32, udhr_article_3__positive_entitlement_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(udhr_tr_t40, udhr_article_3__positive_entitlement_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__positive_entitlement_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(udhr_be_t8, udhr_article_3__positive_entitlement_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(udhr_be_t16, udhr_article_3__positive_entitlement_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(udhr_be_t24, udhr_article_3__positive_entitlement_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(udhr_be_t32, udhr_article_3__positive_entitlement_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(udhr_be_t40, udhr_article_3__positive_entitlement_reading, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__positive_entitlement_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(udhr_su_t8, udhr_article_3__positive_entitlement_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(udhr_su_t16, udhr_article_3__positive_entitlement_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(udhr_su_t24, udhr_article_3__positive_entitlement_reading, suppression_requirement, 24, 0.46).
narrative_ontology:measurement(udhr_su_t32, udhr_article_3__positive_entitlement_reading, suppression_requirement, 32, 0.49).
narrative_ontology:measurement(udhr_su_t40, udhr_article_3__positive_entitlement_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(udhr_article_3__positive_entitlement_reading, 0.12).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3_negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3_procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the udhr_article_3 kernel decomposed per the ε-invariance principle: negative_liberty_reading (state-violence prohibition only, low ε, mountain/rope-leaning), positive_entitlement_reading (this story — affirmative material provision, moderate-high ε, tangled_rope), and procedural_hybrid_reading (due-process guarantees, low-moderate ε, rope-leaning). Each reading has its own beneficiary/victim structure and its own stable ε; they are linked here rather than merged because measuring Article 3 by different observables (state-violence incidence vs. material deprivation rates vs. due-process compliance) yields structurally different constraints, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__positive_entitlement_reading, powerless, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
