% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra as Historically-Conditioned Text with Separable Ethical Core
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint models the reformist-contextual reading of the contested
 *   Dharmasastra kernel: the claim that the textual corpus's ethical core
 *   (dharma as righteous conduct, truthfulness, non-violence) is separable
 *   from time-bound caste and gender prescriptions that reflect the
 *   historical conditions of its composition rather than eternal revealed
 *   truth. This reading is held chiefly by modern reform-minded religious
 *   authorities, neo-Vedantic institutions, and moderate caste communities
 *   seeking to retain scriptural legitimacy while disavowing untouchability
 *   and rigid varna hierarchy. It is one of three structurally distinct
 *   constraints instantiated from the same kernel: orthodox_literalist (which
 *   reads the hierarchy as eternal and binding) and abolitionist_rejection
 *   (which reads the entire framework, ethical core included, as
 *   constitutively compromised by its origin in hierarchy). Extraction here
 *   is measured as medium (0.42) because the reading softens but does not
 *   eliminate hierarchy's practical residue — caste distinction persists
 *   symbolically, reframed as 'spiritual stages' or 'historical accretion,'
 *   while local enforcement of caste practice continues largely unabated.
 *   This is decisively lower than what an orthodox_literalist story would
 *   author (hierarchy fully vindicated) and higher than what an
 *   abolitionist_rejection story would author for the surviving arrangement
 *   (near-total rejection leaves near-zero residual extraction under this
 *   reading's own framework, since it denies the arrangement any legitimate
 *   continuation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.42).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.38).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.42).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra as Historically-Conditioned Text with Separable Ethical Core").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, '2b620140-87d7-4c7c-b096-521488746a58').
narrative_ontology:cs_kernel_codification('2b620140-87d7-4c7c-b096-521488746a58', fixed_text).
narrative_ontology:cs_authority_grounding('2b620140-87d7-4c7c-b096-521488746a58', lineage).
narrative_ontology:cs_interpretation_layer_present('2b620140-87d7-4c7c-b096-521488746a58').
narrative_ontology:cs_reading_relation('2b620140-87d7-4c7c-b096-521488746a58', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('2b620140-87d7-4c7c-b096-521488746a58', dharmasastra_corpus__abolitionist_rejection, influences).
narrative_ontology:cs_axiom('2b620140-87d7-4c7c-b096-521488746a58', foundational, dharma_ethical_core_severable_from_social_prescription).
narrative_ontology:cs_axiom_status(dharma_ethical_core_severable_from_social_prescription, holdable).
narrative_ontology:cs_axiom_grounding('2b620140-87d7-4c7c-b096-521488746a58', dharma_ethical_core_severable_from_social_prescription, conventional).
narrative_ontology:cs_axiom('2b620140-87d7-4c7c-b096-521488746a58', foundational, textual_layers_reflect_historical_composition_not_timeless_revelation).
narrative_ontology:cs_axiom_status(textual_layers_reflect_historical_composition_not_timeless_revelation, holdable).
narrative_ontology:cs_axiom_grounding('2b620140-87d7-4c7c-b096-521488746a58', textual_layers_reflect_historical_composition_not_timeless_revelation, empirically_contingent).
narrative_ontology:cs_reference_frame('2b620140-87d7-4c7c-b096-521488746a58', classical_brahmanical_dharma_synthesis).
narrative_ontology:cs_drift_state('2b620140-87d7-4c7c-b096-521488746a58', post_independence_constitutional_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('2b620140-87d7-4c7c-b096-521488746a58', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_religious_authorities).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, moderate_caste_communities).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, modern_hindu_institutions_seeking_legitimacy).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, lower_caste_reform_advocates).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, dalit_communities_seeking_full_repudiation).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, women_under_residual_gendered_prescriptions).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, dharma_as_righteous_conduct_doctrine).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, textual_layered_composition_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Dharmasastra corpus as a historically layered text, extracting the 'ethical core' (dharma as righteous conduct, non-violence, truthfulness) while recasting caste prescriptions as time-bound social arrangements rather than eternal revelation. This preserves their institutional authority to speak for the tradition, allows them to retain congregational legitimacy across caste lines, and shields the textual corpus itself from wholesale rejection. They control which passages are foregrounded as 'core' and which are historicized away.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_religious_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, reformist_religious_authorities, beneficiary).

% Middle and upper-middle caste groups who benefit from a reading that lets them retain social and ritual status markers (family lineage claims, temple access, marriage norms) reframed as 'cultural heritage' rather than divinely mandated hierarchy — insulating inherited privilege from direct moral challenge while appearing to embrace reform.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, moderate_caste_communities, beneficiary,
    organized, generational, mobile, national).

% Advocate for concrete redistribution of ritual, marital, and economic access rights. Under the reformist-contextual reading, their claims are acknowledged as historically valid grievances but the practical hierarchy persists in softened, 'spiritualized' form (varna as stages of moral development rather than birth-rank) — a reframing that costs them the clean rejection the abolitionist reading would offer, while not delivering the material equality the reframing claims to enable.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, lower_caste_reform_advocates, payer,
    moderate, biographical, constrained, national).

% Bear the lived weight of caste practice regardless of how the text is reinterpreted at the seminary or academy level; local enforcement of untouchability practices, endogamy norms, and occupational restriction persists in villages even as national religious authorities declare the hierarchy 'merely historical.' The reformist reading gives them partial rhetorical cover but no binding mechanism forcing local practice to follow the reinterpretation.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, dalit_communities_seeking_full_repudiation, payer,
    powerless, generational, trapped, national).

% Gendered prescriptions in the corpus (regarding widowhood, inheritance, ritual eligibility) are frequently exempted from the 'time-bound and separable' historicizing move applied to caste, or reinterpreted more slowly and unevenly; the reformist reading's selective historicization leaves them bearing costs the reading's own logic could in principle also relieve but in practice does not prioritize.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, women_under_residual_gendered_prescriptions, payer,
    powerless, biographical, constrained, regional).

% Diaspora temples, reform movements, and apologetic institutions use the reformist-contextual reading to present the tradition internationally as ethically coherent and modern, defusing external criticism of caste while retaining the prestige and continuity of scriptural lineage.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, modern_hindu_institutions_seeking_legitimacy, beneficiary,
    institutional, civilizational, arbitrage, global).

% Hold that the reformist reading is itself a capitulation and a distortion of revealed prescription; they are structurally excluded from the reformist coalition's framing even though they claim continuity with the same textual corpus, because the reformist reading's legitimacy depends on rejecting their literalism as historically naive.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_literalist_authorities, excluded,
    organized, civilizational, trapped, national).

% Study the corpus's composite, multi-author, multi-era composition and can independently assess whether the 'ethical core vs. time-bound prescription' distinction is a defensible textual finding or a retrospectively imposed apologetic device.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, academic_textual_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a way for a large, textually-grounded religious tradition to retain internal continuity and external legitimacy while disavowing its most morally indefensible historical prescriptions — allowing adherents across caste lines to remain within a single interpretive community rather than fracturing entirely.
% TRANSFER_FUNCTION: Moves moral and reputational cost away from the textual corpus and its custodial authorities and onto reform advocates, who must accept a partial, negotiated repudiation instead of the clean rejection they sought; simultaneously moves legitimacy and continuity toward reformist authorities and institutions who get to retain custodianship of the tradition.
% ABSENT_VOICES: Dalit and lower-caste communities living under continuing local caste enforcement have little voice in which passages get designated 'core' versus 'historical accretion' — that interpretive labor is performed largely by upper-caste-descended scholars and clergy who retain institutional standing. Orthodox literalists are excluded from the reformist coalition's legitimacy claim entirely, framed as having misunderstood their own tradition.
% DISAPPEARANCE_RATIONALE: Reformist authorities and moderate institutions would say the tradition's continuity and moral credibility depend on this interpretive move persisting; abolitionist and orthodox critics would each say (for opposite reasons) that its disappearance would simply reveal what was already true beneath it — either the corpus's irredeemable hierarchy (abolitionist) or its uncorrupted eternal validity (orthodox). The disagreement itself is evidence the reading performs real interpretive work rather than being cosmetic.
% FOUNDING_PROBLEM: The reformist-contextual reading was constructed to solve the problem of a scriptural tradition whose historical prescriptions had become morally and politically indefensible (especially post-independence India's constitutional rejection of untouchability) without abandoning the tradition's authority structure or textual lineage altogether.
% FOUNDING_PROBLEM_CORROBORATION: Reformist theologians (Vivekananda-lineage neo-Vedanta, Arya Samaj-descended movements) attest the ethical-core/historical-accretion distinction is textually sound and the problem of reconciling tradition with modern ethics remains live and legitimately addressed by this move. Independent academic Indologists (outside both the reformist and orthodox camps) are divided: some corroborate that the corpus is demonstrably composite and layered across centuries, supporting separability in principle; others, including several Dalit scholars and historians of caste practice, attest that the 'ethical core' framing has functioned historically to defer rather than resolve the founding problem, since local caste enforcement has continued largely unaffected by the reinterpretation.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, contested).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction (0.42) reflects a hierarchy that persists in attenuated, symbolic form: reformist authorities retain interpretive and institutional authority (low-d beneficiary position), while communities seeking full repudiation bear ongoing costs the reinterpretation promises to address but does not mechanically enforce redress for. Suppression (0.38) is moderate-declining over the interval, reflecting a gradual loosening of the texts' coercive social force as constitutional and legal structures (particularly in post-independence India) increasingly displace scriptural authority as the operative enforcement mechanism — suppression here is substantially legal/social rather than purely textual. Theater ratio rises over the interval (0.3 to 0.5) because as material caste practice persists locally, an increasing share of the reformist apparatus's activity becomes the performative labor of reinterpretation and public disavowal rather than the delivery of concrete redistribution — the theater_ratio trajectory captures a Goodhart-style drift where the reinterpretive act substitutes for material change.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist agenda-setter seat, this reading is coordination: it holds a fracturing tradition together and delivers real, if partial, moral progress. From the payer seats (lower-caste and Dalit advocates, gendered-prescription-bound women), the same structure functions as extraction dressed in the language of reform — a mechanism for retaining scriptural and institutional authority while deferring the redistribution the reform rhetoric implies. The engine's per-seat computation should register this divergence directly from the declared power/exit/beneficiary-victim structure, not from any authored resolution of which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist authorities and moderate caste communities sit near the beneficiary end: they retain institutional legitimacy, continuity of tradition, and social status while incurring only reputational adjustment costs. Lower-caste and Dalit communities sit near the target end: they bear the practical costs of hierarchy's residual operation while receiving symbolic acknowledgment rather than structural remedy. Women under gendered prescriptions occupy a similarly targeted position, compounded by the reading's uneven application of its own historicizing logic across caste versus gender prescriptions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling scriptural authority with modern ethical rejection of untouchability) is contested as live versus dead: reformist authorities treat it as an ongoing, successfully-managed project; critics both to the left (abolitionist) and structurally excluded (Dalit advocates for full repudiation) treat the arrangement as having drifted into a legitimacy-preservation exercise that no longer tracks its founding justification. This is exactly the mismatch the R5 corroboration field is designed to surface: status=contested paired with disappearance_verdict=contested, rather than a clean mandatrophy resolution either way, reflects genuine, unresolved structural contest rather than an easily-flagged zombie arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    separability_of_ethical_core_from_caste_prescription,
    'Is the distinction between dharma-as-ethical-core and caste-as-time-bound-prescription a textually defensible finding about the corpus''s composite, multi-era authorship, or a retrospectively imposed apologetic device that reads modern ethical commitments back into an ancient text?',
    'Comparative philological analysis of the corpus''s compositional layers (dating strata, internal contradictions, regional variation) cross-checked against whether the proposed ''core'' passages are demonstrably earlier, more widely attested, or structurally privileged within the text independent of modern ethical preference.',
    'If the separability is textually well-founded, this reading has stronger claim to genuine coordination (preserving what is textually central while historicizing what is textually peripheral). If the separability tracks modern preference more than textual structure, the reading functions closer to a legitimacy-preservation device, and the tangled_rope classification would tilt further toward extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_ethical_core_from_caste_prescription, conceptual, 'Whether the ethical-core/caste-prescription separability is a textual finding or a retrospective apologetic construction.').

omega_variable(
    material_effect_of_reinterpretation,
    'Does the reformist-contextual reading''s adoption by national religious authorities produce measurable change in local caste practice, or does it operate entirely at the level of elite theological and institutional discourse while local enforcement continues unaffected?',
    'Longitudinal sociological data on caste-based discrimination, endogamy rates, and untouchability practice in regions/communities where reformist teaching has been actively promoted versus regions where it has not.',
    'If material practice shifts meaningfully, the coordination function is real and substantial; if practice is unaffected regardless of doctrinal reinterpretation, the theater_ratio trajectory authored here understates the degree to which the reading is purely discursive, and the constraint would sit closer to a piton (theatrical maintenance of a claim divorced from operative function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_effect_of_reinterpretation, empirical, 'Whether reformist reinterpretation produces measurable material change or remains elite discourse.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the disagreement between this reading and its siblings (orthodox_literalist, abolitionist_rejection) locate structurally — is it a disagreement about the FACTS of textual composition (layered vs. unified authorship), about the ETHICAL STATUS of hierarchy (legitimate vs. illegitimate), or about the STRATEGIC QUESTION of whether reform-from-within or rejection is more likely to reduce material harm?',
    'Decompose the kernel dispute into its factual, ethical, and strategic components and assess each sibling reading''s actual point of departure — reformist_contextual accepts factual layering (shared with some orthodox scholarship) but rejects orthodox''s ethical claim of eternal validity, while diverging from abolitionist_rejection on the strategic question of whether the tradition''s authority structure can be repurposed rather than must be abandoned.',
    'Locating the disagreement precisely determines whether reformist_contextual and orthodox_literalist could in principle be reconciled by textual-historical evidence (a factual dispute) or are irreducibly value-divergent (an ethical dispute) — this bears on whether cs_structure.reading_relations should treat the orthodox relationship as more foreclosing than currently authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Where the reformist/orthodox/abolitionist disagreement is structurally located: fact, ethics, or strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__reformist_contextual, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__reformist_contextual, theater_ratio, 20, 0.35).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__reformist_contextual, theater_ratio, 40, 0.4).
narrative_ontology:measurement(dhar_tr_t60, dharmasastra_corpus__reformist_contextual, theater_ratio, 60, 0.44).
narrative_ontology:measurement(dhar_tr_t80, dharmasastra_corpus__reformist_contextual, theater_ratio, 80, 0.47).
narrative_ontology:measurement(dhar_tr_t100, dharmasastra_corpus__reformist_contextual, theater_ratio, 100, 0.5).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__reformist_contextual, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__reformist_contextual, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__reformist_contextual, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(dhar_be_t60, dharmasastra_corpus__reformist_contextual, base_extractiveness, 60, 0.46).
narrative_ontology:measurement(dhar_be_t80, dharmasastra_corpus__reformist_contextual, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(dhar_be_t100, dharmasastra_corpus__reformist_contextual, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__reformist_contextual, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__reformist_contextual, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__reformist_contextual, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(dhar_su_t60, dharmasastra_corpus__reformist_contextual, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(dhar_su_t80, dharmasastra_corpus__reformist_contextual, suppression_requirement, 80, 0.4).
narrative_ontology:measurement(dhar_su_t100, dharmasastra_corpus__reformist_contextual, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'Dharmasastra and caste' concept per the ε-invariance principle: orthodox_literalist claims near-total vindication of hierarchy (high ε from the reform/abolitionist vantage, near-zero from its own); abolitionist_rejection claims the entire framework is illegitimate (ε near-total for the standing arrangement, by its own lights); reformist_contextual (this story) claims medium ε — hierarchy softened but not eliminated, textual authority partially preserved. Each story authors its own ε for the SAME standing arrangement (current Dharmasastra-derived social practice) as seen through its own reading's lights, per the kernel-reading ε referent rule. All three are linked bidirectionally via affects_constraints to preserve the constraint-family topology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
