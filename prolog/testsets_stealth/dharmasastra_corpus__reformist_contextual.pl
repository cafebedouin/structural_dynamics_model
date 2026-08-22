% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra Corpus — Reformist Contextual Reading
 *   domain: religious/normative-authority
 *
 * SUMMARY:
 *   This story instantiates the reformist contextual reading of the
 *   dharmasastra_corpus kernel: the claim that the corpus reflects historical
 *   social conditions and that dharma as righteous conduct is separable from
 *   time-bound caste prescriptions. The standing arrangement under contest —
 *   the referent for every metric — is the dharmasastra-grounded normative
 *   order as this reading sustains it: textual authority retained, caste
 *   prescriptions demoted to the historically contingent, hierarchy
 *   persisting symbolically where it once ruled in strict enforcement. The
 *   reading solves a real coordination problem (portable ethical identity
 *   across a vast, plural population) while transferring real goods
 *   (interpretive authority, ritual precedence, marriage-market position)
 *   along inherited lines — hence the tangled-rope claim. Per the family
 *   decomposition documented in network.dual_formulation_note, the sibling
 *   readings orthodox_literalist and abolitionist_rejection are separate
 *   constraints with their own epsilon values over the same referent; they
 *   are not folded into this story. The claim/metric gap is deliberate and
 *   uncorrected: the reading presents itself as the tradition's purification,
 *   while the authored metrics record a settlement whose declared equality
 *   increasingly outruns its practiced endogamy — the engine measures that
 *   divergence.
 *
 * KEY AGENTS:
 *   - reformist_acharya_lineage: Agenda setter and concentrated beneficiary (institutional/identity_locked) — administers the purified reading, collects the interpretive-authority premium
 *   - upper_caste_devotees: Primary beneficiary (powerful/constrained) — retain symbolic precedence and marriage-network position under the purified settlement
 *   - dalit_and_adivasi_communities: Primary target (powerless/constrained) — bear the residual hierarchy's costs: segregation, occupation inheritance, marriage-barrier stigma
 *   - shudra_caste_communities: Secondary target with partial benefit (moderate/constrained) — mid-ladder position, gained ritual access, pay continued deference
 *   - diaspora_hindu_communities: Secondary beneficiary (organized/constrained) — receive the portable purified tradition, import remnant norms
 *   - orthodox_literalist_establishments: Excluded rival authority (institutional/mobile) — hold the prescriptions eternally binding, contest from independent institutions
 *   - abolitionist_ambedkarite_movements: Excluded objectors (organized/mobile) — demand total repudiation, supply the pressure that keeps purification moving
 *   - academic_indologists: Analytical observer — supply the philological evidence on which separability stands or falls
 *   - indian_constitutional_state: Institutional observer — bounds what the settlement may enforce without interpreting doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.48).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.42).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.48).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra Corpus — Reformist Contextual Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious/normative-authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, 'da17837a-4c4e-44f6-b1d3-0d2de3793037').
narrative_ontology:cs_kernel_codification('da17837a-4c4e-44f6-b1d3-0d2de3793037', fixed_text).
narrative_ontology:cs_authority_grounding('da17837a-4c4e-44f6-b1d3-0d2de3793037', lineage).
narrative_ontology:cs_interpretation_layer_present('da17837a-4c4e-44f6-b1d3-0d2de3793037').
narrative_ontology:cs_reading_relation('da17837a-4c4e-44f6-b1d3-0d2de3793037', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('da17837a-4c4e-44f6-b1d3-0d2de3793037', dharmasastra_corpus__abolitionist_rejection, influences).
narrative_ontology:cs_axiom('da17837a-4c4e-44f6-b1d3-0d2de3793037', foundational, dharma_ethical_core_separable_from_varna_prescriptions).
narrative_ontology:cs_axiom_status(dharma_ethical_core_separable_from_varna_prescriptions, holdable).
narrative_ontology:cs_axiom_grounding('da17837a-4c4e-44f6-b1d3-0d2de3793037', dharma_ethical_core_separable_from_varna_prescriptions, empirically_contingent).
narrative_ontology:cs_axiom('da17837a-4c4e-44f6-b1d3-0d2de3793037', foundational, varna_prescriptions_time_bound_not_eternal).
narrative_ontology:cs_axiom_status(varna_prescriptions_time_bound_not_eternal, holdable).
narrative_ontology:cs_axiom_grounding('da17837a-4c4e-44f6-b1d3-0d2de3793037', varna_prescriptions_time_bound_not_eternal, empirically_contingent).
narrative_ontology:cs_axiom('da17837a-4c4e-44f6-b1d3-0d2de3793037', secondary, corpus_authority_survives_selective_suspension).
narrative_ontology:cs_axiom_status(corpus_authority_survives_selective_suspension, holdable).
narrative_ontology:cs_axiom_grounding('da17837a-4c4e-44f6-b1d3-0d2de3793037', corpus_authority_survives_selective_suspension, conventional).
narrative_ontology:cs_reference_frame('da17837a-4c4e-44f6-b1d3-0d2de3793037', contextual_dharma_authority).
narrative_ontology:cs_drift_state('da17837a-4c4e-44f6-b1d3-0d2de3793037', contemporary_post_ambedkarite_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da17837a-4c4e-44f6-b1d3-0d2de3793037', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, upper_caste_devotees).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_acharya_lineage).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, diaspora_hindu_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, dalit_and_adivasi_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, shudra_caste_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, shudra_caste_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches and administers the contextual reading: decides which textual strands bind present-day practice, trains teachers, runs trusts, ashrams, schools, and publishing houses, and answers both orthodox objections and anti-caste criticism on behalf of the tradition. Income, standing, and vocation flow from being the recognized interpreter of what counts as timeless and what counts as historical. Leaving the role would mean surrendering the calling and community position that constitute their life's work.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_acharya_lineage, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, reformist_acharya_lineage, beneficiary).

% Practice within reformist congregations while retaining inherited ritual precedence, surname capital, and marriage-network position. They publicly affirm the purified teaching and privately sustain endogamous matchmaking and festival hierarchies. Exit would mean forfeiting community standing accumulated over generations of precedence.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, upper_caste_devotees, beneficiary,
    powerful, biographical, constrained, national).

% Bear the residue the settlement leaves: village segregation, sanitation-caste occupation inheritance, temple-entry frictions, and marriage-barrier stigma, now defended less by scriptural citation than by custom and economic dependency. Constitutional law and reformist teaching formally welcome them; daily practice frequently does not. Conversion offers a way out but triggers family rupture, local boycott, and complications with scheduled-status protections.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, dalit_and_adivasi_communities, payer,
    powerless, generational, constrained, national).

% Occupy the middle of the residual ladder: above the ex-untouchable groups in ritual terms, below the twice-born in precedence and matrimonial exchange. They gained expanded ritual access from reform movements and pay in continued deference expectations and marriage restrictions.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, shudra_caste_communities, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, shudra_caste_communities, beneficiary).

% Receive a portable, presentable tradition suited to plural societies: ethical teaching, festival calendar, and identity without overt caste enforcement. They fund reformist institutions generously and simultaneously import remnant norms — caste-filtered matrimonial matching, guru hierarchies — into their new settings.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, diaspora_hindu_communities, beneficiary,
    organized, biographical, constrained, global).

% Maintain rival centers of textual authority — traditional teaching lines, mathas, hereditary priesthoods — that hold the prescriptions eternally binding. They are not seated in reformist adjudication; their objections are answered polemically rather than incorporated. Their independent institutions let them contest rather than comply.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_literalist_establishments, excluded,
    institutional, generational, mobile, national).

% Organize around total repudiation: public burnings of the Manu text, conversion campaigns, legislative politics, and literary assertion. They stand outside the reformist conversation — treated as tragic losses or political factions rather than co-interpreters — while their critique supplies much of the pressure that keeps reformist purification moving.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, abolitionist_ambedkarite_movements, excluded,
    organized, generational, mobile, national).

% Study the corpus's composition, transmission, and reception; produce critical editions and stratifications separating textual layers. Neither bound by the teaching nor funded by its institutions, they supply the philological evidence on which claims about what is separable stand or fall.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, academic_indologists, observer,
    analytical, civilizational, analytical, global).

% Abolished untouchability, legislates temple entry, administers reservation schedules, and prosecutes caste atrocities. It bounds what the religious settlement may enforce without itself interpreting doctrine.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, indian_constitutional_state, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__reformist_contextual, reformist_acharya_lineage).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__reformist_contextual, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The corpus, read contextually, coordinates Hindu religious life across regions and languages: a shared ethical vocabulary (dharma as righteous conduct), a common ritual calendar and lifecycle rites, and a portable identity — solved once through canonical texts rather than invented per community.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy to those who control the reading (the acharya class and its institutions); moves deference, ritual precedence, and marriage-market position along inherited caste lines even as formal discrimination recedes; and moves moral respectability to adherents who remain inside the tradition rather than exiting.
% ABSENT_VOICES: Two flanks are outside the reformist adjudication. Ambedkarite anti-caste voices would object that a hierarchy retained in symbol is a hierarchy retained in fact, and that reinterpretation launders what it claims to purify; they are present only as pressure, never as co-interpreters. Orthodox pandits would object that the reading amputates the corpus to save it; they are answered polemically. Dalit women specifically are absent from decisions about how spiritual-stage reinterpretations land on those who carry the stigma.
% DISAPPEARANCE_RATIONALE: If the reformist reading vanished overnight, hundreds of millions of adherents would face a forced binary — revert to literal observance or abandon the corpus — that the current settlement exists to dissolve. Diaspora identity infrastructure, reformist educational networks, and temple governance would reorganize immediately; anti-caste politics would lose its principal intra-tradition interlocutor; endogamy would lose its chief theological alibi and its chief theological shield simultaneously.
% FOUNDING_PROBLEM: The nineteenth-century collision between egalitarian critique (missionary attack, colonial administration, later constitutional democracy) and scriptural authority: how to keep the corpus authoritative while answering the charge that it sanctifies caste oppression. The reformist contextual reading was built to solve exactly this — preserve the tradition by reclassifying its oppressive prescriptions as time-bound.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Ambedkar's 1936 address attests the problem from the abolitionist seat (arguing reconciliation is impossible); missionary-era polemical records and colonial administrative reports document the original legitimation crisis; academic historiography of the reform movements traces the reading's genesis to that crisis rather than to internal doctrinal development. The orthodox and abolitionist seats both deny the founding problem's framing — and in doing so attest that the tension it names is real.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48 (interval end): the strict-enforcement layer is legally dead, but the symbolic layer still transfers precedence, marriage position, and occupational inheritance along caste lines — a genuine reduction from the literalist baseline, not a zero. Suppression is 0.42 as a raw structural property (unscaled by power or scope; only extractiveness is scaled, by directionality and spatial scope in the engine's computation): the settlement no longer coerces by scripture-backed sanction but by custom, economic dependency, and the reputational machinery of reformist respectability. Theater_ratio is 0.54 and rising monotonically across the series — the signature drift of this story: declarations of equality have radicalized faster than practice (intercaste marriage remains rare, purity practice persists), so a growing share of the settlement's activity is performing purification rather than producing it. Suppression_requirement rises from 0.15 to 0.42: the young reading barely needed enforcement (it was fighting for survival against orthodox counter-pressure); the mature settlement polices its own boundaries against both flanks — a compliance-hardening ratchet as reformist orthodoxy congealed. Base_extractiveness declines then plateaus (0.62 to 0.48): the legal and liturgical victories were real, but the residual floor is sticky. The series run on one shared six-point grid; no cyclical oscillation is asserted — the drift is monotonic, driven by the widening declaration/practice gap rather than by intermittent reinforcement. Identity-lock note: the acharya lineage's exit is identity_locked in the fused professional-relational sense — their authority IS the reading; if the separability frame broke, the interpreter role dissolves with it, which is precisely why the lineage defends the frame against refutation rather than testing it. Inter-institutional note: the orthodox establishments and the constitutional state experience the same settlement from opposite flanks with opposite exits — the orthodox can retreat into independent institutions (mobile), the state observes without doctrinal standing (analytical), while the acharya class has no exit that preserves its vocation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the acharya seat the settlement is a moral achievement it administers: the tradition saved, the oppressive layer discarded, extraction a residue to be managed. From the dalit and adivasi seat the same settlement is a hierarchy that traded enforcement for etiquette: the temple door opens and the marriage alliance closes; the scripture stops commanding and the custom continues delivering. From the upper-caste devotee seat the settlement is complete — the moral project finished, remaining inequality attributed to society rather than to the tradition. The engine derives these divergent classifications from the structural data (roles, power, exit options); nothing in the authored claim adjudicates between them. Coalition note: the powerless seat is not static — Ambedkarite mobilization, reservation politics, and conversion movements demonstrate coalition capacity, which is why the dalit exit option is authored constrained rather than trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   The acharya lineage sits near the beneficiary end despite setting the agenda: it collects the interpretive premium (trust income, publishing, global donations, the authority to decide what binds) and bears mainly the defensive burden of contest. Upper-caste devotees derive low directionality as declared beneficiaries — subsidized in status, paying little. Diaspora communities sit low-to-mid: net beneficiaries of the portable tradition, importing some remnant-norm costs. Dalit and adivasi communities derive high directionality near the full-target end: they bear the residual transfers, and their constrained exit amplifies effective extraction beyond what a mobile population would experience. Shudra communities sit mid-high: partial beneficiaries of expanded access, payers of continued deference. Observers (indologists, the state) take the analytical seat and feed no directional arithmetic. The victim declarations drive the asymmetry that makes this a tangled rope rather than a rope: the same structure that coordinates identity for the majority transfers position along caste lines to a defined minority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling textual authority with egalitarian critique — is live, not dead: every generation relitigates it, and the settlement's own theater trajectory shows the reconciliation failing at the practice level even as it succeeds at the declarative level. Classification therefore guards against two mislabels. It is not a rope: the beneficiary and victim declarations are real and asymmetric, and the settlement requires active enforcement (boundary-policing against literalist resurgence and abolitionist exit-blunting) to hold. It is not a snare: the coordination function is genuine and load-bearing for hundreds of millions, most seats adhere voluntarily, and the victim set is materially smaller than the literalist arrangement's. The watch item is drift toward piton: theater_ratio crossing 0.5 with a plateaued extraction floor suggests the settlement's purification function may be decaying into performance while its authority function persists — if the coordination function atrophies further, the same structure reclassifies as maintained-by-inertia. The mandatrophy question this story leaves open is recorded in the symbolic_hierarchy_residual_harm and gunakarma_reinterpretation_fidelity omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_separability_projection,
    'Is the reformist separability thesis a discovery of structure latent in the corpus, or a modern projection onto it — that is, do this reading and the orthodox literalist reading operate on the same kernel at all?',
    'Philological stratification studies (critical editions, transmission history) tracing whether dharma-as-conduct discourse is textually independent of the varna-prescriptive strands, or pervasively presupposes them.',
    'If projection, the reformist reading loses its claim to transmit the same kernel and collapses toward a newly-authored constraint closer to abolitionist_rejection; if discovery, the reading stands as legitimate heir and its medium epsilon is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_separability_projection, conceptual, 'Whether separability is recovered from the texts or imported into them.').

omega_variable(
    symbolic_hierarchy_residual_harm,
    'Does the symbolically retained hierarchy — endogamy, purity practice, ritual precedence — impose measurable harm on dalit and adivasi communities under the reformist settlement, or does the softening genuinely reduce harm?',
    'Longitudinal data on intercaste marriage rates, manual-scavenging persistence, temple-entry incidents, and atrocity rates correlated with reformist-institution density across regions.',
    'Determines the effective victim-set size and epsilon: high residual harm pushes the computed type toward snare; negligible residual harm supports a rope-leaning tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_hierarchy_residual_harm, empirical, 'Whether symbolic-only hierarchy still extracts from those at its base.').

omega_variable(
    gunakarma_reinterpretation_fidelity,
    'When reformists reinterpret varna as guna-and-karma-based spiritual stages, is this a faithful recovery of an internal textual resource or a motivated reinterpretation deployed to launder hierarchy under spiritual vocabulary?',
    'Compare pre-colonial commentarial usage of the relevant verses against reformist-era deployments; trace whether the reinterpretation tracks textual warrant or tracks external egalitarian pressure.',
    'If laundering, extraction persists beneath the spiritual vocabulary and effective extraction rises above the authored measure; if faithful, part of the measured extraction is misattributed and epsilon falls.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gunakarma_reinterpretation_fidelity, conceptual, 'Fidelity of the spiritual-stages reinterpretation of caste.').

omega_variable(
    stigma_internalization_ambiguity,
    'Is the suppression sustaining the residual hierarchy structural (economic dependency, village geography, marriage-market closure) or internalized (stigma carried by victims themselves, purity self-policing)?',
    'Post-exit trajectory: communities that converted or migrated out of the village economy — does stigma-correlated behavior and marriage closure persist after the structural mechanism is removed?',
    'If internalized, effective suppression exceeds the structural measure and persists after structural reform; reformist softening then understates the constraint''s real grip and the victim set is stickier than declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_internalization_ambiguity, empirical, 'Structural versus internalized suppression mechanism in residual caste stigma.').

omega_variable(
    kernel_identity_across_readings,
    'This constraint is one reading of kernel dharmasastra_corpus; the sibling readings orthodox_literalist and abolitionist_rejection instantiate different constraints from the same corpus. Is the corpus one kernel read three ways, or three kernels sharing a vocabulary? The disagreement is located in the authority predicate on the prescriptions: eternal, time-bound, or null.',
    'Not resolvable internally — resolved only by which reading''s institutions win allegiance over time; track jurisdictional shifts in temple governance, teaching lineages, and legal recognition.',
    'If the kernel fragments, cross-reading contamination analysis over the family becomes invalid and the network links degrade from structural to merely historical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_across_readings, conceptual, 'Committer-frame omega: kernel unity across the three declared readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__reformist_contextual, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__reformist_contextual, theater_ratio, 40, 0.28).
narrative_ontology:measurement(dhar_tr_t80, dharmasastra_corpus__reformist_contextual, theater_ratio, 80, 0.36).
narrative_ontology:measurement(dhar_tr_t120, dharmasastra_corpus__reformist_contextual, theater_ratio, 120, 0.44).
narrative_ontology:measurement(dhar_tr_t160, dharmasastra_corpus__reformist_contextual, theater_ratio, 160, 0.5).
narrative_ontology:measurement(dhar_tr_t200, dharmasastra_corpus__reformist_contextual, theater_ratio, 200, 0.54).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__reformist_contextual, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__reformist_contextual, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(dhar_be_t80, dharmasastra_corpus__reformist_contextual, base_extractiveness, 80, 0.52).
narrative_ontology:measurement(dhar_be_t120, dharmasastra_corpus__reformist_contextual, base_extractiveness, 120, 0.5).
narrative_ontology:measurement(dhar_be_t160, dharmasastra_corpus__reformist_contextual, base_extractiveness, 160, 0.49).
narrative_ontology:measurement(dhar_be_t200, dharmasastra_corpus__reformist_contextual, base_extractiveness, 200, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__reformist_contextual, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__reformist_contextual, suppression_requirement, 40, 0.22).
narrative_ontology:measurement(dhar_su_t80, dharmasastra_corpus__reformist_contextual, suppression_requirement, 80, 0.3).
narrative_ontology:measurement(dhar_su_t120, dharmasastra_corpus__reformist_contextual, suppression_requirement, 120, 0.36).
narrative_ontology:measurement(dhar_su_t160, dharmasastra_corpus__reformist_contextual, suppression_requirement, 160, 0.4).
narrative_ontology:measurement(dhar_su_t200, dharmasastra_corpus__reformist_contextual, suppression_requirement, 200, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the authority of the Dharmasastra' covers three structurally distinct claims and is modeled as three linked stories. The orthodox literalist reading (upstream, historically prior, cited as warrant) authors high epsilon over the same referent — full enforcement, maximal victim set. The abolitionist rejection reading (downstream repudiation) authors near-maximal epsilon — the arrangement is fundamentally oppressive with no legitimate residue. This reformist contextual reading authors medium epsilon (0.48): reduced victim set, hierarchy retained symbolically rather than in strict enforcement. Same referent (the standing dharmasastra-grounded normative arrangement), reading-indexed values. Each member links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
