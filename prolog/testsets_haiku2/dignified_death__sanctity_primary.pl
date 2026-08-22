% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__sanctity_primary, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Sanctity of Life Constraint (Intentional Life-Termination Prohibition)
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   The sanctity-of-life reading holds that human dignity resides in life's
 *   intrinsic value independent of the subject's conscious experience or
 *   autonomy, and that intentional life-termination violates transcendent
 *   moral law regardless of individual consent or suffering. This constraint
 *   operates as a legal and institutional prohibition on assisted dying. The
 *   reading frames the constraint as protective moral law; empirically, the
 *   constraint operates as coercive prolongation of suffering for those
 *   judging themselves unfit to continue. The claim-metric divergence is
 *   structural: the reading claims the constraint is natural law / moral
 *   necessity; the authored metrics describe substantial extraction from a
 *   powerless victim set (terminally ill, elderly, poor, disabled) whose exit
 *   options are constrained by both law and identity-locking (internalized
 *   moral narratives). The kernel-contest frame is essential: this is ONE
 *   READING of a contested dignity kernel. The autonomy-primary and
 *   relational-autonomy readings instantiate different constraints from the
 *   same kernel, with different ε values, different victim sets, and
 *   different types.
 *
 * KEY AGENTS:
 *   - institutional_moral_order: agenda-setter of the constraint; derives legitimacy from transcendent principle rather than beneficiary collection
 *   - religious_communities: beneficiaries; maintain doctrinal coherence backed by state enforcement; mobile exit
 *   - medical_professionals: enforcers; constrained by professional codes, legal liability, and moral strain; institutional power but trapped exit
 *   - terminally_ill_with_intractable_suffering: primary victim; powerless; trapped exit; immediate time horizon; seek autonomy over timing/method
 *   - elderly_socially_isolated: secondary victim; identity-locked by institutional dependence; both structural and internalized suppression
 *   - poor_burden_constrained: tertiary victim; trapped within binary of resource-drain prolongation vs. legal violation; precarity-driven
 *   - disability_advocacy_conservative: beneficiary through moral-order alignment; organized power; mobile exit; defend against medicalization of disability
 *   - autonomy-primary reading advocates: structurally excluded; would argue for individual choice architecture; their competing reading is not admitted to law-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.58).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.71).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity of Life Constraint (Intentional Life-Termination Prohibition)").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, 'dfa162af-5946-447a-8738-859959ff1cce').
narrative_ontology:cs_kernel_codification('dfa162af-5946-447a-8738-859959ff1cce', fixed_text).
narrative_ontology:cs_authority_grounding('dfa162af-5946-447a-8738-859959ff1cce', lineage).
narrative_ontology:cs_interpretation_layer_present('dfa162af-5946-447a-8738-859959ff1cce').
narrative_ontology:cs_reading_relation('dfa162af-5946-447a-8738-859959ff1cce', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('dfa162af-5946-447a-8738-859959ff1cce', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_axiom('dfa162af-5946-447a-8738-859959ff1cce', foundational, life_intrinsic_value_sacrosanct).
narrative_ontology:cs_axiom_status(life_intrinsic_value_sacrosanct, holdable).
narrative_ontology:cs_axiom_grounding('dfa162af-5946-447a-8738-859959ff1cce', life_intrinsic_value_sacrosanct, deontological).
narrative_ontology:cs_axiom('dfa162af-5946-447a-8738-859959ff1cce', foundational, autonomy_subordinate_to_sanctity).
narrative_ontology:cs_axiom_status(autonomy_subordinate_to_sanctity, holdable).
narrative_ontology:cs_axiom_grounding('dfa162af-5946-447a-8738-859959ff1cce', autonomy_subordinate_to_sanctity, theological).
narrative_ontology:cs_reference_frame('dfa162af-5946-447a-8738-859959ff1cce', divine_sanctity_doctrine).
narrative_ontology:cs_drift_state('dfa162af-5946-447a-8738-859959ff1cce', contemporary_globalization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dfa162af-5946-447a-8738-859959ff1cce', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, institutional_moral_order).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, religious_communities).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, disability_advocacy_conservative).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, terminally_ill_with_intractable_suffering).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, elderly_socially_isolated).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, disabled_economically_precarious).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, poor_burden_constrained).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, medical_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The inherited doctrinal and legal commitment to the sanctity of life principle. Administers prohibition through law, professional codes, and institutional practice. Does not collect rents directly but maintains legitimacy through enforcement and symbolic performance. Justifies the constraint as moral law transcending individual preference.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, institutional_moral_order, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(dignified_death__sanctity_primary, institutional_moral_order).

% Maintain doctrinal coherence around sanctity-of-life teaching. Benefit from legal enforcement of the norm — their moral claims are backed by state coercion. Can exit individual jurisdictions and do so where legal environment shifts (migrate to supportive nations), but organized institutional presence persists across borders.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, religious_communities, beneficiary,
    organized, civilizational, mobile, global).

% Enforce the constraint through clinical gatekeeping, professional licensing, and refusal-to-participate protocols. Face legal liability and license revocation if they assist death. Experience ongoing professional and moral strain: many report conflict between the constraint's prohibition and their patients' expressed values. Their constrained exit means they cannot easily relocate to jurisdictions with different law; they are institutionally embedded.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, medical_professionals, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, medical_professionals, payer).

% Seek to end life on their own timeline due to unmanageable pain, loss of bodily function, or loss of capacities constitutive of their self-understanding. The constraint denies them this option regardless of their expressed will and autonomous judgment. Migration to jurisdictions permitting assisted death requires resources and agency most lack at end-of-life; covert exit (unsupervised overdose, refusal of food/water) occurs in conditions of secrecy and fear. Their immediate time horizon means prolongation extracts from them daily.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, terminally_ill_with_intractable_suffering, payer,
    powerless, immediate, trapped, local).

% Lack family or community presence to advocate for their preferences or even to hear them. Institutional isolation creates identity-locking: they cannot exit the medical system without losing the care relationship that structures their remaining existence. Suppression operates at two levels: structural (legal prohibition) and internalized (absorbed narratives that prolonged suffering is moral duty, that their death would be a burden). The identity-lock is not volitional but structural — they have become defined by institutional dependence.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, elderly_socially_isolated, payer,
    powerless, immediate, identity_locked, local).

% Advocates against assisted death on grounds that disability itself is not grounds for death and that permitting death decisions medicalizes disability stigma. Their stated goal is protecting disabled lives from ableist assumptions. They benefit from the legal prohibition protecting against death-through-disability assumptions, though their reasoning differs from the sanctity principle's grounding. Mobile exit through political advocacy and international disability networks.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disability_advocacy_conservative, beneficiary,
    organized, generational, mobile, national).

% Experience economic pressure not to seek palliative care (cost, family burden). Face internalized narratives of being a drain on family resources. The constraint blocks the exit they perceive as merciful (managed death) while also blocking structural interventions (guaranteed palliative care, family economic support, living wage, caregiver benefits) that would address the underlying precarity. Trapped within a binary: suffer while consuming family resources, or violate law. No exit option available that preserves dignity as they understand it.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, poor_burden_constrained, payer,
    powerless, biographical, trapped, local).

% Bioethicists, autonomy-focused disability advocates, and end-of-life preference researchers who argue for individual choice architecture, informed consent, and procedural safeguards. In sanctity-dominant jurisdictions, their reading is legally and institutionally excluded from shaping end-of-life policy. They would argue for the autonomy_primary constraint instead. Their exclusion is structural to this constraint's enforcement — the sanctity reading can only persist by silencing the autonomy reading.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, autonomy_primary_advocates, excluded,
    organized, generational, trapped, global).

% Investigates the structure of the end-of-life constraint independently, documenting the divergence between stated protective function and observed coercive operation. Observes the kernel contest among readings and tracks which reading dominates in which jurisdictions and why.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, bioethics_analytical_seat, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__sanctity_primary, institutional_moral_order).
narrative_ontology:fixing_cost_class(dignified_death__sanctity_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The sanctity constraint performs NO genuine coordination function. It does not solve a collective-action problem or align incentives around a shared benefit. The appearance of coordination — protecting vulnerable populations from pressure or regret — requires active suppression of the alternative (that the vulnerable population itself judges its interests better than remote moral authority). This differentiates snare from rope: a rope solves a real problem through coordination; a snare presents cover-story coordination to justify extraction.
% TRANSFER_FUNCTION: Transfers moral authority and decision-right from individual agents to institutional medical and legal authority, backed by the state. Moves the locus of end-of-life decisions from the individual+family+clinician triad (where preferences and values can be negotiated) to law enforcement and professional gatekeeping (where the constraint is non-negotiable). Extracts autonomy — the capacity to determine the timing and manner of one's own death — from those experiencing suffering and concentrates that authority in institutional hands.
% ABSENT_VOICES: Individuals at end-of-life in sanctity-dominant jurisdictions whose autonomy preferences and experiential judgments about their own dignity are systematically excluded from the law-making conversation. The constraint is authored by institutional authorities, religious communities, and conservative disability advocates; those who would choose different timing are either unheard (cognitively impaired, sedated, dying alone) or actively silenced (legal prohibition makes the conversation itself illegal — discussing options with a patient can constitute assisting death). Also excluded: the relational-autonomy reading's advocates (those who would argue for shared-authority decision architecture with procedural safeguards) and the autonomy-primary reading's advocates (those who would center individual choice). The constraint can only persist by excluding the voices that would say 'I want a different reading of dignity and therefore a different law'.
% DISAPPEARANCE_RATIONALE: If the sanctity-enforced prohibition disappeared overnight, end-of-life decisions would be made through varied decision architectures (some purely individual choice, some family-centered, some clinical consultation) rather than imposed by law. Medical practice would bifurcate by jurisdiction: some would adopt legalized assisted death with procedural safeguards; others would maintain prohibition voluntarily through professional culture. The moral and legal authority currently concentrated in institutional hands would distribute across individuals, families, and clinicians. Some terminally ill and elderly agents would gain options they currently lack; the institutional moral order would lose enforcement capacity and symbolic authority. The world materially rearranges.
% FOUNDING_PROBLEM: Early recognition that life-ending decisions require moral seriousness: preventing capricious or exploitative killing, protecting those unable to consent (unconscious, cognitively impaired, isolated), preventing pressure on vulnerable populations to end their lives for family or social convenience. The founding problem was real and defensible — the constraint originates in a legitimate protective instinct.
% FOUNDING_PROBLEM_CORROBORATION: The institutional moral order (Catholic hierarchy, evangelical Protestant leadership, conservative bioethicists) attests the problem is live and the constraint necessary. Medical professions attest both ways: some argue the prohibition is essential protection; others argue protection is maintained by procedural safeguards without prohibition. Terminally ill advocates and autonomy-focused bioethicists attest that the founding problem is substantially solved in contexts with robust palliative care, informed-consent architecture, and procedural safeguards, and that the constraint now operates as coercive prolongation of suffering rather than protection of the vulnerable. Empirical evidence from jurisdictions with legalized assisted death (Switzerland since 1937, Netherlands since 2002, Belgium since 2002, Canada since 2016, and expanding regimes in Australia, Uruguay, and US states) shows that careful procedural architecture — mandatory waiting periods, clinician consultation, family notification options, mandatory palliative care, psychological assessment — preserves protection against exploitation while permitting individual choice. Exit studies track individuals who travel to legalized jurisdictions: they report high satisfaction with timing and method; regret rates are near-zero. This corroboration from outside the sanctity-defending parties supports the contested status.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__sanctity_primary, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__sanctity_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__sanctity_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__sanctity_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.58 at interval end. The constraint extracts decision authority, autonomy, and the timing of death from individuals experiencing suffering they judge unendurable, and concentrates that authority in institutional hands (medical, legal, religious). The extraction is substantial because it denies the subject's own judgment about their own dignity and interests. It is not extractive in the economic sense (no money flows) but in the structural sense (alienation of self-determination). Suppression measures 0.71: the constraint's persistence depends on active legal enforcement (criminal penalties for assisting), professional gatekeeping (licensing boards enforce refusal), and internalized moral narratives (elderly isolated agents accept prolonged suffering as moral duty). Suppression shows upward trajectory (0.55 → 0.72 over the interval) because enforcement infrastructure hardened (criminal statutes tightened, professional codes sharpened, international treaty pressure increased). Theater ratio measures 0.42 at interval end with upward trajectory (0.28 → 0.44): the protective language (safeguarding the vulnerable, defending dignity) persists as the public face while the functional operation increasingly reveals itself as coercive prolongation. The gap widens because empirical evidence accumulates (jurisdictions with legalized assisted death show protection goals can be met without prohibition; victim testimony documents that the constraint prevents rather than protects dignity as the victims understand it). Measurement grid is aligned: all three metrics share the same time points (0, 8, 16, 24, 32, 40). The terminal projection (t=40) shows extractiveness and suppression receding slightly, reflecting potential pressure from the competing autonomy-reading and visible falsehood of the protection claim.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (institutional moral order) computes the constraint as protective natural law with minimal extraction and theater (the stated function matches the functional reality as seen from authority's perspective). The victim seats (terminally ill, elderly, poor) compute the constraint as coercive extraction masquerading as protection (high extraction, high suppression, high theater — the constraint's stated protective function contradicts the victims' experience). The medical-professional seat sits between: genuine protective function (preventing capricious killing) is real, but increasingly serves the institutional agenda-setter's authority-concentration goal rather than the victims' interests. The engine computes these divergences from the structural data: beneficiary/victim declarations, power atoms, exit options, time horizons. The claimed type (snare) already declares the author's reading of this divergence — the agenda-setter's protective claim masks extractive operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional moral order: d ≈ 0.2 (beneficiary, low target pressure). Religious communities: d ≈ 0.15 (beneficiary; mobile exit reduces target pressure). Medical professionals: d ≈ 0.5 (genuinely split: they benefit from professional authority and clear ethical guidance, but are trapped by liability and professional strain; neither fully beneficiary nor fully target). Terminally ill with intractable suffering: d ≈ 0.95 (full target; powerless, trapped exit, immediate time horizon, suffer under the constraint). Elderly socially isolated: d ≈ 0.92 (full target; powerless, identity-locked exit, immediate time horizon, suppression both structural and internalized). Disabled economically precarious: d ≈ 0.88 (target; powerless, constrained exit, biographical time horizon, structural precarity amplifies constraint's extraction). Poor burden constrained: d ≈ 0.91 (target; powerless, trapped exit, biography- to immediate time horizon, binary choice between prolonged resource-drain and legal violation). Disability advocacy conservative: d ≈ 0.25 (beneficiary despite advocacy framing; their stated goal — protecting disabled lives — aligns them with the constraint's operation, though their reasoning differs). Autonomy-primary advocates: d ≈ 0.95 (full target; excluded from decision-making; their reading is suppressed by this constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy detection asks: does the constraint's mandate (protect life, prevent capricious killing) still match the constraint's functional operation? Founding problem status = contested (the original problem — preventing exploitation and murder — is substantively addressed in contexts with robust procedural safeguards and palliative care; the constraint now enforces prolongation beyond the point the victim judges their interests). Disappearance verdict = world_rearranges (the arrangement depends on continued enforcement; without the prohibition, end-of-life decisions restructure). Theater ratio = 0.42 and rising (protective language persists but masks coercive operation; the gap widens as evidence accumulates that protection is decoupled from prohibition). The triumvirate signals incipient mandatrophy: the mandate is dead (protection goals are met elsewhere), the verdict says arrangements rearrange (the constraint is not natural law but human choice), and theater is high (the authority's story about the constraint diverges from what it functionally does). The constraint has not fully resolved mandatrophy — the institutional moral order still maintains sincere belief in the original mandate — but the conditions for mandatrophy resolution are present. A future snapshot would likely show base_properties.mandatrophy_resolved = true if the constraint persists despite the manifest death of its founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transcendent_law_vs_constructed_norm,
    'Is the sanctity-of-life principle a transcendent moral law discovered and transmitted, or a constructed institutional norm maintained through state coercion?',
    'Comparative institutional analysis: jurisdictions that decriminalize assisted death while maintaining robust palliative care and procedural safeguards; empirical measurement of whether moral outcomes (patient autonomy satisfaction, family grief, clinician moral distress) improve or degrade; genealogical historical analysis of the constraint''s institutional and legal origins.',
    'If transcendent/discovered, the constraint is natural law (mountain type), ε ≈ 0, victims are protected by moral reality. If constructed/maintained, the constraint is extracted authority (snare type), ε = high, victims are those denied choice. The type classification depends entirely on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transcendent_law_vs_constructed_norm, conceptual, 'Natural law vs. institutional construction of the sanctity principle').

omega_variable(
    suppression_structural_vs_internalized,
    'For elderly socially isolated agents and poor burden-constrained agents, is measured suppression (0.71) structural (external legal barriers) or internalized (moral narratives they have absorbed)?',
    'Post-exit measurement: tracking moral distress and decision-regret in individuals who migrate to jurisdictions permitting assisted death and choose it, vs. those who are blocked and forced to remain under the constraint. If suppression persists after barrier removal (continued moral conviction that death was wrong), it is internalized; if suppression resolves, it was structural.',
    'If internalized, the constraint''s effective suppression is deeper than the legal metric suggests — the target carries the suppression with them even after exit. This would elevate the piton risk (internalized theater) or deepen the snare classification. If structural, the suppression resolves once barriers drop, suggesting the constraint''s persistence is more brittle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression in end-of-life constraints').

omega_variable(
    protection_efficacy_across_safeguard_architecture,
    'Do the stated protective goals of the sanctity constraint (prevent exploitation, protect vulnerable populations) require the blanket prohibition, or can they be achieved through procedural safeguards (informed consent, family consultation, clinician judgment, mandatory palliative care)?',
    'Comparative empirical analysis of outcomes in jurisdictions with (1) blanket prohibition, (2) procedural safeguards + legalized choice, (3) intermediate regimes. Measure: vulnerable-population exploitation rates, patient satisfaction, family regret, clinician moral distress, quality of palliative care, incidence of coerced death decisions.',
    'If protective goals are achievable under safeguards, the blanket prohibition extracts autonomy without corresponding protective gain — ε stays high, type stays snare. If safeguards fail to protect (evidence of exploitation rises), the extraction serves a real protective function — ε could be reframed as coordination cost, type might shift toward tangled_rope. The current corpus assumes the foundational problem (preventing exploitation) is substantially addressed where safeguards are robust; this omega tests that assumption.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(protection_efficacy_across_safeguard_architecture, empirical, 'Whether the sanctity constraint''s protective function requires prohibition or is achievable through procedural safeguards').

omega_variable(
    dignity_meaning_contest_in_kernel,
    'This reading defines dignity as life''s intrinsic value and interprets the constraint as protecting it. The autonomy-primary reading defines dignity as self-determination and interprets the same constraint as violating it. Is this a factual disagreement (what dignity IS) or a preference disagreement (what should matter)?',
    'Conceptual analysis of whether dignity-as-intrinsic-value and dignity-as-autonomy are logically compatible frameworks, or mutually exclusive frameworks. Phenomenological study of how terminally ill agents themselves describe their dignity in end-of-life decisions. Theological and philosophical hermeneutics of the sanctity tradition''s own texts on dignity (do they foreclose autonomy claims or permit them?)',
    'If factual disagreement: one reading is wrong about what dignity is. The victimhood classification (who is harmed by the constraint) depends on getting dignity''s nature right. If preference disagreement: both readings can be simultaneously true under different value systems; the kernel admits genuine coexistence rather than foreclosure. The reading_relations.relation value (forecloses vs. coexists_with) depends on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_meaning_contest_in_kernel, conceptual, 'Whether competing dignity definitions in the kernel are factual or preference-based disagreements').

omega_variable(
    institutional_self_interest_in_sanctity_maintenance,
    'To what extent does the institutional moral order (religious communities, medical professions, state authorities) maintain the sanctity constraint because they sincerely believe it protects dignity vs. because it preserves institutional authority over end-of-life decisions?',
    'Comparative institutional analysis: tracking shifts in institutional position as (1) palliative care improves (reduces suffering-based pressure for assisted death), (2) procedural safeguard proposals surface (requiring institutional authority to either endorse safeguards or explicitly defend prohibition), (3) jurisdictional legalization abroad pressures domestic institutions. Historical analysis of institutional positions in contexts where the constraint has been relaxed (did institutions genuinely accept the moral argument for autonomy, or were they forced by legal change?)',
    'If institutional position is driven by genuine moral conviction, the constraint remains stable as principled snare. If institutional position is driven by authority-preservation, the constraint is vulnerable to institutional capture (institutional authority converts to other domains, leaving the constraint degraded / Piton-like). If mixed, the constraint''s persistence depends on the balance between conviction and self-interest, and theater ratio becomes a leading indicator of institutional disengagement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_self_interest_in_sanctity_maintenance, conceptual, 'Institutional sincerity vs. self-interest in maintaining the sanctity constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__sanctity_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement(dign_tr_t8, dignified_death__sanctity_primary, theater_ratio, 8, 0.32).
narrative_ontology:measurement(dign_tr_t16, dignified_death__sanctity_primary, theater_ratio, 16, 0.37).
narrative_ontology:measurement(dign_tr_t24, dignified_death__sanctity_primary, theater_ratio, 24, 0.42).
narrative_ontology:measurement(dign_tr_t32, dignified_death__sanctity_primary, theater_ratio, 32, 0.46).
narrative_ontology:measurement(dign_tr_t40, dignified_death__sanctity_primary, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__sanctity_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dign_be_t8, dignified_death__sanctity_primary, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(dign_be_t16, dignified_death__sanctity_primary, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(dign_be_t24, dignified_death__sanctity_primary, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(dign_be_t32, dignified_death__sanctity_primary, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(dign_be_t40, dignified_death__sanctity_primary, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__sanctity_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dign_su_t8, dignified_death__sanctity_primary, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(dign_su_t16, dignified_death__sanctity_primary, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(dign_su_t24, dignified_death__sanctity_primary, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(dign_su_t32, dignified_death__sanctity_primary, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(dign_su_t40, dignified_death__sanctity_primary, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dignified_death__sanctity_primary, 0.12).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% The dignified_death kernel admits three structurally distinct constraint readings. SANCTITY_PRIMARY (this story) defines dignity as life's intrinsic value and measures extraction from those denied choice. AUTONOMY_PRIMARY defines dignity as self-determination and measures extraction from those denied agency. RELATIONAL_AUTONOMY defines dignity as emergent from triad process and measures extraction from those with skewed relational power. All three readings use the same legal substrate (end-of-life law) but instantiate different constraints with different ε values, different victim sets, and different types. The three stories are linked by network.affects_constraints; each sister story documents the same kernel contest from a different reading position. No single story represents the kernel; the kernel is the set of all three stories and their structural relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dignified_death__sanctity_primary, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
