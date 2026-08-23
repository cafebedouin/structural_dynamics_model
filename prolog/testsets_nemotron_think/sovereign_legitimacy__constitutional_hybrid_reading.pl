% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Constitutional Hybrid Legitimacy Settlement
 *   domain: political/philosophical/constitutional
 *
 * SUMMARY:
 *   This constraint story models the constitutional hybrid reading of
 *   sovereign legitimacy — the settlement in which ceremonial/symbolic
 *   authority is inherited by a hereditary monarch while political authority
 *   is delegated to elected officials, with constitutional law (interpreted
 *   by courts) mediating the boundary. The arrangement is presented as a
 *   stable compromise that reduces the extractiveness of both pure
 *   monarchical and pure republican forms, but introduces persistent
 *   ambiguity costs and vulnerability to boundary disputes. The claim/metric
 *   gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   (coordination + asymmetric extraction) while the authored metrics
 *   describe a low-to-moderate extraction settlement with active enforcement
 *   of the boundary — the engine measures that divergence; do not reconcile
 *   the claim to the metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.35).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.45).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Hybrid Legitimacy Settlement").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political/philosophical/constitutional").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '7ad234eb-226b-433e-b31e-500c2f8561df').
narrative_ontology:cs_kernel_codification('7ad234eb-226b-433e-b31e-500c2f8561df', formalized).
narrative_ontology:cs_authority_grounding('7ad234eb-226b-433e-b31e-500c2f8561df', lineage).
narrative_ontology:cs_interpretation_layer_present('7ad234eb-226b-433e-b31e-500c2f8561df').
narrative_ontology:cs_reading_relation('7ad234eb-226b-433e-b31e-500c2f8561df', sovereign_legitimacy__monarchical_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ad234eb-226b-433e-b31e-500c2f8561df', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_axiom('7ad234eb-226b-433e-b31e-500c2f8561df', foundational, dual_source_legitimacy).
narrative_ontology:cs_axiom_status(dual_source_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7ad234eb-226b-433e-b31e-500c2f8561df', dual_source_legitimacy, conventional).
narrative_ontology:cs_axiom('7ad234eb-226b-433e-b31e-500c2f8561df', foundational, constitutional_mediation_authority).
narrative_ontology:cs_axiom_status(constitutional_mediation_authority, holdable).
narrative_ontology:cs_axiom_grounding('7ad234eb-226b-433e-b31e-500c2f8561df', constitutional_mediation_authority, conventional).
narrative_ontology:cs_reference_frame('7ad234eb-226b-433e-b31e-500c2f8561df', founding_compromise).
narrative_ontology:cs_drift_state('7ad234eb-226b-433e-b31e-500c2f8561df', contemporary_constitutional_stress, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7ad234eb-226b-433e-b31e-500c2f8561df', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_faction).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, republican_faction).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, citizens).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, citizens).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__constitutional_hybrid_reading, dual_legitimacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ceremonial head-of-state status, symbolic authority, state income, and ceremonial prerogatives (dissolution of parliament, appointment of ministers on advice, royal assent) under constitutional convention. The monarch's person and office are fused — exit would mean abdication, which is treated as constitutional crisis rather than career change. Benefits from the settlement's stability but cannot convert symbolic capital into political power without triggering the boundary dispute the settlement exists to contain.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, generational, identity_locked, national).

% Hold delegated political authority (legislative initiative, executive power, budgetary control) derived from electoral mandate. Their legitimacy depends on the constitutional settlement's recognition that popular sovereignty operates within the inherited framework. They benefit from the monarch's ceremonial legitimation of the state but are constrained by conventions that reserve certain prerogatives to the crown. Exit means electoral defeat or resignation — possible but costly, and the office's legitimacy survives the individual.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, beneficiary,
    institutional, biographical, constrained, national).

% Authoritatively interprets the constitutional boundary between ceremonial and political authority. Decides boundary disputes (e.g., scope of royal prerogative vs. ministerial advice, dissolution powers, reserve powers). Its rulings constitute the living settlement. Does not collect extraction but administers the constraint's coordination function. Exit is analytical — judges serve fixed terms and their authority is role-bound, not person-bound.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_court, agenda_setter,
    institutional, generational, analytical, national).

% Advocates for undivided sovereign authority vested in the monarch by inheritance and divine/traditional sanction. The constitutional hybrid denies them their preferred legitimacy structure — they must operate within a framework that treats popular sovereignty as co-equal source. Their political project is structurally constrained: they can lobby for expanded royal powers but cannot openly seek restoration of absolute rule without delegitimizing themselves within the constitutional order. Exit means leaving the polity or accepting permanent opposition.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_faction, payer,
    organized, generational, trapped, national).

% Advocates for legitimacy flowing exclusively from popular sovereignty, with no hereditary element. The constitutional hybrid denies them their preferred legitimacy structure — they must operate within a framework that treats inherited ceremonial authority as legitimate. Their political project is structurally constrained: they can campaign for abolition of the monarchy but cannot achieve it without constitutional amendment procedures that require the very institutions they oppose. Exit means leaving the polity or accepting permanent opposition.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, republican_faction, payer,
    organized, generational, trapped, national).

% Receive stability, continuity, and a legitimated state apparatus from the hybrid settlement — the coordination benefit. Also bear the ambiguity costs: constitutional crises over boundary disputes (prorogation, dissolution, reserve powers), symbolic exclusion of citizens who reject hereditary principle, and the democratic deficit of an unelected head of state. Exit is constrained — emigration is possible but costly; the settlement's legitimacy shapes the political culture they inhabit regardless of individual consent.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, citizens, payer).

% Analyze the settlement's operation, boundary disputes, and comparative variants. They do not collect extraction nor bear its costs directly. Their work informs the constitutional court's interpretation layer and public discourse. Exit is analytical — they can change research focus without personal cost.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mediates between inherited ceremonial authority and delegated political authority, preventing legitimacy vacuums and civil conflict by providing a settled boundary recognized by constitutional law and enforced through judicial interpretation.
% TRANSFER_FUNCTION: Moves ceremonial status, symbolic capital, and material privileges (state income, residences, ceremonial prerogatives) to the hereditary monarch; moves policy decision-making power, legislative initiative, and executive authority to elected officials; moves authoritative boundary interpretation to constitutional courts. The costs of ambiguity, boundary disputes, and democratic deficit are borne by factions seeking pure forms (absolutists and republicans) and by citizens subject to institutional friction.
% ABSENT_VOICES: Radical republicans who reject any hereditary element and absolute monarchists who reject any popular element are structurally excluded from the constitutional settlement. They exist in civil society, opposition movements, and diaspora but have no voice in the mediating institutions (parliament, court, crown) because the settlement's coherence depends on their exclusion.
% DISAPPEARANCE_RATIONALE: If the constitutional hybrid settlement vanished overnight, the monarch would either reclaim political power (restoring absolutism) or be abolished (establishing a republic); elected officials would either become fully sovereign or lose the ceremonial legitimation that stabilizes their authority; the polity would reorganize around either monarchical or republican principles, likely through constitutional crisis, referendum, or civil conflict.
% FOUNDING_PROBLEM: The founding problem was resolving the legitimacy crisis between hereditary right and popular sovereignty without civil war or regime collapse — establishing a stable boundary that lets both sources coexist within a single constitutional order.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional historians outside the benefiting parties (e.g., scholars of European constitutional monarchies, Commonwealth transitions) attest the settlement solved the immediate post-revolutionary crisis but introduced persistent boundary disputes that resurface under stress; the monarch's household and the government attest the problem remains live as new challenges (devolution, human rights, prorogation crises) test the boundary.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-to-moderate (0.35) because the compromise genuinely reduces the extractive overhead of both pure forms: the monarch loses political power but retains status/income; elected officials gain power but accept ceremonial legitimation from an unelected source. Suppression is moderate (0.45) because the boundary requires active judicial enforcement and constitutional convention maintenance — when disputes arise (prorogation, dissolution, reserve powers), the court must intervene. Theater is low-moderate (0.25): ceremonial performance is real but not the primary extraction mechanism. Accessibility collapse is moderate (0.40): pure-form alternatives (republic, absolutism) remain intellectually and politically available but are structurally excluded from the operating constitution. Resistance is moderate-high (0.55): both absolutist and republican factions actively contest the settlement, though within constitutional channels.
 *
 * PERSPECTIVAL GAP:
 *   From the monarch's seat, the settlement is a genuine coordination achievement — they retain dignity and relevance while avoiding the extraction that would come from political rule. From the elected officials' seat, it is a pragmatic legitimation device — the crown's ceremony stabilizes their democratic mandate. From the absolutist and republican seats, it is an unjust compromise that denies their respective legitimacy principles. The constitutional court experiences it as an interpretive burden — the boundary is inherently contestable and every ruling risks appearing partisan. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary monarch and elected officials are structural beneficiaries — both collect material and symbolic benefits from the settlement (d near beneficiary end). The absolutist and republican factions are structural payers — their preferred legitimacy structures are excluded by the compromise (d near target end). Citizens sit near symmetric: they receive stability (coordination benefit) but bear ambiguity costs and democratic deficit. The constitutional court sits at analytical — it administers the boundary without collecting extraction. The monarch's identity_locked exit (office fused with person) amplifies their effective extraction despite beneficiary status; the factions' trapped exit amplifies their effective extraction despite organized power.
 *
 * MANDATROPHY ANALYSIS:
 *   The settlement was built to solve a live founding problem (post-revolutionary legitimacy crisis). That problem is now contested — the immediate crisis is gone, but new boundary disputes (devolution, human rights, prorogation) keep the settlement's function alive. The classification as tangled_rope (not snare) captures this: the coordination function (preventing legitimacy vacuum) is real and ongoing, but asymmetric extraction persists (monarch retains privileges without accountability; officials gain legitimation without full democratic accountability). If the founding problem were dead and the settlement persisted purely by inertia, it would drift toward piton. The active interpretation layer (constitutional court) prevents that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'How does the classification change if this constraint is understood as one reading of the contested sovereign_legitimacy kernel rather than a standalone constraint?',
    'Compare the hybrid reading''s ε, beneficiary/victim structure, and type against the monarchical_reading and republican_reading stories. If the three readings form a constraint family with distinct ε values, the kernel decomposition is validated. If they collapse to similar metrics, the kernel framing is artificial.',
    'If the kernel framing is validated, cross-reading contamination analysis becomes possible — drift in one reading (e.g., monarchical legitimism resurgence) predicts drift in the hybrid reading''s boundary stability. If not, each reading must be evaluated independently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'Whether the sovereign_legitimacy kernel decomposition into three readings is structurally warranted.').

omega_variable(
    boundary_ambiguity_extraction,
    'Does the constitutional boundary''s inherent ambiguity function as a coordination feature (flexibility) or an extraction mechanism (discretionary power for agenda_setters)?',
    'Track boundary dispute outcomes over time: if courts consistently rule to expand one side''s authority at the expense of the other, ambiguity is an extraction mechanism. If rulings oscillate or maintain balance, ambiguity is a coordination feature.',
    'If extraction mechanism, the settlement''s ε is understated — the ambiguity costs are not symmetric but systematically favor the agenda_setter (court) or one beneficiary. If coordination feature, the current ε fairly captures the settlement''s extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_ambiguity_extraction, empirical, 'Whether constitutional ambiguity serves coordination or extraction.').

omega_variable(
    identity_lock_mechanism_monarch,
    'Is the monarch''s identity_locked exit driven by personal identity fusion (psychological) or institutional identity fusion (the office cannot exist without the person)?',
    'Compare abdication precedents: if abdication triggers constitutional crisis regardless of the individual, the lock is institutional. If abdication is treated as personal choice with managed succession, the lock is psychological.',
    'If institutional, the monarch''s effective extraction is higher — they cannot exit without destabilizing the constraint itself, making them a captive beneficiary. If psychological, the lock is contingent and could shift with generational change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_monarch, conceptual, 'Nature of the monarch''s identity-locked exit from the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_legit_hybrid_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sov_legit_hybrid_tr_t20, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(sov_legit_hybrid_tr_t40, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(sov_legit_hybrid_tr_t60, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(sov_legit_hybrid_tr_t80, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(sov_legit_hybrid_tr_t100, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(sov_legit_hybrid_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(sov_legit_hybrid_be_t20, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(sov_legit_hybrid_be_t40, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(sov_legit_hybrid_be_t60, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(sov_legit_hybrid_be_t80, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 80, 0.35).
narrative_ontology:measurement(sov_legit_hybrid_be_t100, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(sov_legit_hybrid_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sov_legit_hybrid_su_t20, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(sov_legit_hybrid_su_t40, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(sov_legit_hybrid_su_t60, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement(sov_legit_hybrid_su_t80, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 80, 0.45).
narrative_ontology:measurement(sov_legit_hybrid_su_t100, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__constitutional_hybrid_reading, 0.1).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is the constitutional_hybrid_reading of the sovereign_legitimacy kernel. The monarchical_reading asserts single-source inherited authority; the republican_reading asserts single-source delegated authority. This reading asserts dual-source authority with constitutional mediation. The three readings form a constraint family: the hybrid reading's ε (0.35) is lower than either pure form would exhibit (monarchical ~0.6, republican ~0.4) because the compromise reduces extractiveness, but the ambiguity costs and boundary enforcement create a distinct extractive profile. The hybrid reading influences both siblings by occupying the institutional center — its stability affects the viability of the pure-form alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_legitimacy__constitutional_hybrid_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
