% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__property_reading, []).

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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animals as Legal Property Objects (Rights-Excluding Reading)
 *   domain: legal philosophy / applied ethics
 *
 * SUMMARY:
 *   This constraint instantiates the property reading of contested
 *   animal-status kernel: animals are treated as legal objects (chattels,
 *   property) without independent moral standing or inherent rights. Human
 *   ownership is unrestricted except by welfare statutes enacted through
 *   human legislative processes. The reading establishes a framework where
 *   all questions about animal treatment are resolved as human-to-human
 *   disputes over property use, never as direct claims from animal interests.
 *   This is the dominant reading in Western property law and most economic
 *   systems; it is actively contested by abolitionist and strong-welfare
 *   readings that deny its core premises. The constraint's extractiveness is
 *   near-zero (0.05) because the property framework itself performs minimal
 *   extraction on human parties — the principal extraction (if any) is on
 *   animals, but animals are not stakeholders in this legal framework by
 *   definition under this reading.
 *
 * KEY AGENTS:
 *   - human_economic_actors (owners, users of animal property)
 *   - welfare_advocates (pressure for restrictions within property frame)
 *   - abolitionist_movements (excluded, deny the frame itself)
 *   - legislators (enact welfare statutes as human prerogative)
 *   - courts (enforce property law)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.05).
domain_priors:suppression_score(animal_status__property_reading, 0.12).
domain_priors:theater_ratio(animal_status__property_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, rope).
narrative_ontology:human_readable(animal_status__property_reading, "Animals as Legal Property Objects (Rights-Excluding Reading)").
narrative_ontology:topic_domain(animal_status__property_reading, "legal philosophy / applied ethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, 'faf911b6-20af-498f-a48d-fb3f34d2a8d3').
narrative_ontology:cs_kernel_codification('faf911b6-20af-498f-a48d-fb3f34d2a8d3', fixed_text).
narrative_ontology:cs_authority_grounding('faf911b6-20af-498f-a48d-fb3f34d2a8d3', lineage).
narrative_ontology:cs_interpretation_layer_present('faf911b6-20af-498f-a48d-fb3f34d2a8d3').
narrative_ontology:cs_reading_relation('faf911b6-20af-498f-a48d-fb3f34d2a8d3', animal_status__welfare_reading, influences).
narrative_ontology:cs_reading_relation('faf911b6-20af-498f-a48d-fb3f34d2a8d3', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('faf911b6-20af-498f-a48d-fb3f34d2a8d3', foundational, animals_lack_independent_moral_standing).
narrative_ontology:cs_axiom_status(animals_lack_independent_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('faf911b6-20af-498f-a48d-fb3f34d2a8d3', animals_lack_independent_moral_standing, conventional).
narrative_ontology:cs_axiom('faf911b6-20af-498f-a48d-fb3f34d2a8d3', foundational, property_rights_primacy_over_sentience).
narrative_ontology:cs_axiom_status(property_rights_primacy_over_sentience, holdable).
narrative_ontology:cs_axiom_grounding('faf911b6-20af-498f-a48d-fb3f34d2a8d3', property_rights_primacy_over_sentience, instrumental).
narrative_ontology:cs_reference_frame('faf911b6-20af-498f-a48d-fb3f34d2a8d3', common_law_property_doctrine).
narrative_ontology:cs_drift_state('faf911b6-20af-498f-a48d-fb3f34d2a8d3', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('faf911b6-20af-498f-a48d-fb3f34d2a8d3', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, human_economic_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status__property_reading, welfare_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own animals as property, use them instrumentally in agriculture, research, entertainment, and food production. Under this reading, they face no legal constraint from the animal itself — only from human-authored welfare statutes they can influence or modify. They define the boundary of acceptable treatment through legislative and common-law processes. The benefit is unrestricted property use subject only to human preferences and economic considerations.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, human_economic_actors, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_status__property_reading, human_economic_actors, agenda_setter).

% Push for stronger welfare statutes within the property framework, arguing that animals' sentience and capacity for suffering create prudential reasons to restrict human treatment. They cannot escape the underlying property logic — the constraint grants them standing only insofar as they can persuade human economic actors and legislatures. Their primary cost is the ongoing political friction of defending welfare rules against economic pressure to weaken them.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, welfare_advocates, payer,
    organized, biographical, constrained, national).

% Reject the entire property reading and argue animals hold independent rights precluding instrumental use. This reading's framework excludes them — it denies them standing to argue from animal interests directly. Their exclusion is structural: under property law, they can advocate for welfare amendments but cannot claim the constraint itself is illegitimate. They can exit into alternative moral communities or jurisdictions but not into the legal framework itself.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, abolitionist_movements, excluded,
    moderate, generational, mobile, global).

% Enact and modify welfare statutes within the property framework. They operationalize the boundary between permissible and impermissible treatment, always starting from the premise that animals are objects whose use is a human prerogative. Legislative dispute under this reading is never about whether animals may be used, but on what terms.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, legislators, agenda_setter,
    institutional, generational, analytical, national).

% Interpret and enforce property law and welfare statutes. They apply the property reading by default — treating animals as objects whose legal status derives entirely from human law, not from animal intrinsic characteristics. When welfare statutes conflict with property rights, courts typically resolve in favor of the property holder unless explicit statutory language forbids the treatment.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, legible framework for allocating animals as productive assets: owners know their legal entitlements, regulators know the scope of permissible treatment, and the system operates without ongoing philosophical adjudication of animal status.
% TRANSFER_FUNCTION: Transfers decision-making authority over animals' lives, bodies, and use from any agent that might claim to represent animal interests directly to human owners and legislatures. The constraint moves the locus of moral valuation from animals' own characteristics to human preferences, law, and economic interest.
% ABSENT_VOICES: Animals themselves cannot be parties to the legal arrangement (they cannot testify, contract, or claim rights). Abolitionist movements are present in public discourse but structurally excluded from the legal framework — they can advocate for amendments but cannot argue the property premise itself is legitimate. Indigenous and non-Western frameworks that recognize animal personhood or kinship are globally absent from most Western legal jurisdictions.
% DISAPPEARANCE_RATIONALE: Human property holders would experience immediate economic disruption if property claims were cancelled; welfare regulations would persist as human-to-human coordinative constraints (how we collectively treat objects we own). Some nations and communities might reorganize animal governance around rights frameworks; others would sustain property law. The verdict is contested because the constraint's actual role — whether it is doing essential coordination work or merely protecting entrenched economic interests — is exactly what the kernel contest disputes.
% FOUNDING_PROBLEM: Early human societies needed a legible way to control and allocate animals for subsistence, labor, and security. Property law provided this: a stable framework for establishing who controlled which animals and under what conditions.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and animal agriculture stakeholders attest the founding problem remains live — reliable animal allocation still matters for food security and economic activity. Abolitionist and animal-rights philosophers attest the founding problem was always misframed — it was never about animals' own interests but about human economic convenience, and that convenience cannot ethically override animal interests. Welfare advocates occupy middle ground: the founding problem had real force but has been superseded by knowledge of animal sentience.
narrative_ontology:disappearance_verdict(animal_status__property_reading, contested).
narrative_ontology:founding_problem_status(animal_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.05, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__property_reading_tests).
:- end_tests(animal_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely low (0.05) because this reading defines extractiveness on human parties only — the constraint coordinates human property ownership without imposing asymmetric extraction between humans. Suppression is low (0.12) because the property reading has deep institutional inertia and faces suppression only from abolitionist movements, which the constraint structurally excludes from its adjudicatory framework. Theater is minimal (0.08) and rises slowly over time: the property framework operates with little performative activity, though late-20th-century welfare regulations introduce some theater (animal welfare as moral gesture within economic relations). Accessibility collapse is low (0.15) because alternatives to property-based animal governance remain intellectually live and in practice (sanctuary movements, alternative legal systems), even if dominant law forecloses them. Resistance is high (0.72) because abolitionist and welfare movements mount sustained, visible opposition — though this resistance is structurally excluded from the property framework's own logic. The measurement series show stable low extractiveness and suppression from 1950–2026: the property reading's legal dominance has not intensified extraction pressure on humans; welfare regulations rose modestly without dislodging the core property premise.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental seat divergence is between human parties (for whom extractiveness is near-zero) and excluded parties (abolitionist movements) who experience the constraint as radically extractive — not as a neutral coordinative frame but as a mechanism that denies animals' moral standing and transfers their fate entirely to human prerogative. From the property-reading seat, this is not extraction but clarification: animals are objects, period. From the abolitionist seat, the entire reading is a cover story for institutionalized extraction from beings with independent interests. The engine cannot reconcile these — they do not share a common metric space. The high measured resistance (0.72) reflects this: sustained public opposition to the property reading itself, even though the reading's legal dominance means that opposition is structurally excluded from the law's own logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the property reading, animals do not appear in the directionality calculation at all — they are objects, not agents with interests. Human economic actors are structural beneficiaries (d near 0.0): they collect unrestricted use rights subject only to welfare constraints they can influence. Welfare advocates are slight targets (d ~0.35): they bear the cost of defending welfare rules against economic pressure and lack direct standing to argue from animal interests. Abolitionist movements are full targets (d ~0.95): the constraint's entire logic excludes their core premise and they cannot operate within its framework, only against it. Legislators sit near symmetric (d ~0.5): they maintain the system but can be pressured by welfare advocates to modify it incrementally.
 *
 * MANDATROPHY ANALYSIS:
 *   The property reading's founding problem — establishing legible control over animals for subsistence and labor — was live in 1800 and remains contested in 2026. Welfare advocates argue the founding problem has been superseded by knowledge of animal sentience and by economic diversification (mechanized agriculture reducing animal-labor dependence). Abolitionist movements argue the founding problem was never legitimate — it was always a rationalization for exploitation. The property reading itself denies mandatrophy: it asserts that human need for animal use is permanent and ineliminable, so the founding problem remains live by definition. The 'contested' verdict reflects this structural disagreement — the founding problem's status is precisely what the kernel dispute is about.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_foundation,
    'Is the treatment of animals as legal objects a necessary feature of property law itself, or a contingent historical choice that alternative legal frameworks could reject while retaining functional property systems?',
    'Comparative legal analysis of non-Western and alternative-justice systems that grant animals legal standing or personhood (e.g., New Zealand granting rivers personhood, Indigenous legal traditions recognizing animal kinship). If functional property allocation occurs in systems that deny the property-reading''s core premise, the choice is contingent, not necessary.',
    'If contingent, the property reading is a constructed constraint choice, not a natural law — it would move toward snare classification in comparative frames. If necessary, the constraint reflects inevitable features of property law, supporting mountain-adjacent status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_foundation, conceptual, 'Whether animal legal non-standing is intrinsic to property logic or a choice point.').

omega_variable(
    sentience_moral_relevance_kernel_contest,
    'Does animal sentience create independent moral standing that should constrain human property use, or is sentience a human-relevant consideration (for human welfare reasons) without granting animals direct claims?',
    'This is the core normative disagreement between property and welfare readings. No empirical data will resolve it — it is a value-premises clash. Resolution-as-contest: which reading wins institutional dominance in legislatures, courts, and global legal systems over the next 20–30 years?',
    'If welfare reading gains dominance, animals move into constraint stakeholder sets (as beneficiaries or victims). If abolitionist reading gains dominance, animals'' moral status fundamentally restructures the constraint set. If property reading maintains dominance, animals remain excluded objects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sentience_moral_relevance_kernel_contest, preference, 'Normative weighting of animal sentience in legal frameworks — core kernel dispute.').

omega_variable(
    welfare_statute_coordination_vs_extraction,
    'Are welfare statutes genuine coordination mechanisms (establishing shared standards for animal care that benefit both owners and animals), or extraction disguised as coordination (restrictions on property owners that primarily serve human moral signaling)?',
    'Empirical: examine whether welfare regulations improve animal welfare outcomes and reduce owner friction, or primarily impose costs on owners while animals experience unchanged suffering. Examine legislative intent and owner compliance patterns.',
    'If coordination, welfare statutes increase rope-ness of the overall arrangement. If extraction, they increase theater without reducing harm — piton dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_statute_coordination_vs_extraction, empirical, 'Whether welfare regulations coordinate or performatively restrict property use.').

omega_variable(
    reading_committer_ambiguity,
    'Is the property reading a coherent logical position, or does it depend on excluding animals'' own interests from moral consideration by stipulation rather than argument?',
    'Philosophical analysis: examine whether the property reading can defend the exclusion of animal interests on principled grounds, or whether it relies on asserting animal non-status rather than deriving it from defensible premises. Consult both property-reading and abolitionist philosophical literature.',
    'If the exclusion is stipulative (asserted but not derived), the reading''s logical rigor is lower, which affects its institutional resilience — it is maintained by inertia rather than argument. If the exclusion is principled, the reading has greater coherence-based legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'Whether animal non-standing is logically derived or stipulatively asserted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 1800, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1800, animal_status__property_reading, theater_ratio, 1800, 0.02).
narrative_ontology:measurement(anim_tr_t1900, animal_status__property_reading, theater_ratio, 1900, 0.03).
narrative_ontology:measurement(anim_tr_t1950, animal_status__property_reading, theater_ratio, 1950, 0.04).
narrative_ontology:measurement(anim_tr_t1975, animal_status__property_reading, theater_ratio, 1975, 0.06).
narrative_ontology:measurement(anim_tr_t2000, animal_status__property_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(anim_tr_t2026, animal_status__property_reading, theater_ratio, 2026, 0.08).

% Extraction over time
narrative_ontology:measurement(anim_be_t1800, animal_status__property_reading, base_extractiveness, 1800, 0.03).
narrative_ontology:measurement(anim_be_t1900, animal_status__property_reading, base_extractiveness, 1900, 0.04).
narrative_ontology:measurement(anim_be_t1950, animal_status__property_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(anim_be_t1975, animal_status__property_reading, base_extractiveness, 1975, 0.05).
narrative_ontology:measurement(anim_be_t2000, animal_status__property_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(anim_be_t2026, animal_status__property_reading, base_extractiveness, 2026, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1800, animal_status__property_reading, suppression_requirement, 1800, 0.05).
narrative_ontology:measurement(anim_su_t1900, animal_status__property_reading, suppression_requirement, 1900, 0.07).
narrative_ontology:measurement(anim_su_t1950, animal_status__property_reading, suppression_requirement, 1950, 0.08).
narrative_ontology:measurement(anim_su_t1975, animal_status__property_reading, suppression_requirement, 1975, 0.1).
narrative_ontology:measurement(anim_su_t2000, animal_status__property_reading, suppression_requirement, 2000, 0.11).
narrative_ontology:measurement(anim_su_t2026, animal_status__property_reading, suppression_requirement, 2026, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status__property_reading, 0.02).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three constraint stories, each representing a different reading. All three share the same kernel (animal status in human legal systems) but generate distinct constraints with different ε values, beneficiary/victim structures, and classifications. The property_reading treats animals as legal objects (ε ~0.05, humans as sole stakeholders). The welfare_reading treats animals as sentient beings with interests constraining human use (ε ~0.45, animals as beneficiaries, humans as payers/beneficiaries split by position). The abolitionist_reading treats animals as rights-holders precluding instrumental use (ε ~0.75, animals as victims, human exploiters as payers). These are not perspectives on one constraint — they are three distinct constraints that would obtain under different legal frameworks. Linked via network.affects_constraints to enable comparative analysis of kernel-reading family dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
