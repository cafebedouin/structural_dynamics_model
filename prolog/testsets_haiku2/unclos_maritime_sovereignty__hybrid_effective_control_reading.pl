% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__hybrid_effective_control_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__hybrid_effective_control_reading
 *   human_readable: UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading (Graduated Feature-Based Territoriality)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint embodies one contested reading of the UNCLOS maritime
 *   sovereignty kernel: the hybrid effective-control reading. Under this
 *   reading, naturally-formed features generate full territorial sea (12nm)
 *   and exclusive economic zone (200nm) automatically.
 *   Artificially-constructed features (islands, reefs, structures) generate
 *   only a 500m safety zone initially, but this safety zone can mature into
 *   full territorial jurisdiction through prolonged effective occupation and
 *   administrative control, absent active challenge by rival powers. The
 *   reading sits between strict geographic interpretation (artificial
 *   features never generate territory) and expansive construction
 *   interpretation (artificial features generate territory immediately upon
 *   effective control). It has become the de facto operating principle for
 *   maritime expansion by capable regional powers (China, India, Vietnam,
 *   others), though UNCLOS does not explicitly authorize the maturation
 *   mechanism. The constraint extracts maritime jurisdiction and resource
 *   access from weaker claimants, justifying this extraction through a
 *   coordination frame (UNCLOS provides a shared legal metric) that masks the
 *   asymmetric benefit to construction-capable powers.
 *
 * KEY AGENTS:
 *   - militarily_capable_regional_powers: Agenda-setter. Define the constraint through reclamation projects and effective presence. Benefit from graduated maturation timeline.
 *   - states_with_construction_capacity: Beneficiary. Can build artificial features and accumulate territorial claims over time.
 *   - militarily_weaker_claimant_states: Payer/victim. Natural-feature claims frozen at 12nm; lose maritime space as rivals expand artificial features.
 *   - coastal_communities_distant_from_power_centers: Payer/victim. Powerless; lose fishing rights as maritime jurisdiction expands into distant territories.
 *   - unclos_interpretation_authorities: Observer. Provide authoritative readings but limited enforcement power.
 *   - rival_maritime_claimants: Payer. Engage in arms racing and competing reclamation as artificial-feature expansion accelerates.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.62).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.71).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading (Graduated Feature-Based Territoriality)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a').
narrative_ontology:cs_kernel_codification('f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a', fixed_text).
narrative_ontology:cs_authority_grounding('f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a', extraction).
narrative_ontology:cs_interpretation_layer_present('f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a').
narrative_ontology:cs_reading_relation('f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a', unclos_maritime_sovereignty__strict_geographic_reading, coexists_with).
narrative_ontology:cs_reading_relation('f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a', unclos_maritime_sovereignty__expansive_construction_reading, influences).
narrative_ontology:cs_axiom('f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a', foundational, feature_type_sovereignty_gradient).
narrative_ontology:cs_axiom_status(feature_type_sovereignty_gradient, holdable).
narrative_ontology:cs_axiom_grounding('f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a', feature_type_sovereignty_gradient, conventional).
narrative_ontology:cs_axiom('f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a', foundational, prolonged_effective_control_matures_claims).
narrative_ontology:cs_axiom_status(prolonged_effective_control_matures_claims, holdable).
narrative_ontology:cs_axiom_grounding('f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a', prolonged_effective_control_matures_claims, empirically_contingent).
narrative_ontology:cs_axiom('f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a', secondary, unopposed_occupation_suffices_for_maturation).
narrative_ontology:cs_axiom_status(unopposed_occupation_suffices_for_maturation, holdable).
narrative_ontology:cs_axiom_grounding('f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a', unopposed_occupation_suffices_for_maturation, conventional).
narrative_ontology:cs_reference_frame('f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a', unclos_neutral_jurisdiction_baseline).
narrative_ontology:cs_drift_state('f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a', contemporary_great_power_reclamation_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('f07bcbd8-78fb-4c87-a2f0-7fc8f73b0e7a', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_capable_regional_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, coastal_communities_distant_from_power_centers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, expansionist_states_and_militaries).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, rival_maritime_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, commercial_maritime_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with naval capacity and construction technology set the rule through reclamation projects, administrative presence, and military occupation of artificial features. They interpret this reading as permitting artificial feature construction that matures into territorial claims over decades. They argue effective control demonstrates sovereignty intention.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_capable_regional_powers, agenda_setter,
    institutional, generational, arbitrage, regional).

% Technologically advanced states benefit from the graduated timeline: they can build artificial features, maintain them indefinitely, and accumulate territorial claims without immediate legal challenge so long as no rival power actively contests. The 500m safety zone provides a toehold; prolonged effective control without challenge upgrades to territorial jurisdiction.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity, beneficiary,
    powerful, generational, mobile, regional).

% States without naval capacity or construction capability bear the cost: their natural-feature claims are frozen at 12nm territorial sea and 200nm EEZ, while rival powers expand artificial-feature territorial waters through reclamation. They can protest, but lack enforcement capacity to challenge effective occupation. Their exit is diplomatic complaint or UNCLOS arbitration, which is slow and often unenforced.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimant_states, payer,
    moderate, biographical, constrained, regional).

% Fishing villages and coastal economies on territories of weaker claimants lose access to traditional fishing grounds as artificial features expand territorial claims controlled by distant power centers. They cannot exit the jurisdiction or challenge the expansion. Their voice is mediated through their state's diplomacy, which carries limited leverage.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, coastal_communities_distant_from_power_centers, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, coastal_communities_distant_from_power_centers, excluded).

% The Permanent Court of Arbitration, UNCLOS commissions, and academic international law interpreters assess whether effective control and feature type support graduated sovereignty. They provide authoritative readings of UNCLOS Article 60 and the definition of 'island' under Article 121. Their interpretations are non-binding but shape legitimacy frames.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_interpretation_authorities, observer,
    institutional, generational, analytical, global).

% Military establishments of construction-capable states benefit from territorial expansion: more maritime jurisdiction, extended EEZ claims, military staging bases in disputed waters. This reading legitimizes their construction programs by offering a maturation pathway.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, expansionist_states_and_militaries, beneficiary,
    institutional, generational, mobile, regional).

% Rival capable states (e.g., India vs. China, Vietnam vs. China) pay through compressed regional maritime space and accelerated arms racing: as one power constructs artificial features, rivals must respond with their own construction or military presence to contest claims, or accept permanent loss of jurisdiction.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, rival_maritime_claimants, payer,
    powerful, generational, constrained, regional).

% Shipping lines, fisheries, and offshore energy operators face expanding territorial claims and shifting jurisdiction. Under this reading, yesterday's neutral waters become tomorrow's foreign territorial seas, with unpredictable permitting, fee, and sovereignty regimes. They must navigate cumulative claim expansion.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, commercial_maritime_operators, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, commercial_maritime_operators, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_capable_regional_powers).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__hybrid_effective_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: UNCLOS Article 121 and the effective-control principle coordinate competing maritime claims by offering a metric (feature type + duration of occupation) for resolving which state holds territory and jurisdiction. The hybrid reading operationalizes this metric by grading sovereignty by feature type and maturation time, creating a shared framework states use to communicate sovereignty intent.
% TRANSFER_FUNCTION: Moves maritime jurisdiction and resource access from militarily weaker claimants and traditional maritime users to capable states with construction technology. Transfers economic zones, fishing rights, hydrocarbon exploration rights, and military staging authority from the broader regional or global commons into the exclusive control of the occupying power.
% ABSENT_VOICES: Small island nations, fishing communities dependent on traditional grounds, subsistence coastal populations, and non-state maritime actors (e.g., stateless maritime workers, indigenous sea peoples) have no seat at UNCLOS renegotiation. They would object to the maturation mechanism if empowered; their absence lets the reading persist unchallenged by those bearing the cost.
% DISAPPEARANCE_RATIONALE: If this constraint vanished (i.e., the effective-control maturation mechanism were removed), states would lose the legal pathway to expand artificial-feature claims over time. Existing artificial installations would revert to safety zones only. The incentive structure for reclamation projects would collapse, regional powers would reduce construction, and maritime space would restabilize to narrower territorial claims. The distribution of maritime jurisdiction would reorganize around feature type alone, without the control-duration upgrade path.
% FOUNDING_PROBLEM: UNCLOS created legal ambiguity: Article 121 defines 'island' as naturally formed land surrounded by water, and Article 60 permits artificial structures for economic purposes with only 500m safety zones. But it did not specify whether prolonged effective occupation of artificial features could upgrade them into territorial claims. Early claimants (1980s–2000s) faced legal uncertainty about whether their reclamation and occupation would be recognized as sovereignty. This reading emerged to resolve that uncertainty by offering a maturation mechanism: initial safety zone, then upgrade to territorial jurisdiction after effective control is demonstrated and unchallenged.
% FOUNDING_PROBLEM_CORROBORATION: Capable regional powers attest the problem is live: they point to their reclamation programs and effective presence as demonstrating sovereignty intent. Weaker claimants and UNCLOS arbitration bodies (Philippines v. China arbitration, 2016) attest the problem has been weaponized: the founding ambiguity has been filled by powerful states unilaterally declaring effective control, not by negotiated clarification. Independent international law scholars and maritime policy analysts document that the 'maturation pathway' is now the de facto rule among construction-capable states, even though UNCLOS does not explicitly authorize it — the reading has become customary practice despite textual ambiguity.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is 0.62: the constraint redistributes maritime jurisdiction and resources from weaker claimants to capable powers through a legal mechanism (effective control) that appears neutral but structurally favors those with construction capacity and naval power. The reading claims to operationalize UNCLOS Article 121 but actually fills textual ambiguity with a maturation pathway UNCLOS does not explicitly authorize. Suppression is 0.71: weaker claimants face military asymmetry preventing effective challenge; they can protest diplomatically but lack enforcement capacity. Theater ratio is 0.42: the constraint maintains genuine coordination framing (UNCLOS as shared metric), but an increasing share of state activity is defensive (rival powers building to preempt or contest claims) rather than solving the original coordination problem. The coercion grid shows rising suppression and stakes inflation from individual (coastal communities) to structural (system-level maritime jurisdiction). Resistance declines over time as initial objections to artificial-feature construction are normalized through prolonged unchallenged practice. Accessibility of alternatives collapses: weaker claimants lose options (diplomatic settlement, alternative maritime zones) as fait accompli expansion proceeds. The time series (0–25 year interval, roughly 2000–2025 and projected to 2050) models accelerating extraction as reclamation becomes normal practice and the maturation mechanism hardens through precedent.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter perspective (militarily capable regional power), this constraint appears as legitimate coordination: UNCLOS provides a shared legal framework, effective control is a verifiable principle, and maturation through prolonged occupation is a natural consequence of sovereignty. From the weaker-claimant perspective, the same constraint appears as extraction: UNCLOS provides the cover, but the maturation mechanism is unilaterally defined by capable powers, and their military asymmetry prevents any real challenge. The engine should compute per-seat classifications that show this divergence: capable powers see Rope or Tangled Rope, weaker claimants see Snare. The authored claim (Tangled Rope) reflects the committer's reading (neither beneficiary nor victim framing exclusively); the engine's per-seat computation should reveal the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable regional powers sit at d ≈ 0.0 (beneficiaries): they set the agenda, define effective control, and collect expanded maritime jurisdiction. Weaker claimants sit at d ≈ 0.95 (targets): they bear costs through compressed maritime space, lack enforcement options (trapped/constrained exit), and cannot exit the jurisdiction without abandoning their state territory. The reading creates asymmetric extraction precisely because feature type and duration of occupation are observable only to those with power to implement them — only capable powers can undertake reclamation; weaker claimants can only protest. This asymmetry is what the engine should compute from the beneficiary/victim declarations and power/exit atoms of the stakeholders.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (UNCLOS textual ambiguity about artificial features' legal status) was real in 1982–2000 but is now substantively dead: state practice has resolved it unilaterally through the de facto maturation mechanism. However, the constraint persists and is actively enforced (rising suppression_requirement over time), not merely inertial. This is not Piton (theater_ratio is 0.42, not >0.5, and beneficiary activity is expansion, not mere performance). The constraint is Tangled Rope at the committer level: it genuinely coordinates by providing a shared metric (effective control, feature type), but the coordination function has become secondary to the extraction function (widening maritime jurisdiction for capable powers). The six-questions mismatch (founding_problem_status=contested, disappearance_verdict=world_rearranges) identifies the constraint as potentially zombie: the founding problem is no longer the operative driver, yet removal would cause major reorganization. This is consistent with Tangled Rope when the balance tips toward extraction, not a Piton signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effective_control_threshold_ambiguity,
    'How much time and administrative presence constitute ''prolonged effective control absent challenge''? Is it 10 years, 20 years, one generation? Does passive acceptance by rival powers equal ''absent challenge,'' or must challenge be affirmative resistance?',
    'State practice analysis: track how long rival powers have tolerated each artificial feature before accepting or contesting it. Review UNCLOS commission rulings and bilateral maritime agreements for explicit duration thresholds. Monitor whether any major power establishes a red-line duration or presence threshold.',
    'If the threshold is short (5–10 years), artificial features mature rapidly into territorial claims, accelerating regional expansion. If long (30+ years) or requiring affirmative challenge, the constraint''s maturation pathway slows, reducing benefit to construction-capable powers and reducing extraction from weaker claimants.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effective_control_threshold_ambiguity, conceptual, 'The boundary between ''provisional effective control'' and ''mature territorial claim'' is textually undefined.').

omega_variable(
    feature_type_sovereignty_gradient_naturalness,
    'Is the graduated sovereignty model (natural features → full EEZ, artificial features → 500m → territorial if unchallenged) a natural-law reading of geography, or a constructed legal reading benefiting capable powers?',
    'Genealogical analysis: trace whether this gradient appears in pre-UNCLOS maritime law, or whether it is a novel interpretation inserted during UNCLOS negotiations or post-hoc. Compare treaty text to how states practice it. Identify which states'' interests the gradient serves.',
    'If natural/inevitable, the reading deserves Rope or even Mountain classification (coordination function). If constructed, it is Tangled Rope or Snare (extraction masked by coordination framing). This omega addresses potential false-summit structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feature_type_sovereignty_gradient_naturalness, empirical, 'Whether graduated feature-based sovereignty is a discovered principle or a constructed framing that benefits certain states.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the high measured suppression (0.71) structural (military power imbalance, absence of enforcement mechanisms for weaker claimants) or internalized (weaker states have internalized the illegitimacy of their claims or the acceptance of great-power expansion)?',
    'Post-exit observation: monitor whether weaker claimants who gain military capacity or international support (e.g., through alliance shifts) change their willingness to contest artificial features. If suppression persists despite capacity change, it is more internalized; if resistance rises with capacity, more structural.',
    'If internalized, the measured suppression persists even after structural barriers are removed, making the constraint more extractive than the structural measure suggests. If structural, removing military imbalance would increase resistance and challenge the constraint''s persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'The nature of suppression in maritime claimant states'' acceptance of great-power artificial-feature expansion.').

omega_variable(
    reading_family_kernel_contest,
    'This reading (hybrid_effective_control) coexists with two sibling readings: expansive_construction (artificial features generate de facto territorial waters through effective occupation alone, no maturation timeline required) and strict_geographic (only natural features generate territorial sea; artificial features never do). Which reading is the kernel actually supporting, and are the three readings genuinely coequal competitors or is one subsidiary?',
    'State practice analysis over the next 10 years: which reading do construction-capable powers implement? Do they move toward the expansive reading (faster claims), stay at hybrid (graduated timeline), or are pressured back toward strict geographic? Track UNCLOS commission and regional arbitration decisions.',
    'If state practice moves toward expansive, this reading may be undercut (intermediate position loses to the framing that serves the beneficiaries best). If strict geographic enforcement rises, this reading becomes the contested middle ground. The kernel structure itself may shift as power distributions change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_family_kernel_contest, conceptual, 'The stability of this reading within the kernel family as geopolitical power distributions shift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(uncl_tr_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(uncl_tr_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(uncl_tr_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(uncl_tr_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(uncl_tr_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(uncl_be_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(uncl_be_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(uncl_be_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(uncl_be_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(uncl_be_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(uncl_su_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(uncl_su_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(uncl_su_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(uncl_su_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(uncl_su_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 25, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(uncl_grid_01, unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(uncl_grid_02, unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse(class), 25, 0.72).
narrative_ontology:measurement(uncl_grid_03, unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(uncl_grid_04, unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse(individual), 25, 0.74).
narrative_ontology:measurement(uncl_grid_05, unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse(organizational), 0, 0.52).
narrative_ontology:measurement(uncl_grid_06, unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse(organizational), 25, 0.68).
narrative_ontology:measurement(uncl_grid_07, unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse(structural), 0, 0.45).
narrative_ontology:measurement(uncl_grid_08, unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse(structural), 25, 0.62).
narrative_ontology:measurement(uncl_grid_09, unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(uncl_grid_10, unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance(class), 25, 0.44).
narrative_ontology:measurement(uncl_grid_11, unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance(individual), 0, 0.72).
narrative_ontology:measurement(uncl_grid_12, unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance(individual), 25, 0.38).
narrative_ontology:measurement(uncl_grid_13, unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance(organizational), 0, 0.64).
narrative_ontology:measurement(uncl_grid_14, unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance(organizational), 25, 0.52).
narrative_ontology:measurement(uncl_grid_15, unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance(structural), 0, 0.58).
narrative_ontology:measurement(uncl_grid_16, unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance(structural), 25, 0.48).
narrative_ontology:measurement(uncl_grid_17, unclos_maritime_sovereignty__hybrid_effective_control_reading, stakes_inflation(class), 0, 0.52).
narrative_ontology:measurement(uncl_grid_18, unclos_maritime_sovereignty__hybrid_effective_control_reading, stakes_inflation(class), 25, 0.68).
narrative_ontology:measurement(uncl_grid_19, unclos_maritime_sovereignty__hybrid_effective_control_reading, stakes_inflation(individual), 0, 0.58).
narrative_ontology:measurement(uncl_grid_20, unclos_maritime_sovereignty__hybrid_effective_control_reading, stakes_inflation(individual), 25, 0.74).
narrative_ontology:measurement(uncl_grid_21, unclos_maritime_sovereignty__hybrid_effective_control_reading, stakes_inflation(organizational), 0, 0.45).
narrative_ontology:measurement(uncl_grid_22, unclos_maritime_sovereignty__hybrid_effective_control_reading, stakes_inflation(organizational), 25, 0.62).
narrative_ontology:measurement(uncl_grid_23, unclos_maritime_sovereignty__hybrid_effective_control_reading, stakes_inflation(structural), 0, 0.38).
narrative_ontology:measurement(uncl_grid_24, unclos_maritime_sovereignty__hybrid_effective_control_reading, stakes_inflation(structural), 25, 0.51).
narrative_ontology:measurement(uncl_grid_25, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression(class), 0, 0.62).
narrative_ontology:measurement(uncl_grid_26, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression(class), 25, 0.78).
narrative_ontology:measurement(uncl_grid_27, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression(individual), 0, 0.68).
narrative_ontology:measurement(uncl_grid_28, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression(individual), 25, 0.82).
narrative_ontology:measurement(uncl_grid_29, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression(organizational), 0, 0.55).
narrative_ontology:measurement(uncl_grid_30, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression(organizational), 25, 0.72).
narrative_ontology:measurement(uncl_grid_31, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression(structural), 0, 0.48).
narrative_ontology:measurement(uncl_grid_32, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression(structural), 25, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.12).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_maritime_arms_race).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, south_china_sea_artificial_island_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is part of the unclos_maritime_sovereignty kernel family. The strict_geographic_reading (UNCLOS Article 121 read literally: artificial features never generate territory) and the expansive_construction_reading (artificial features generate territory immediately) are structurally distinct constraints with different ε, different beneficiary/victim structures, and different contested stakes. The hybrid reading sits between them. All three readings share the same kernel (UNCLOS text, 1982), but diverge on interpretation of 'island,' 'artificial,' and 'effective control.' This file authors only the hybrid reading as a clean ε-invariant constraint. The other readings are separate story files. Links via network.affects_constraints route containment and dependency analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, moderate, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
