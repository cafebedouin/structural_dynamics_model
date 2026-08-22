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
 *   human_readable: UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   The hybrid effective control reading of UNCLOS maritime sovereignty
 *   establishes that natural geographic features generate full exclusive
 *   economic zones and territorial sea automatically, while artificial
 *   features constructed on submerged terrain or low-tide elevations generate
 *   only limited 500-meter safety zones — BUT these artificial features may
 *   mature into full territorial claims through prolonged effective control
 *   (administrative presence, infrastructure, military or civilian
 *   occupation) provided they remain unchallenged. This reading is one
 *   instantiation within a contested UNCLOS kernel where three major
 *   interpretations compete. The hybrid reading sits between strict
 *   geographic interpretation (artificial features never generate territory)
 *   and expansive construction reading (artificial features become islands
 *   immediately upon effective occupation). It represents a compromise that
 *   legitimates technological transformation of maritime geography while
 *   preserving some protection for initial claimants through the temporal
 *   requirement and challenge condition. The beneficiary structure is
 *   asymmetric: construction-capable states with capital, dredging
 *   technology, and regional power projection benefit from a framework that
 *   rewards capability and persistence; developing and militarily weaker
 *   claimants pay through loss of maritime jurisdiction and forced boundary
 *   concessions. The reading translates technical capability into legal
 *   sovereignty.
 *
 * KEY AGENTS:
 *   - Construction-capable states (China, Vietnam, major maritime powers): possess dredging fleets, capital, and technological infrastructure for artificial island projects; benefit from a reading that legitimates their construction-based territorial expansion.
 *   - Militarily weaker claimants (Philippines, smaller ASEAN members, developing maritime states): bear costs through loss of effective maritime jurisdiction and inability to contest occupation of disputed features.
 *   - Small island states and indigenous maritime populations: identity-locked in the constraint; exist as maritime powers only if natural features generate EEZ, and cannot exit without ceasing to be sovereign maritime actors.
 *   - UNCLOS strict interpreters (international courts, legal scholars, developing state delegations): argue the hybrid reading is a Trojan horse for annexation and advocate strict geographic limits.
 *   - UNCLOS expansive interpreters (construction advocates, some state legal advisors): view the hybrid reading as an insufficient compromise that underestimates technological and administrative legitimacy.
 *   - International maritime security actors (US Navy, NATO, regional navies): excluded from formal UNCLOS negotiation but contest the reading through military presence and freedom-of-navigation operations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.68).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.71).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, '79a05e94-4739-4143-81b1-581b9dd43e44').
narrative_ontology:cs_kernel_codification('79a05e94-4739-4143-81b1-581b9dd43e44', fixed_text).
narrative_ontology:cs_authority_grounding('79a05e94-4739-4143-81b1-581b9dd43e44', extraction).
narrative_ontology:cs_interpretation_layer_present('79a05e94-4739-4143-81b1-581b9dd43e44').
narrative_ontology:cs_reading_relation('79a05e94-4739-4143-81b1-581b9dd43e44', unclos_maritime_sovereignty__strict_geographic_reading, influences).
narrative_ontology:cs_reading_relation('79a05e94-4739-4143-81b1-581b9dd43e44', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('79a05e94-4739-4143-81b1-581b9dd43e44', foundational, natural_features_privileged_automatic_recognition).
narrative_ontology:cs_axiom_status(natural_features_privileged_automatic_recognition, holdable).
narrative_ontology:cs_axiom_grounding('79a05e94-4739-4143-81b1-581b9dd43e44', natural_features_privileged_automatic_recognition, deontological).
narrative_ontology:cs_axiom('79a05e94-4739-4143-81b1-581b9dd43e44', foundational, artificial_features_mature_through_effective_control_and_absence_of_challenge).
narrative_ontology:cs_axiom_status(artificial_features_mature_through_effective_control_and_absence_of_challenge, holdable).
narrative_ontology:cs_axiom_grounding('79a05e94-4739-4143-81b1-581b9dd43e44', artificial_features_mature_through_effective_control_and_absence_of_challenge, instrumental).
narrative_ontology:cs_reference_frame('79a05e94-4739-4143-81b1-581b9dd43e44', geographic_fixity_with_capability_maturation).
narrative_ontology:cs_drift_state('79a05e94-4739-4143-81b1-581b9dd43e44', contemporary_artificial_island_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('79a05e94-4739-4143-81b1-581b9dd43e44', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_power_projectors).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, developing_maritime_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, commercial_maritime_interests).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, small_island_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, commercial_maritime_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% China, Vietnam, Philippines, UAE, Singapore, and other states with capital, dredging fleets, and technological capacity to convert submerged features into artificial islands. They benefit from the hybrid reading because it legitimates their construction-based territorial expansion and justifies forward-deployed military infrastructure. They set the agenda by building islands first and forcing other states to react, while providing the legal framework (hybrid reading) that justifies their actions as lawful occupation rather than territorial aggression. Gain from extraction (territorial claims, resource rights, strategic depth) is captured directly by these states through administrative control and military presence on constructed features.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_states, agenda_setter).

% Smaller coastal states in the South China Sea, Southeast Asia, and other disputed maritime zones (Philippines, Vietnam in some disputes, Malaysia, Brunei, Taiwan) that claim overlapping EEZ based on their own natural features but lack construction capacity and military deterrence to occupy disputed submerged features first. They pay through loss of effective maritime jurisdiction in waters their geography initially granted them, forced maritime boundary concessions, and inability to exploit marine resources in areas now claimed as artificial-island territory by more powerful neighbors. They face a timing disadvantage: whoever builds an artificial island first and establishes 10-15 years of continuous presence may lock in territorial claims before weaker claimants can respond.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants, payer,
    moderate, biographical, constrained, regional).

% Low-income and developing maritime states without capital, technology, or naval capacity to build artificial islands or defend maritime claims militarily. They are forced to accept territorial subordination to constructed facts by more powerful neighbors. They pay through maritime access restriction (artificial islands blockade shipping routes), inability to contest resource claims (whoever builds the island controls nearby fishing and seabed resources), and dependence on whoever controls constructed features for passage rights and fishing agreements.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, developing_maritime_states, payer,
    powerless, biographical, trapped, regional).

% Military and geopolitical powers (China, India, Japan, Russia, ASEAN members with naval capability) using artificial island construction as a tool for force projection and extended maritime control. Artificial islands enable forward air bases, radar stations, and naval infrastructure at strategic positions, extending deterrence and control throughout exclusive economic zones and contested waters. The hybrid reading legitimates this military infrastructure as territorial sovereignty, not as provocative militarization. These actors benefit from the strategic depth and forward-deployed capability that artificial islands provide.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_power_projectors, beneficiary,
    institutional, generational, mobile, regional).

% International courts (particularly ITLOS and ICJ), legal scholars, state parties from the developing and small-island coalitions, and the UN Seabed Authority that advocate for strict geographic interpretation of UNCLOS Article 121. They argue that only naturally formed features generate territorial sea and EEZ, and artificial features cannot alter legal status regardless of effective occupation. They view the hybrid reading as a Trojan horse legitimating annexation and demand enforcement of strict reading to preserve territorial status quo and protect smaller claimants.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_strict_interpreters, observer,
    institutional, generational, analytical, global).

% Some international legal theorists, construction-capable state legal advisors, and scholars who argue that artificial islands built on submerged features become islands through effective occupation and generate full territorial sea and EEZ immediately upon establishing administrative control. They view the hybrid reading as too weak and restrictive, underestimating the legitimacy of technological development and effective state presence as grounds for sovereignty. They compete with hybrid advocates to justify the most expansive reading possible.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_expansive_interpreters, observer,
    institutional, generational, analytical, global).

% Sovereign island nations and Pacific island states whose entire territorial legitimacy and economic survival depend on the principle that natural features generate maritime zones. Identity is constituted through sovereignty over maritime space; they cannot exit the constraint because their existence as recognized maritime powers depends on the reading of UNCLOS they are trapped in. The hybrid reading threatens their identity by creating a two-tier system where their natural island sovereignty is junior to others' artificial construction, and they face marginalization in resource allocation and strategic power.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, small_island_states, payer,
    powerless, biographical, identity_locked, regional).

% Shipping companies, fishing corporations, and resource extraction industries that benefit from artificial islands providing ports, refueling infrastructure, military protection, and insurance cost reductions in disputed waters. They also pay through route disruptions, arbitrary enforcement, forced commercial partnerships with whoever controls constructed features, and navigational uncertainty. They are dual-positioned: they benefit from the infrastructure artificial islands provide, but they pay through arbitrary enforcement and exclusion from competing routes.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, commercial_maritime_interests, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, commercial_maritime_interests, payer).

% Naval powers and maritime security coalitions (US Navy, NATO allies, regional navies conducting freedom-of-navigation operations) that contest the hybrid reading because artificial islands with military installations threaten open-ocean deterrence and freedom of navigation. They argue that accepting artificial features as territory legitimates militarization of disputed waters and undermines their ability to project power and enforce international rules. They are excluded because their objections are not seated at the UNCLOS negotiation table but rather articulated through military presence, threat, and freedom-of-navigation operations.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_maritime_security_actors, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_states).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__hybrid_effective_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a procedural framework for resolving competing maritime territorial claims by distinguishing natural features (automatic full EEZ and territorial sea) from artificial features (limited safety zones with maturation potential). The framework attempts to coordinate state behavior under technological transformation: states can expand maritime claims through construction, but only through prolonged effective control and only where initial natural claimants do not actively contest the occupation.
% TRANSFER_FUNCTION: Transfers effective maritime jurisdiction, exclusive resource extraction rights, and chokepoint control from smaller/weaker claimants and initial natural-feature claimants to construction-capable states that build artificial islands and establish administrative/military presence. Specifically: (1) moves fishing and resource extraction rights to whoever constructs artificial islands in disputed waters; (2) moves navigational authority and strategic depth to states that build military or civilian infrastructure on constructed features; (3) converts technological and capital capacity into legal sovereignty—construction capability becomes a form of territorial power.
% ABSENT_VOICES: Indigenous and subsistence maritime populations have no seat and cannot contest claims that displace their historic fishing grounds. Developing maritime states lack diplomatic and legal capacity to challenge the interpretation despite bearing costs. Non-state maritime actors (environmental groups, fisheries communities, indigenous networks) are structurally excluded from UNCLOS negotiation and cannot shape how the reading evolves. International maritime security actors (US Navy, NATO regional navies) object to the reading but are excluded from formal UNCLOS negotiation seats; their objections are articulated through military presence rather than legal argument.
% DISAPPEARANCE_RATIONALE: If the hybrid reading ceased and the strict geographic reading became globally dominant, construction-capable states would face pressure to abandon artificial island projects or lose all territorial claim; constructed military bases would lose legal cover; maritime boundaries would revert to natural-feature-based claims; resource rights in disputed waters would shift back to initial natural claimants; strategic depth in chokepoints would return to geographic rather than constructed facts. The entire infrastructure of de facto control through artificial islands would face delegitimation.
% FOUNDING_PROBLEM: UNCLOS Article 121 left ambiguous whether artificial features—particularly artificial islands constructed on submerged terrain or low-tide elevations—could generate territorial sea and EEZ, or only safety zones. Traditional UNCLOS drafting did not anticipate modern dredging technology capable of transforming large areas of undersea topography into above-water inhabited territory. As construction-capable states began building artificial islands with ports and military installations in disputed waters, the question became urgent and practical: does technological capability to transform maritime geography also transform territorial claims?
% FOUNDING_PROBLEM_CORROBORATION: Construction-capable states and their legal advisors attest the problem is live: technological advancement creates new situations UNCLOS did not contemplate, and effective occupation should determine sovereignty. Small island nations, developing maritime states, and international legal scholars attest the problem is resolved by UNCLOS's own text (Article 121 classifies artificial islands as not islands) and the interpretation is being manipulated to justify annexation. The International Court of Justice (in the Philippines v. China case and related maritime boundary cases) has issued rulings and advisory opinions supporting geographic priority over constructed facts. The UN Seabed Authority and ITLOS have issued statements and technical analyses supporting restrictions on artificial island territorial claims. The International Maritime Organization and dozens of legal scholars and state delegations from outside the construction-capable bloc provide corroboration for the 'problem is resolved' view.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is intermediate (0.68 at interval end) because the reading creates graduated sovereignty: natural features get full recognition (zero extraction for those claimants), but artificial features generate limited initial rights with potential upgrade (high extraction for those trying to claim them, moderate for weaker initial claimants). The measurement series shows extraction rising steeply from t=0 to t=25 (0.42→0.68) as construction-capable states build more projects and establish longer effective control, then plateauing (0.68 from t=25 onward) as the reading becomes entrenched and the marginal territorial claims saturate. Suppression rises similarly (0.48→0.71) as enforcement of the reading requires active exclusion of rival claimants, denial of disputed-waters access to weaker states, and rejection of challenges to constructed facts. Theater ratio is moderate (0.42 at end): the reading has genuine coordination content (establishing a framework for resolution rather than pure conflict), but an increasing share of the enforcement activity is dedicated to defending contested constructed claims against challenge, not to negotiating neutral maritime boundaries. All three metrics are authored on a single shared time grid. Accessibility collapse (0.62) reflects that once states understand the hybrid reading, alternative interpretations (strict geographic, expansive construction, or a completely different legal regime) become technically difficult to achieve—the constructed facts on the ground make reversal expensive and collective action to challenge them is vulnerable to free-rider incentives. Resistance (0.58) is moderate because smaller claimants do actively contest the reading (diplomatic protests, ICJ filings, freedom-of-navigation operations by outside powers), even though they lack the capacity to physically dislodge artificial islands.
 *
 * PERSPECTIVAL GAP:
 *   The construction-capable agenda-setter (China, Vietnam, UAE, Singapore) perceives the hybrid reading as legitimate coordination: a legal framework that respects their technological capacity and rewards effective control through administrative and military presence. From their perspective, natural geography alone is an accident of nature and should not freeze maritime boundaries forever when capable states can transform undersea topography into usable territory. The beneficiary seats compute the constraint as genuinely coordinating (low d, net-positive valuation). Weaker claimants and smaller island states perceive the same reading as enforced extraction: a rule that converts capability into rights at their expense, replacing geographic lottery with a power-based hierarchy where those with capital and dredging fleets claim territory from under their feet. The payer seats compute the constraint as extractive (high d toward target end). Small island states carry particular stake-inversion: their entire maritime identity depends on natural features generating territory, so accepting the hybrid reading means accepting their subordination to others' artificial construction. The agenda-setter (construction-capable states) operates from an arbitrage exit position (they can deploy capacity globally, shift their construction projects if blocked in one region, or threaten military force if their claims are rejected). Weaker claimants are trapped or identity-locked (they cannot move their maritime zones, cannot build artificial islands to compete, and cannot exit by accepting a different legal regime because their sovereignty depends on the reading they are trapped in). The engine will compute different effective extraction (χ) for each seat from the same base ε and structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: construction_capable_states and regional_power_projectors carry role=beneficiary + secondary_role=agenda_setter (they both collect from and set the rules). They have institutional power, generational time horizon, and arbitrage exit options (can shift construction globally, invest in different regions, or deploy military deterrence). Their d derives from beneficiary declaration + arbitrage exit → d near 0.0 (full beneficiary end) → χ negative or near-zero (they gain from the constraint, not extract from it, at least not net). Victims: militarily_weaker_claimants and developing_maritime_states carry role=payer. They have moderate-to-powerless power atoms, biographical time horizons, and constrained/trapped exit options. Their d derives from victim declaration + low-exit → d near 1.0 (full target end) → χ amplified upward by directionality toward target (they bear the extraction, cannot escape). Small island states have a structural twist: role=payer but exit=identity_locked (they cannot exit without ceasing to be maritime sovereigns), and they have powerless power. Identity-locked status should derive d slightly higher toward the target end than mere trapped exit (identity-locked agents carry the constraint even after physical departure; they are bound by their self-conception as maritime powers). The agenda-setter's power (institutional, generational, arbitrage) contrasts sharply with payer power (moderate-to-powerless, biographical, trapped/identity-locked), producing strong per-seat divergence in computed d and χ.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading has no straightforward mandatrophy signal because both the coordination function (establishing a framework for maritime claim resolution) and the extraction function (converting capability into territory at the expense of weaker claimants) are present and real. The founding problem (ambiguous UNCLOS Article 121 interpretation) is contested but arguably still live: states continue to build artificial islands and dispute claims, international courts continue to receive filings, and the community of nations remains divided on the proper legal interpretation. However, there is a secondary mandatrophy signal in the theater ratio: the ratio rises from 0.28 to 0.42 (50% increase over the interval), suggesting that enforcement activity has shifted increasingly toward defending constructed claims against challenge rather than toward neutral maritime boundary negotiation. If theater continues rising toward 0.6+, the reading may be undergoing mandatrophy where the original coordination function (resolving ambiguity in UNCLOS) has been achieved (the interpretation is now established in state practice) and the remainder is mostly performance (defending constructed facts, rejecting reopening of challenges, excluding rival interpretation). A piton classification could be justified if the reading transitions to mostly-theatrical maintenance of the constructed-facts status quo, but at the present measurement point (theater=0.42, extraction=0.68) the constraint retains substantial real function and should remain tangled_rope rather than downgrade to piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effective_control_ambiguity,
    'What constitutes ''effective control'' sufficient for artificial features to mature into territorial claims? Is administrative presence alone sufficient, or does military occupation, economic infrastructure, or continuous population requirement?',
    'Case-by-case ICJ or ITLOS rulings on specific artificial island projects (South China Sea islands, artificially constructed atolls, etc.) would establish precedent; state practice over 10-15 years of island-building would reveal what actions courts and rival claimants accept as qualifying for maturation.',
    'A narrow definition of effective control (military occupation only) would slow the reading''s extractive operation; a broad definition (any administrative presence) would accelerate maturation and increase extraction for construction-capable states. This directly affects χ for weaker claimants: narrow definition keeps their contested territory in the safety-zone limbo longer; broad definition accelerates their loss of maritime jurisdiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effective_control_ambiguity, conceptual, 'The threshold for ''effective control'' that justifies maturation of artificial features from safety zones to territorial claims is undefined and hotly contested.').

omega_variable(
    challenge_and_abandonment,
    'What type of ''challenge'' resets the maturation clock for an artificial feature? Is diplomatic protest sufficient, military incursion, legal filing, or only formal state occupation? How long is the feature allowed to stand unchallenged before maturation becomes irreversible?',
    'Behavioral observation: track whether states that build artificial islands but face diplomatic or legal challenges treat them differently from those that face no opposition; international court precedent if any state seeks damages or injunctive relief for an artificial island built in their claimed EEZ; tracking of state statements on whether a feature remains ''contested'' if one state files an ICJ case vs. if no challenge occurs.',
    'If challenge is narrowly defined (only military force counts) and the maturation period is short (5-10 years), construction-capable states can safely build in disputed waters and convert them to territory before rivals can effectively respond. If challenge is broadly defined (including diplomatic protest, ICJ filing) and the maturation period is long (20-30 years), then persistent objection and legal action can prevent or delay maturation. This directly affects whether the reading functions as a fair coordination mechanism or as a time-limited grab mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(challenge_and_abandonment, empirical, 'The conditions under which an artificial feature is deemed ''challenged'' and therefore prevented from maturing into territorial claims remain unspecified in international practice.').

omega_variable(
    reading_foreclosure_vector,
    'Does the hybrid reading''s differentiation between natural and artificial features foreclose the expansive construction reading (they cannot coexist in one legal framework because hybrid privileges natural features over artificial), or do they coexist as competing readings held by different state blocs?',
    'Doctrinal analysis: if a state party formally adopts the hybrid reading, must it reject the expansive reading''s axiom that ''artificial features become islands through effective occupation''? Or can a state hold both readings in sequence (hybrid in formal legal argument, expansive in actual state practice)? If states can hold both, the readings coexist; if adoption of one requires rejection of the other''s core premise, the readings foreclose.',
    'If hybrid forecloses expansive, then the three readings occupy a foreclosure triplet (only one can prevail globally). If hybrid and expansive coexist despite their apparent contradiction, then multiple readings remain live simultaneously, creating persistent legal ambiguity. This affects the constraint''s persistence: a foreclosure structure means one reading will eventually become dominant and the others will be abandoned; a coexistence structure means the readings will remain contested indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_vector, conceptual, 'Whether the hybrid reading logically forecloses the expansive construction reading or whether both can coexist as competing interpretations held by different state coalitions.').

omega_variable(
    natural_feature_ambiguity,
    'What qualifies as a ''natural feature'' for purposes of automatic territorial sea generation? Does a low-tide elevation exposed naturally at low water qualify? What about natural features that are then artificially enhanced (e.g., a small natural rock artificially augmented with concrete)? Where is the boundary between ''natural'' and ''artificial''?',
    'UNCLOS Article 121 and implementing case law (ICJ, ITLOS decisions on island status) would need to define natural-feature status more precisely through precedent; geological surveys and feature classification systems would need to establish clear categories of natural vs. enhanced.',
    'A narrow definition of natural (only clearly naturally formed features at high tide) protects small claimants who own unambiguous natural features; a broad definition (any naturally emergent feature, even if enhanced) allows larger states to claim natural status for features they have artificially augmented. This affects how much territory falls into the ''automatic full EEZ'' category (natural) vs. the ''contestable maturation'' category (artificial).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_feature_ambiguity, empirical, 'The boundary between naturally formed maritime features (automatic territorial sea) and artificially constructed features (limited safety zones, contestable maturation) is undefined in edge cases.').

omega_variable(
    kernel_reading_coexistence,
    'This hybrid reading competes with strict_geographic and expansive_construction readings for dominance in the same UNCLOS kernel. Do the three readings coexist as live positions held by different state coalitions indefinitely, or does one reading eventually become hegemonic and displace the others?',
    'Long-term observation: track state practice over 20-30 years; monitor whether the three readings remain in active contention at UNCLOS conferences, ICJ filings, and state diplomatic statements, or whether one reading gradually becomes the de facto standard and the others are relegated to minority or historical positions.',
    'If readings coexist indefinitely, the maritime sovereignty constraint remains contestable and multi-interpretable—weaker claimants retain hope for revisiting the legal framework, and construction-capable states cannot settle their claims permanently. If one reading becomes hegemonic, the constraint hardens into stable law and claimants must accept the established interpretation. The hybrid reading''s stability depends on whether it can command sufficient consensus (construction-capable states + some international courts) to marginalize the other two readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Whether the three readings of the UNCLOS maritime sovereignty kernel coexist indefinitely or whether one reading gradually becomes hegemonic and displaces the others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_tr_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_tr_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_tr_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_tr_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_tr_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_tr_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_tr_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_tr_t35, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_tr_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_be_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_be_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_be_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_be_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_be_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_be_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_be_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_be_t35, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_be_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_su_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_su_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_su_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_su_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_su_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_su_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_su_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_su_t35, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(unclos_hybrid_eff_ctrl_su_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.12).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, south_china_sea_militarization).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, freedom_of_navigation_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-constraint family decomposing the contested UNCLOS Article 121 maritime sovereignty kernel. The three readings (strict_geographic, hybrid_effective_control, expansive_construction) share the same referent (how artificial features affect territorial claims) but instantiate different ε values and beneficiary/victim structures based on their different underlying legal premises. Each reading's ε reflects its own assessment of how extractive the standing maritime arrangement is under that interpretation—not a split over the same measurement, but three separate constraints assessed by their own logics. All three are linked via network.affects_constraints to capture their mutual influence: the strict reading's prominence would suppress the hybrid; the expansive reading's adoption would marginalize the hybrid; and the hybrid reading's entrenchment in state practice constrains how viable the other two interpretations can be going forward.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, institutional, 0.08).
constraint_indexing:directionality_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
