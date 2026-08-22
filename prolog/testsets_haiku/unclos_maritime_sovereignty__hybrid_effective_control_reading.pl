% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   domain: international_law/maritime_governance/geopolitics
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested UNCLOS maritime
 *   sovereignty kernel—the hybrid effective control reading. It models a
 *   graduated sovereignty framework where natural geographic features
 *   automatically generate full exclusive economic zones (EEZ) and
 *   territorial seas, while artificial features (islands constructed on
 *   submerged rocks, reefs, or low-tide elevations) generate only 500m safety
 *   zones initially, but may mature into full territorial claims through
 *   prolonged effective occupation absent timely challenge. This reading
 *   benefits states with construction capacity and regional military
 *   superiority (who can build, occupy, and hold territory), and extracts
 *   from weaker claimants (who cannot occupy or build to match stronger
 *   claims) and distant maritime actors (who lose commons access). The
 *   constraint is structurally a Tangled Rope: it coordinates maritime
 *   governance under a rule-based graduated framework (coordination
 *   function), while simultaneously transferring maritime control rights from
 *   weaker to stronger claimants through the occupation-ripening mechanism
 *   (extraction function). The measurement series show extractiveness rising
 *   (0.48→0.62 over 20 years observed, continuing to 0.67 at year 35
 *   projected before declining to 0.62 at year 40 as contestation increases),
 *   suppression rising as the framework hardens enforcement expectations
 *   (0.55→0.71 observed), and theater rising modestly (0.38→0.48) as
 *   performative sovereignty demonstrations increase alongside effective
 *   occupation.
 *
 * KEY AGENTS:
 *   - States with Construction Capacity (institutional, global reach, arbitrage-level mobility): China, Japan, Vietnam, Philippines, Indonesia. Benefit from graduated framework because engineering capacity converts to sovereignty claims.
 *   - Regional Power Projectors (powerful, generational horizon, regional scope): militarily superior states that occupy and hold artificial features to accumulate occupational facts. Benefit from the ripening mechanism.
 *   - Militarily Weaker Claimants (moderate power, biographical horizon, constrained exit): Vietnam, Philippines, smaller Southeast Asian states. Bear costs because stronger neighbors can build unopposed and accumulate claims.
 *   - Distant Maritime Actors (organized, biographical horizon, global scope): commercial shipping, distant fishing nations, passage-dependent economies. Pay diffusely through restricted access and compressed commons.
 *   - UNCLOS Authority (institutional, generational, universal scope): International Court of Justice, treaty bodies, dispute settlement panels. Enforce the hybrid reading by recognizing claims that fit it.
 *   - Military Rivals (powerful, generational, regional scope): States that could challenge construction but face political/military costs for doing so; excluded from the conversation by the absence-of-challenge mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.62).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.71).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitics").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, '1b7106fc-6c54-448f-99c9-26fcdda74f5e').
narrative_ontology:cs_kernel_codification('1b7106fc-6c54-448f-99c9-26fcdda74f5e', fixed_text).
narrative_ontology:cs_authority_grounding('1b7106fc-6c54-448f-99c9-26fcdda74f5e', extraction).
narrative_ontology:cs_interpretation_layer_present('1b7106fc-6c54-448f-99c9-26fcdda74f5e').
narrative_ontology:cs_reading_relation('1b7106fc-6c54-448f-99c9-26fcdda74f5e', unclos_maritime_sovereignty__strict_geographic_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b7106fc-6c54-448f-99c9-26fcdda74f5e', unclos_maritime_sovereignty__expansive_construction_reading, influences).
narrative_ontology:cs_axiom('1b7106fc-6c54-448f-99c9-26fcdda74f5e', foundational, artificial_features_dual_status).
narrative_ontology:cs_axiom_status(artificial_features_dual_status, holdable).
narrative_ontology:cs_axiom_grounding('1b7106fc-6c54-448f-99c9-26fcdda74f5e', artificial_features_dual_status, empirically_contingent).
narrative_ontology:cs_axiom('1b7106fc-6c54-448f-99c9-26fcdda74f5e', foundational, effective_control_ripening_mechanism).
narrative_ontology:cs_axiom_status(effective_control_ripening_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('1b7106fc-6c54-448f-99c9-26fcdda74f5e', effective_control_ripening_mechanism, conventional).
narrative_ontology:cs_reference_frame('1b7106fc-6c54-448f-99c9-26fcdda74f5e', graduated_maritime_sovereignty_by_feature_type).
narrative_ontology:cs_drift_state('1b7106fc-6c54-448f-99c9-26fcdda74f5e', contemporary_artificial_island_accumulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1b7106fc-6c54-448f-99c9-26fcdda74f5e', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_power_projectors).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, distant_maritime_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% China, Japan, Vietnam, Indonesia, and other states with engineering and financial capacity to construct artificial features on submerged or low-tide elevations. They benefit because the hybrid reading permits their construction to generate immediate 500m safety zones and potentially full territorial claims if occupied unopposed for 15-25 years. They can choose where to build (arbitrage: they can build in favorable locations or refrain from building in unfavorable ones). They set the agenda by deciding when and where to initiate construction projects. The constraint amplifies their existing regional power by converting it into legal territory.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity, beneficiary,
    institutional, generational, arbitrage, global).

% Militarily superior states that can hold artificial features against potential challenges. They project power by occupying contested waters, establishing administrative presence, and accumulating the occupational facts that trigger the maturation mechanism. They enforce the constraint by maintaining military garrisons, conducting administrative activities in disputed zones, and resisting diplomatic or military challenges to their occupation. Their mobile exit reflects that they can redirect construction investment elsewhere if one location becomes too contested.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_power_projectors, agenda_setter,
    powerful, generational, mobile, regional).

% Vietnam, Philippines, smaller ASEAN states with legitimate geographic features but insufficient military or construction capacity to compete. They bear costs because stronger neighbors can build artificial features and occupy them unopposed, accumulating claims under the hybrid reading's graduated framework. They cannot exit the constraint because they are geographically tied to the region (constrained exit). Challenging construction requires military intervention (expensive, risky, diplomatically costly). They lose maritime territory to occupation-based claims they cannot prevent.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants, payer,
    moderate, biographical, constrained, regional).

% Commercial shipping companies, fishing nations, maritime trading partners with no regional presence. They bear diffuse costs through restricted passage (500m safety zones enforce navigation restrictions), compressed fishing zones, and increased transit costs. Their exit is geographically constrained (they cannot avoid the South China Sea or other contested waters without major rerouting). The constraint narrows open ocean commons into exclusive zones controlled by regional powers.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, distant_maritime_actors, payer,
    organized, biographical, constrained, global).

% International Court of Justice, treaty bodies, dispute resolution panels, and the epistemic community of international law scholars and practitioners. They interpret and enforce the hybrid reading by recognizing state claims that fit it, issuing advisory opinions, and applying the graduated framework in disputes. They maintain the constraint by documenting state practice, issuing clarifications about what constitutes 'effective control,' and reinforcing the natural/artificial distinction. They hold analytical exit (they can revise the interpretation if it becomes unjust or unworkable).
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_authority_structures, agenda_setter,
    institutional, generational, analytical, universal).

% States that could militarily challenge artificial construction and occupation but face political costs (reputational damage, escalation risk, international opposition) or diplomatic isolation that prevents them from mounting timely challenges. They are excluded from the conversation because the hybrid reading's mechanism depends on their silence or ineffective objections. The longer they remain silent, the faster occupation ripens into legal claims. They are trapped: challenging is costly, but not challenging is also costly (loss of maritime territory). Their exclusion is what permits the maturation mechanism to function.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_superior_neighbors, excluded,
    powerful, generational, trapped, regional).

% Environmental organizations, maritime rights monitors, human rights observers, and international media. They document occupational facts, track claim accumulation, monitor military activities in disputed zones, and report on consistency of state practice with the hybrid reading. They have analytical exit (they can change what they observe, question the framework's legitimacy). Their role is observational, not enforcing; they shape the legitimacy discourse without setting the formal agenda.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, ngos_and_international_observers, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__hybrid_effective_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a graduated maritime sovereignty framework that permits natural geographic features to automatically generate full exclusive economic zones (EEZ) and territorial seas under UNCLOS, while allowing artificial features to generate limited 500m safety zones and conditional territorial claims based on duration of effective occupation without timely challenge. This replaces chaos with rule-based governance where maritime boundaries are determined by recognizable criteria (natural/artificial distinction, occupational duration, absence of challenge) rather than raw military power deployed on a case-by-case basis.
% TRANSFER_FUNCTION: Transfers maritime control rights from geographically entitled weaker claimants who cannot occupy contested features to construction-capable states who can build artificial features and occupy them militarily. Also transfers diffuse maritime commons access from distant maritime actors into exclusive zones controlled by regional powers. The transfer mechanism is the occupational-ripening provision: if a state builds an artificial feature and maintains effective occupation for 15-25 years without timely military or legal challenge, it gains full territorial claims. This converts engineering capacity, military superiority, and absence of challenge into legal sovereignty.
% ABSENT_VOICES: Militarily weaker claimants who cannot mount timely challenges are excluded by the constraint's own mechanism—their silence is required for occupation to ripen. Distant maritime actors relying on open commons access are excluded from the treaty-negotiation table; their interests in passage and fishing access are not represented. States that could challenge occupation militarily but face diplomatic isolation are excluded by political costs that prevent them from voicing objection. The hybrid reading itself privileges the voices of construction-capable, militarily superior states over those unable to build or defend.
% DISAPPEARANCE_RATIONALE: If the hybrid effective control reading disappeared and maritime sovereignty reverted to strict geographic principles (only natural features generate full EEZ, artificial islands are permanently limited to 500m zones), the geopolitical map would reorganize: artificial islands constructed in contested waters would lose their path to territorial sovereignty, existing occupational claims would be voided, military garrisons on artificial features would lose legal justification, and regional power competition would shift toward natural-feature occupation and alliance-formation instead of construction-driven territorial accumulation. Maritime commons would partially reopen as exclusive zones lose legal grounding.
% FOUNDING_PROBLEM: UNCLOS III negotiations and post-treaty practice encountered the problem of submerged and low-tide geographic features (rocks, reefs, shoals, low-tide elevations) in disputed maritime regions. The treaty's language (Articles 60, 121-133) did not explicitly address whether artificial construction on such features could generate territorial claims, and no consensus existed on how to treat artificially created land. The founding problem was establishing stable maritime boundaries while accounting for technological innovation in feature utilization and maintaining the distinction between naturally entitled features and human-made constructions.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and treaty bodies (including International Court of Justice opinions and UNCLOS commission documents) acknowledge that the problem of artificial features and occupation-based maturation remains live and disputed. China invokes the hybrid reading to justify its artificial island construction and territorial claims; Philippines, Vietnam, and other claimants invoke the strict geographic reading to deny legitimacy to those claims. The United States and distant maritime nations invoke principles of open passage and strict geographic limitation. No authoritative outside source confirms which reading reflects the treaty's true intent; instead, multiple interpretations coexist in state practice, indicating the founding problem is not resolved.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.62 final): The hybrid reading extracts because it permits occupation duration to substitute for geographic entitlement. A naturally formed island generates full EEZ instantly; an artificial island generates a 500m zone instantly but full sovereignty if held unopposed for 15-25 years. This converts construction investment and military capacity into legal sovereignty gain—a transfer from those who cannot build or defend to those who can. The measurement trajectory (0.48→0.62 observed over first 20 years, continuing to project 0.67 at year 35) reflects increasing accumulation of occupied artificial features approaching maturation thresholds; the decline to 0.62 at year 40 reflects increasing contestation and legal challenges that raise uncertainty about whether occupation will continue to ripen. Suppression (0.71 final): The framework requires active enforcement of the occupation-ripening mechanism. Weaker claimants must be deterred from challenging construction (military suppression). Distant maritime actors must accept restricted passage through claimed zones (institutional suppression). The rising trajectory (0.55→0.71 over 20 years observed, plateauing at 0.76 at year 30 projected) reflects hardening enforcement as states build administrative infrastructure, deploy military presence, and assert control within claimed zones. Theater (0.48 final): The occupational activity is substantially functional (establishing administrative presence, building infrastructure, maintaining military garrison), but a growing fraction is performative: sovereignty demonstrations, flag-planting ceremonies, symbolic occupation displays meant to create documentary evidence of effective control. The slow rise (0.38→0.48 observed, reaching 0.52 at year 35 projected) reflects the increasing documentary burden: as the ripening threshold approaches, states intensify performative activity to build the legal record of uninterrupted occupation.
 *
 * PERSPECTIVAL GAP:
 *   From the construction-capable state's seat, the hybrid reading is coordination: it permits governance of a previously lawless commons by establishing rule-based criteria (graduated thresholds) rather than naked power competition. From the weaker claimant's seat, the same framework is extraction: it converts engineering capacity and military dominance into sovereignty, leaving geographical entitlement disadvantaged. From the distant maritime actor's seat, the framework is suppression: it compresses open ocean into exclusive zones. The engine computes these divergent classifications from directionality—construction-capable states get low d (beneficiaries), weaker claimants get high d (targets), distant actors sit near symmetric depending on their actual exit options (some can arbitrage routes, some are trapped in the compressed space). The authored claim (Tangled Rope) acknowledges that the same structure serves both coordination and extraction; the metrics quantify how extraction-heavy the actual operation has become.
 *
 * DIRECTIONALITY LOGIC:
 *   Construction-capable states (d near 0.0, beneficiary): They set the agenda (decide when to build, where to occupy, how to document possession). They benefit directly (gain sovereignty claims). They hold arbitrage exit (can choose where to build, can apply the framework selectively). Beneficiary directionality is low d. Militarily weaker claimants (d near 1.0, target): They bear costs (lose maritime space to stronger neighbors' artificial occupation). Their exit is constrained (they can challenge only at military cost; they cannot arbitrage to another ocean). They derive no coordinate benefit (their own geographical features receive no boost from the graduated framework; natural features generate full EEZ regardless, so the reading does not benefit them). Target directionality is high d. Distant maritime actors (d near 0.5, symmetric): They benefit from the framework's coordination aspect (stable rules instead of chaos), but they bear diffuse costs (restricted passage, fishing zone compression). Their exit is constrained (they cannot avoid the zones), but it is also geographically mobile (they can reroute around claimed waters). Symmetric directionality is near 0.5. UNCLOS authority (d=0.5 analytical): They are neutral interpreters; they do not collect from the constraint or bear its costs. The engine will mark them as analytical-power observatory.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to govern underwater features and permit technological innovation in feature utilization) is LIVE for the benefiting parties (construction-capable states) because occupation-ripening rewards them for investing in artificial features. It is DEAD for weaker claimants: they see the mechanism as a framework that locks in inequality (strong neighbors can build and claim territory; weak claimants cannot). The hybrid reading's mandate was to establish a stable rule-based maritime order; this mandate is increasingly challenged by the gap between the reading's assumption (occupation will be clearly effective and unchallenged) and reality (many occupations are disputed, occupation duration becomes a subject of diplomatic and legal contest, and the duration threshold is unclear). The constraint has not yet become a piton (it is still functionally enforced, not theatrically maintained), but the rising theater_ratio (0.38→0.48) and the contestation evident in the measurement decline at year 40 indicate mandatrophy pressure: the founding problem (stable governance) has been partially superseded by a new problem (determining what counts as 'effective control' and 'absence of challenge'). Tangled Rope classification holds because the coordination and extraction components remain coupled; if they separate (occupation-ripening is severed as a mechanism), the constraint reverts to pure rope (graduated zones) or splits into separate constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effective_control_definition_ambiguity,
    'What constitutes ''effective control'' sufficient for occupation-ripening into territorial claims? Is administrative presence, military garrison, civilian settlement, or flag-planting sufficient, or is some combination required? Does the definition depend on the feature type, regional geography, or the sophistication of the occupying state''s infrastructure?',
    'International Court of Justice guidance in specific maritime disputes (e.g., Philippines v. China recamation claims) that adjudicate whether particular occupations meet the ''effective control'' threshold. State practice documentation and legal scholarship consensus on operational criteria.',
    'If effective control is defined narrowly (requires civilian settlement, economic activity, continuous presence), fewer artificial features will ripen into claims, extraction decreases, and weaker claimants retain more competitive space. If defined broadly (flag-planting and administrative presence suffice), more features ripen faster, extraction increases, and the mechanism accelerates benefit to construction-capable states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effective_control_definition_ambiguity, empirical, 'The operational definition of ''effective control'' determines the maturation rate of artificial occupation into territorial claims.').

omega_variable(
    timely_challenge_threshold,
    'How long is ''prolonged effective control absent challenge'' required for maturation? Is it 15 years, 20 years, indefinite? Does the challenge need to be diplomatic objection, legal filing, or military intervention? If one regional state objects but others remain silent, is the challenge ''timely'' enough to reset the counter?',
    'Dispute settlement decisions, treaty interpretation guidance, and state practice documentation showing how long occupations have persisted without legal challenge and what forms of objection reset the clock.',
    'A shorter threshold (10 years) accelerates extraction and ripening; a longer threshold (30+ years) keeps occupation in legal limbo and extends the high-extraction period. Ambiguity about what counts as timely challenge incentivizes weak claimants to object even when they cannot militarily enforce the objection (escalating theater and performative sovereignty demonstrations), increasing suppression_requirement and theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(timely_challenge_threshold, empirical, 'The duration threshold and challenge criteria for occupation-ripening determine how fast extraction converts into legally secured sovereignty.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Are the hybrid reading and the strict geographic reading logically foreclosing (no single legal framework could hold both), or do they coexist as alternative interpretations invoked by different state parties?',
    'Test case: a state invokes strict geographic reading for a neighbor''s artificial island, then invokes hybrid reading for its own. If international bodies flag this as incoherent and force the state to choose, foreclosure is evidenced. If states maintain dual positions without legal censure, coexistence is evidenced.',
    'If readings foreclose, one will eventually dominate and the other will be classified as overridden. If they coexist, the ambiguity itself becomes a feature of the constraint—different states remain in different interpretive communities, which increases theater (performative legal arguments) and suppression (each state enforces its own reading against others). Coexistence favors construction-capable states (they can invoke the hybrid reading opportunistically and escape strict geographic limits).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the hybrid effective control reading forecloses or coexists with the strict geographic reading determines if maritime law is converging or stratified.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is suppression of weaker claimants'' challenges structural (military inability to enforce objections, geographic isolation, political isolation preventing alliance formation) or internalized (weaker states have accepted the hybrid reading as legitimate law and do not mount challenges because they believe it is correct)?',
    'Post-acceptance context: if a weaker claimant''s capacity changed suddenly (military acquisition, alliance formation, new leadership), would they challenge occupation that was previously accepted? If they would, suppression was structural; if they would not, it has become internalized. Evidence from statements by weak claimants'' leaders about whether they view the hybrid reading as unjust or as legitimate law.',
    'If structural, the constraint depends on military imbalance and can be destabilized by power shifts. If internalized, the constraint is resilient to power shifts but reflects deeper hegemonization. The measurement series (rising suppression 0.55→0.71, plateauing theater 0.48) is consistent with internalization: states perform acceptance through silence and legal documents rather than mounting active resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression reflects structural barriers (military disadvantage) or internalized acceptance (reading legitimacy) determines the constraint''s long-term stability.').

omega_variable(
    kernel_reading_reading_natural_vs_constructed,
    'Is the hybrid effective control reading a natural law reading of UNCLOS (it describes what the treaty genuinely requires) or a constructed reading (it reflects one faction''s interpretation that benefits them and has become canonical through power, not through textual fidelity)?',
    'Textual analysis: does the UNCLOS treaty explicitly endorse the natural/artificial distinction and the maturation mechanism, or are these interpretations added by the reading community? Historical analysis: did the treaty drafters intend this reading, or did it emerge post-hoc through state practice? Alternative reading analysis: do the strict geographic and expansive construction readings have equal textual support?',
    'If natural law, the hybrid reading is a mountain—extractiveness is coincidental, not structural. If constructed, the reading is a deliberate framework that benefiting parties have institutionalized, and extractiveness is the mechanism''s purpose. This feeds the broader FSM (false summit mountain) question: is graduated sovereignty a natural principle or a beneficiary-constructed reading?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_reading_natural_vs_constructed, conceptual, 'Whether the hybrid reading reflects natural UNCLOS requirements or constructed interpretation determines if the constraint is a mountain or a tangled rope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(uncl_tr_t0, observed).
narrative_ontology:measurement(uncl_tr_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement_basis(uncl_tr_t5, observed).
narrative_ontology:measurement(uncl_tr_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement_basis(uncl_tr_t10, observed).
narrative_ontology:measurement(uncl_tr_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement_basis(uncl_tr_t15, observed).
narrative_ontology:measurement(uncl_tr_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(uncl_tr_t20, observed).
narrative_ontology:measurement(uncl_tr_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement_basis(uncl_tr_t25, projected).
narrative_ontology:measurement(uncl_tr_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 30, 0.51).
narrative_ontology:measurement_basis(uncl_tr_t30, projected).
narrative_ontology:measurement(uncl_tr_t35, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 35, 0.52).
narrative_ontology:measurement_basis(uncl_tr_t35, projected).
narrative_ontology:measurement(uncl_tr_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(uncl_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(uncl_be_t0, observed).
narrative_ontology:measurement(uncl_be_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(uncl_be_t5, observed).
narrative_ontology:measurement(uncl_be_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(uncl_be_t10, observed).
narrative_ontology:measurement(uncl_be_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(uncl_be_t15, observed).
narrative_ontology:measurement(uncl_be_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(uncl_be_t20, observed).
narrative_ontology:measurement(uncl_be_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 25, 0.63).
narrative_ontology:measurement_basis(uncl_be_t25, projected).
narrative_ontology:measurement(uncl_be_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(uncl_be_t30, projected).
narrative_ontology:measurement(uncl_be_t35, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement_basis(uncl_be_t35, projected).
narrative_ontology:measurement(uncl_be_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(uncl_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(uncl_su_t0, observed).
narrative_ontology:measurement(uncl_su_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(uncl_su_t5, observed).
narrative_ontology:measurement(uncl_su_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(uncl_su_t10, observed).
narrative_ontology:measurement(uncl_su_t15, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(uncl_su_t15, observed).
narrative_ontology:measurement(uncl_su_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(uncl_su_t20, observed).
narrative_ontology:measurement(uncl_su_t25, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 25, 0.74).
narrative_ontology:measurement_basis(uncl_su_t25, projected).
narrative_ontology:measurement(uncl_su_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement_basis(uncl_su_t30, projected).
narrative_ontology:measurement(uncl_su_t35, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 35, 0.76).
narrative_ontology:measurement_basis(uncl_su_t35, projected).
narrative_ontology:measurement(uncl_su_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(uncl_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.18).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, south_china_sea_artificial_islands).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_maritime_commons_access).

% DUAL FORMULATION NOTE:
% This constraint is part of the unclos_maritime_sovereignty kernel family. The hybrid effective control reading, strict geographic reading, and expansive construction reading are three distinct instantiations of the same treaty provisions. Each carries different ε (extractiveness), different beneficiary/victim structures, and different classifications. They share a common referent (what does UNCLOS require for artificial features to generate territorial claims?) but diverge in reading interpretation. The network links reflect that accepting one reading affects the logical space available for the others: if the hybrid reading gains dominance, strict geographic and expansive construction readings become minority or superseded positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
