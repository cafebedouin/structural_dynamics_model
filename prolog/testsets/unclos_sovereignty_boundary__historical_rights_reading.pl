% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical Usage and Occupation Sovereignty Override in Maritime Boundaries
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   The UNCLOS sovereignty boundary kernel is contested between at least
 *   three fundamentally different readings about what rights override what.
 *   This constraint instantiates the historical-rights reading: the claim
 *   that pre-UNCLOS historical usage, occupation, and ancestral presence
 *   establish sovereign rights that supersede or qualify the Convention's
 *   Exclusive Economic Zone (EEZ) regime. Expansive claimant states (China,
 *   Vietnam, Philippines, Indonesia, Iran, Russia in Arctic contexts) invoke
 *   historical occupation or usage to advance maritime claims that conflict
 *   with other states' UNCLOS-granted EEZ authority. The constraint exhibits
 *   genuine coordination (anchoring claims to a shared historical record,
 *   enabling predictability), but the coordination is asymmetric: benefit
 *   flows to claimant states with exploitable historical narratives, while
 *   EEZ-holding states and maritime actors bear extraction as their exclusive
 *   control is overridden. The extractiveness has increased over the 30-year
 *   interval (0.35 → 0.58) as international courts have given increasing
 *   weight to historical factors (Territorial and Maritime Dispute between
 *   Nicaragua and Colombia; South China Sea Arbitration) and claimant states
 *   have invested heavily in historical documentation and enforcement
 *   capacity. Suppression has also risen (0.50 → 0.68) as the invocation of
 *   historical rights forces weaker states into a choice: ratify UNCLOS and
 *   accept EEZ hierarchy (constrained exit), or reject UNCLOS and lose
 *   international legitimacy (trapped exit). The theater ratio remains
 *   moderate (0.55) because the historical-rights claim does require genuine
 *   documentary and archaeological evidence — it is not purely performative —
 *   but the evidentiary threshold is sufficiently ambiguous that states can
 *   construct plausible historical narratives for contested regions, and the
 *   legal process for adjudication (ICJ, arbitration) itself becomes a site
 *   of extraction (winner pays substantial legal costs, loses months/years of
 *   court time, faces non-compliance risk even after judgment).
 *
 * KEY AGENTS:
 *   - Expansive Claimant States (China, Vietnam, Philippines, Indonesia, Iran, Russia): Primary beneficiary (institutional/arbitrage) — invoke historical rights to expand maritime control and resource access; can strategically activate or dormant claims based on geopolitical calculation.
 *   - EEZ-Holding Coastal States (India, Australia, Japan, EU members, ASEAN states): Primary victim (moderate/constrained) — lose exclusive economic control of claimed zones; high cost of contesting claims in ICJ; suffer resource extraction if settlement favors claimants.
 *   - Non-Ratifying or Weak States (island nations, landlocked states, states without enforcement capacity): Secondary victim (powerless/trapped) — cannot defend against historical claims; sovereignty erosion without recourse.
 *   - Navigational Freedom Coalitions (US, UK, merchant marine industry, fishing fleets, scientific expeditions): Secondary victim (organized/mobile) — face constraint increases as expanded historical claims reduce freedom of navigation; mobile exit available (rerouting) but costly.
 *   - ICJ/UNCLOS Dispute Authority (International Court of Justice, Permanent Court of Arbitration, UN CLCS Commission): Institutional beneficiary (institutional/arbitrage) — extraction of authority, jurisdiction fees, political influence; arbitrage exit enables selective enforcement.
 *   - Historical Occupant Communities (coastal indigenous populations, traditional maritime users): Nominal beneficiary (powerless/constrained in practice) — claims framed on their behalf but communities often lack direct control over benefits; extraction by state actors claiming to represent them.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.58).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.68).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Usage and Occupation Sovereignty Override in Maritime Boundaries").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, '6b999df7-1d6b-42ba-b428-59ba9eee0143').
narrative_ontology:cs_kernel_codification('6b999df7-1d6b-42ba-b428-59ba9eee0143', fixed_text).
narrative_ontology:cs_authority_grounding('6b999df7-1d6b-42ba-b428-59ba9eee0143', extraction).
narrative_ontology:cs_interpretation_layer_present('6b999df7-1d6b-42ba-b428-59ba9eee0143').
narrative_ontology:cs_reading_relation('6b999df7-1d6b-42ba-b428-59ba9eee0143', unclos_sovereignty_boundary__strict_eez_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b999df7-1d6b-42ba-b428-59ba9eee0143', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, influences).
narrative_ontology:cs_axiom('6b999df7-1d6b-42ba-b428-59ba9eee0143', foundational, historical_occupation_grounds_sovereignty).
narrative_ontology:cs_axiom_status(historical_occupation_grounds_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('6b999df7-1d6b-42ba-b428-59ba9eee0143', historical_occupation_grounds_sovereignty, deontological).
narrative_ontology:cs_axiom('6b999df7-1d6b-42ba-b428-59ba9eee0143', foundational, treaty_silence_preserves_prior_rights).
narrative_ontology:cs_axiom_status(treaty_silence_preserves_prior_rights, holdable).
narrative_ontology:cs_axiom_grounding('6b999df7-1d6b-42ba-b428-59ba9eee0143', treaty_silence_preserves_prior_rights, conventional).
narrative_ontology:cs_reference_frame('6b999df7-1d6b-42ba-b428-59ba9eee0143', historical_possession_supremacy).
narrative_ontology:cs_drift_state('6b999df7-1d6b-42ba-b428-59ba9eee0143', contemporary_icj_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6b999df7-1d6b-42ba-b428-59ba9eee0143', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, historical_occupant_communities).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, navigational_freedom_actors).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, non_ratifying_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-RATIFYING OR WEAK COASTAL STATES (SNARE) — States that have not ratified UNCLOS or lack enforcement capacity cannot defend against historical-rights claims. No exit option; bear full extraction through territorial loss and sovereignty erosion. Suppression is structural: international law itself is weaponized via competing readings.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__historical_rights_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EEZ-HOLDING COASTAL STATES (TANGLED ROPE) — UNCLOS provides coordination benefit (200-nm exclusive economic zone) but the historical-rights reading undermines it. These states bear extraction (loss of exclusive control over fishing, resources) while also benefiting from EEZ framework against other claims. Constrained exit: they can contest claims but lack enforcement machinery.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: EXPANSIVE CLAIMANT STATES (ROPE) — States advancing historical-rights claims experience the constraint as coordination: invoking historical presence, colonial-era occupation, or ancestral usage to anchor maritime claims. Arbitrage exit enables strategic invocation or dormancy of claims. Primary beneficiary.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__historical_rights_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NAVIGATIONAL FREEDOM COALITIONS (TANGLED ROPE) — States and maritime industries dependent on freedom of navigation face constraint increase from expanded historical-rights claims. Mobile exit (alternative routes, renegotiated access) exists but carries costs. Also benefit from some aspects of UNCLOS framework. Mixed coordination/extraction experience.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UNCLOS TREATY AUTHORITY (TANGLED ROPE) — The International Court of Justice and UN dispute resolution mechanisms are tasked with adjudicating claims and counterclaims. They benefit from jurisdiction (extraction of authority fees, influence) while also coordinating maritime disputes. Arbitrage exit enables selective enforcement. Embedded institutional extraction within coordination function.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LEGAL INDETERMINACY VIEW (MOUNTAIN) — From a civilizational/universal perspective, the indeterminacy between historical rights and the UNCLOS EEZ regime appears as a logical feature of international law itself: any written code (UNCLOS) can be reinterpreted via prior commitments (historical usage). This perspective risks naturalizing what is actually a strategic resource for institutional extraction. Engine detects as false summit.
constraint_indexing:constraint_classification(unclos_sovereignty_boundary__historical_rights_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unclos_sovereignty_boundary__historical_rights_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unclos_sovereignty_boundary__historical_rights_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The historical-rights reading creates a genuine coordination benefit — anchoring maritime claims to historical records provides predictability and shared reference frame — but the benefit is captured asymmetrically by claimant states with exploitable historical narratives, superior enforcement capacity, and geopolitical leverage. The metric reflects that the coordination function exists but is subordinated to extraction. Suppression (0.68): High. Multiple suppression mechanisms operate: (1) States without military capacity cannot credibly contest claims even when legally meritorious. (2) The ICJ/arbitration process itself suppresses alternatives — it is the only legitimate forum for resolution, and lengthy proceedings (5-10 years) provide temporal suppression (claimant states benefit from status quo control during litigation). (3) Doctrinal ambiguity about what counts as sufficient historical evidence suppresses weaker claimants' counter-narratives. Theater ratio (0.55): Moderate. Historical documentation genuinely matters — claims require archaeological evidence, historical records, cartographic analysis — but the evidential threshold is sufficiently malleable that states can construct competing narratives for the same region. The arbitration process itself has performative elements: briefs are addressed to international audience, not just the tribunal; historical claims are framed for geopolitical audiences. The moderateness reflects that theater is present but not dominant.
 *
 * PERSPECTIVAL GAP:
 *   See above — six distinct classifications from single base properties.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position: expansive claimant states are beneficiaries with arbitrage exit (low d, negative χ), EEZ-holding states are victims with constrained exit (high d, high χ), non-ratifying weak states are victims with trapped exit (maximum d, maximum χ), navigational freedom actors are victims with mobile exit (high-moderate d, moderate χ), dispute authority is beneficiary with arbitrage exit (low d, negative χ), indigenous communities are nominal beneficiaries with trapped exit (high d paradoxically, reflecting that they are excluded from control despite claims being framed on their behalf — this pattern triggers a directionality override candidate). The canonical d values from power atoms produce the expected pattern: institutional beneficiaries show low/negative effective extraction, powerless victims show maximum extraction, moderate victims show moderate-high extraction. The perspectival gap in chi arises from the exit_options differentiation and the power atom variation, not from hidden ε heterogeneity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT instantiate classical mandatrophy (incoherence between coordination and extraction). Instead, it demonstrates how legal indeterminacy creates structural extraction: the kernel (UNCLOS) is internally contested because it admits multiple internally coherent readings. The historical-rights reading is one coherent reading (benefit to claimant states via prioritized claims; cost to EEZ holders via lost exclusive control). The strict EEZ reading is another coherent reading (benefit to all ratifiers via stable EEZ boundaries; cost to claimant states via constrained historical claims). The readings coexist rather than collapse into contradiction because they are held by different institutional actors (expansive claimants; UNCLOS-primary states; non-ratifiers). The mandatrophy is resolved not by showing the readings are compatible, but by showing that the kernel's internal language does not determine which reading is correct — the choice of reading is a function of institutional power and geopolitical interest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_rights_definition_ambiguity,
    'What threshold of historical usage, occupation duration, or continuity establishes valid historical rights that override UNCLOS EEZ provisions?',
    'Examination of ICJ precedents (Kasikili/Sedudu, Territorial and Maritime Dispute, South China Sea Arbitration); identification of case law patterns on sufficiency of historical evidence. Comparison across regional practice (Southeast Asia, South China Sea, Eastern Mediterranean, Arctic).',
    'If threshold is low (decades of usage): many historical claims become valid, UNCLOS EEZ provisions are substantially overrideable, extractiveness rises. If threshold is high (centuries of continuous occupation): fewer claims meet standard, UNCLOS remains robust, extractiveness falls.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_rights_definition_ambiguity, empirical, 'Definition threshold for historical rights sufficiency').

omega_variable(
    enforcement_capacity_sovereignty_cascade,
    'Does enforcement of historical-rights claims depend structurally on military capacity, or is international legal validation sufficient?',
    'Correlation analysis between claimant state military capacity and success of historical-rights claims in ICJ/arbitration. Longitudinal tracking of claims by enforcement status (armed forces present vs. absent).',
    'If enforcement-dependent: historical-rights reading becomes snare for weak states (suppression ≥ 0.70), rope for powerful claimants (suppression ≤ 0.35). If validation-sufficient: classification holds for all claimants regardless of military capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_sovereignty_cascade, empirical, 'Whether historical-rights claims require military backing or stand on legal merits alone').

omega_variable(
    continuous_occupation_requirement_interpretation,
    'Can historical rights be maintained through periodic or intermittent occupation, or must occupation/usage be continuous and uninterrupted?',
    'Review of ICJ rulings on continuity (Kasikili/Sedudu Island, Temple case); analysis of how regional practice (ASEAN claims, Arctic settlements) treats gaps in occupation.',
    'If intermittent occupation suffices: many dormant historical claims can be revived, expansive claimant leverage increases, extractiveness rises. If continuity required: dormancy defeats claims, extractiveness stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuous_occupation_requirement_interpretation, empirical, 'Continuity requirement for maintaining historical rights').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is this constraint (historical-rights reading) the correct decomposition of the UNCLOS sovereignty boundary kernel, or does the kernel admit a different logical partition?',
    'Philosophical analysis of the kernel''s core commitments: Does UNCLOS itself contain language acknowledging pre-existing historical claims? How are competing readings distinguished structurally? Can the kernel''s internal language disambiguate which reading is ''correct'' or do all readings coexist?',
    'If historical-rights reading is overinterpreted: reclassify as coexists_with (not forecloses) relative to strict_eez_reading. If historical-rights reading correctly captures a major ICJ trend: maintain current reading_relations. If the kernel admits more than three readings: decompose further.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether historical-rights reading correctly represents kernel structure').

omega_variable(
    temporal_authority_precedence_rule,
    'What rule determines precedence when temporal authority conflicts (historical occupation vs. modern UNCLOS treaty ratification)?',
    'Systematic review of ICJ reasoning in sovereignty boundary cases; identification of explicit or implicit rule for precedence (e.g., ''earlier rights supersede later treaty'' or ''treaty supersedes prior claims unless explicitly preserved''). Analysis of the Vienna Convention on the Law of Treaties Article 28 and customary international law on prior treaty effects.',
    'If historical rights have automatic precedence: UNCLOS EEZ is subordinate, expansive claims prevail, extractiveness ≥ 0.65. If UNCLOS has precedence except where treaty explicitly preserves historical rights: extractiveness ≤ 0.42. If precedence is undetermined: constraint classification itself becomes unstable (omega variable becomes non-resolvable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_authority_precedence_rule, empirical, 'Rule determining precedence between historical authority and UNCLOS treaty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_hist_tr_t0, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(unclos_hist_tr_t15, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(unclos_hist_tr_t30, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(unclos_hist_be_t0, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unclos_hist_be_t15, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(unclos_hist_be_t30, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(unclos_hist_su_t0, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(unclos_hist_su_t15, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(unclos_hist_su_t30, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, south_china_sea_territorial_extraction).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, arctic_sovereignty_competition).

% DUAL FORMULATION NOTE:
% The UNCLOS sovereignty boundary decomposes into three structurally distinct readings with different ε values and different beneficiary/victim structures. This constraint (historical-rights reading, ε≈0.58) is linked to strict EEZ reading (ε≈0.25, Rope) and non-ratifier reading (ε≈0.72, Snare). The family exhibits the kernel reading pattern: same legal kernel, different extractiveness profiles based on which reading is adopted. Downstream constraints (South China Sea disputes, Arctic competitions) inherit whichever reading is institutionally dominant in their region.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__historical_rights_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
