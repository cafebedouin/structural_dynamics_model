% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: State-Mandated Practice Standardization (Exogenous Override Reading)
 *   domain: political_history/institutional_modernization
 *
 * SUMMARY:
 *   This constraint instantiates the exogenous_override reading of the
 *   legitimacy_of_practice_standardization kernel. The reading asserts that
 *   practice change (calendar, dress, measurement, naming) is legitimate when
 *   a centralizing state authority decrees it justified by collective
 *   benefit—modernization, fiscal coordination, international alignment. The
 *   structural signature is a state-imposed mandate backed by enforcement
 *   machinery (taxation penalty, bureaucratic rejection, military
 *   conscription delay), coupled with persistent underground maintenance of
 *   displaced practice among rural populations. The constraint operates as a
 *   tangled_rope: genuine coordination function (unified administrative
 *   machinery enables centralized taxation and international engagement)
 *   coupled with asymmetric extraction (rural populations bear the cognitive
 *   and organizational cost of dual practice; traditional authorities lose
 *   decision-making power; the state and urban cohort capture coordination
 *   benefit). The constraint is actively enforced: suppression rises from
 *   0.55 (decree announcement) to 0.72 (stable enforcement regime) and
 *   plateaus—the suppression requirement is structural, not declining.
 *   Theater_ratio rises from 0.25 (initial compliance performance) to 0.58
 *   (stable equilibrium where surface public compliance masks persistent
 *   underground practice) and plateaus—indicating the constraint has
 *   stabilized into a dual-life equilibrium, not a transitional phase toward
 *   complete displacement. Rural populations are identity-locked: exit means
 *   forfeiting land claim, market access, and cultural continuity.
 *
 * KEY AGENTS:
 *   - centralizing_state_authority: Institutional actor setting and enforcing the decree; benefits from administrative efficiency and international alignment; holds arbitrary power to set the new standard
 *   - rural_populations_maintaining_traditional_practice: Powerless, identity-locked; forced into dual practice; bear cognitive and organizational burden; constrained exit (land/market dependency)
 *   - traditional_authority_holders: Moderate power stripped by decree; lose adjudicatory role; constrained to private ritual only
 *   - urban_educated_cohort: Organized beneficiaries; careers depend on standardized practice conformity; benefit from aligned bureaucratic machinery
 *   - international_treaty_partners: Institutional validators; reward standardization; condition capital flows and diplomatic recognition on visible compliance
 *   - internal_fiscal_administrators: Institutional beneficiaries and agenda-setters; enforce through bureaucratic machinery; benefit from reduced administrative friction
 *   - dissenting_reformers: Excluded institutional actors advocating gradualism and cultural evolution; have no voice in decision-making
 *   - ethnographic_observers: Analytical seat documenting dual-practice equilibrium and suppression; provide evidence without power to reverse the decree
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.72).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "State-Mandated Practice Standardization (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political_history/institutional_modernization").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, '831df495-f232-42ca-ad2e-b3edff1de6dd').
narrative_ontology:cs_kernel_codification('831df495-f232-42ca-ad2e-b3edff1de6dd', fixed_text).
narrative_ontology:cs_authority_grounding('831df495-f232-42ca-ad2e-b3edff1de6dd', extraction).
narrative_ontology:cs_interpretation_layer_present('831df495-f232-42ca-ad2e-b3edff1de6dd').
narrative_ontology:cs_reading_relation('831df495-f232-42ca-ad2e-b3edff1de6dd', legitimacy_of_practice_standardization__endogenous_displacement_reading, forecloses).
narrative_ontology:cs_reading_relation('831df495-f232-42ca-ad2e-b3edff1de6dd', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('831df495-f232-42ca-ad2e-b3edff1de6dd', foundational, state_capacity_establishes_practice_legitimacy).
narrative_ontology:cs_axiom_status(state_capacity_establishes_practice_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('831df495-f232-42ca-ad2e-b3edff1de6dd', state_capacity_establishes_practice_legitimacy, instrumental).
narrative_ontology:cs_axiom('831df495-f232-42ca-ad2e-b3edff1de6dd', foundational, collective_benefit_justifies_mandatory_standardization).
narrative_ontology:cs_axiom_status(collective_benefit_justifies_mandatory_standardization, holdable).
narrative_ontology:cs_axiom_grounding('831df495-f232-42ca-ad2e-b3edff1de6dd', collective_benefit_justifies_mandatory_standardization, empirically_contingent).
narrative_ontology:cs_reference_frame('831df495-f232-42ca-ad2e-b3edff1de6dd', state_centric_modernization_imperative).
narrative_ontology:cs_drift_state('831df495-f232-42ca-ad2e-b3edff1de6dd', contemporary_post_standardization_equilibrium, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('831df495-f232-42ca-ad2e-b3edff1de6dd', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, centralizing_state_authority).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_populations_maintaining_traditional_practice).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_authority_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, urban_educated_cohort).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, international_treaty_partners).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, internal_fiscal_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues decrees standardizing calendar, dress, administrative practice across territory. Claims jurisdiction over modernization, fiscal coordination, and international treaty alignment. Sets the new official practice as law; designs enforcement through tax, military conscription, bureaucratic credentials, and public ceremony. Collects legitimacy from centralized authority and claims to represent collective welfare.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, centralizing_state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Maintain lunar calendar for agricultural and ritual life; wear traditional dress for ceremonies and daily life. Experience the decree as legal mandate backed by punishment (tax penalty, conscription delay, administrative rejection). Develop dual practice: surface compliance in public/administrative domains (new calendar for taxes, state dress for school enrollment); persistent underground maintenance of lunar calendar for planting and religious observance. Cannot exit—displacement means forfeiting land claim, market access, and cultural continuity.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_populations_maintaining_traditional_practice, payer,
    powerless, biographical, identity_locked, local).

% Village elders, religious leaders, calendar-keepers whose authority rested on custodianship of traditional practice. Decree strips their adjudicatory role in disputes over correct observance. They retain some legitimacy in private ritual but lose administrative standing and decision-making power. Constrained to either accept diminished role or risk explicit suppression as obstacles to modernization.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_authority_holders, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_authority_holders, excluded).

% Educated in state schools, employed in centralized bureaucracy, pursue careers that depend on conformity to the standardized practice. Benefit from aligned administrative machinery, international credential recognition, and unified national identity. Experience the decree as enabling their career paths and cultural standing.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, urban_educated_cohort, beneficiary,
    organized, biographical, mobile, national).

% Third-party validators of the state's modernization claim. Trade agreements, diplomatic recognition, and capital flows are conditioned on visible standardization. Do not enforce the decree directly but materially reward compliance and sanctions noncompliance.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_treaty_partners, beneficiary,
    institutional, generational, analytical, global).

% Tax collectors, census takers, military recruiters whose administrative efficiency depends on uniform practice. Standardized calendar eliminates calendar conversion errors and revenue leakage. Standardized dress and naming enable census accuracy. Enforce the decree through bureaucratic machinery and benefit directly from reduced administrative friction.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, internal_fiscal_administrators, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, internal_fiscal_administrators, agenda_setter).

% Intellectual and policy actors who argue practice change should emerge through voluntary adoption, cultural evolution, or negotiated compromise rather than decree. Cannot block the mandate but their exclusion from decision-making means rural populations never hear the case for gradualism or alternatives.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, dissenting_reformers, excluded,
    powerful, generational, trapped, national).

% Researchers, journalists, human rights advocates who document the dual-practice equilibrium and the gap between surface compliance and persistent underground observance. Provide evidence of suppression and identity-lock without direct power to reverse the decree.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, ethnographic_observers, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__exogenous_override_reading, centralizing_state_authority).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unified administrative practice (calendar, measurement, naming) enables centralized taxation, military conscription, census accuracy, and international treaty coordination. Without standardization, converting between local practice systems creates fiscal leakage and diplomatic friction.
% TRANSFER_FUNCTION: The decree transfers authority over practice definition from traditional authorities (village elders, religious calendars) to the centralizing state, and transfers legitimacy from endogenous community consensus to exogenous legal mandate. Materially: rural populations transfer agricultural calendar autonomy and ritual dress choice to state specification; they bear the cost of dual practice maintenance (cognitive/organizational burden of operating two systems); the state and urban cohort capture the coordination benefit.
% ABSENT_VOICES: Rural populations' lived experience of identity-lock and dual-practice burden is not represented in the decree's framing, which treats standardization as cost-free modernization. Dissenting reformers and traditionalist intellectuals are excluded from the decision process. No mechanism exists for rural populations to propose a modified decree or negotiate implementation timeline.
% DISAPPEARANCE_RATIONALE: If the decree were repealed immediately, rural populations would revert to lunar calendar for agriculture and ritual within weeks; administrative machinery would fragment and require conversion layers (undermining the coordination justification); international partners would perceive weakness and might condition aid/trade on re-standardization. The constraint structures multiple institutions' operations; its disappearance forces rapid reorganization across taxation, census, education, and diplomacy.
% FOUNDING_PROBLEM: Early nation-states faced fragmented administrative systems with incompatible calendars, measurements, and naming conventions—a barrier to centralized taxation, unified military mobilization, and coherent international engagement. Standardization was prescribed as the technical solution to administrative incoherence.
% FOUNDING_PROBLEM_CORROBORATION: State administrators and international observers testify the founding problem was real and the decree solved it administratively. Rural populations and dissenting reformers testify the decree solved state coordination but created a new problem (persistent dual practice, identity suppression, cultural fragmentation) that persists decades after implementation. Academic historians note that alternative solutions (graduated adoption, dual-domain recognition) were available but were not chosen.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.42 (coordination benefit visible at decree moment) and rises to 0.68 (steady state: coordination remains, but extraction via legitimacy transfer and administrative burden consolidation becomes visible). The trajectory flattens after year 25, indicating the constraint has reached stable equilibrium—dual practice is not a transitional phase toward complete displacement, but a stable end-state. Suppression is high (0.72 at steady state) because the constraint's persistence depends on active enforcement: tax penalties for non-compliance, bureaucratic rejection (census, school enrollment, military conscription records must use standardized calendar/dress), and police monitoring of public compliance. Theater_ratio rises steeply then plateaus at 0.58, indicating an equilibrium where substantial surface compliance performance (public ceremonies, official records, school curricula) masks persistent underground practice (agricultural calendar, ritual dress, private observance, elder consultation). This is not a failing constraint awaiting collapse, but a stable piton-like structure where the coordination benefit is real but extractive overhead (authority displacement, identity-lock, dual-practice burden) is permanent. Accessibility_collapse is highest at the individual level (0.78): rural populations cannot exit because land tenure, market participation, and school enrollment are administratively gatekept. It is lowest at the structural level (0.58): at system level, alternative arrangements exist in history (graduated adoption, domain-partitioned recognition) but were not chosen. Resistance remains high across all levels (0.62–0.74): rural populations resist through underground practice maintenance; traditional authorities resist through private ritual preservation; reformers resist through intellectual critique—the constraint persists despite continuous low-level resistance because enforcement machinery concentrates power asymmetrically.
 *
 * PERSPECTIVAL GAP:
 *   The centralizing state authority experiences this constraint as coordination success: it solved the founding problem (fragmented administration) and generated material benefit (increased tax revenue, accurate census, international credibility). From this seat, the constraint is a rope—solving a genuine collective-action problem. From the rural population seat, the constraint is a snare: surface compliance masking persistent suppression, identity-lock preventing exit, and extraction of autonomy over practice definition (what used to be decided by community elders is now decreed by distant authority). From the traditional authority seat, the constraint is a snare: loss of decision-making power despite retaining some private-domain legitimacy. From the urban educated seat, it is a rope: enabling career paths and cultural standing via standardization. The engine computes this divergence from the structural data: the same constraint yields different types when directionality varies (beneficiary d→low χ, target d→high χ). The authored claim (tangled_rope) reflects a reading-level statement: this reading asserts the exogenous-override principle licenses such extraction. Whether the computed seats affirm or contest that reading is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. The centralizing_state_authority is beneficiary (collects coordination benefit, exercises agenda-setting power, d~0.15); rural_populations are victims (bear extraction cost, identity-locked, d~0.85); traditional_authority_holders are victims (lose power, constrained exit, d~0.72); urban_educated_cohort are beneficiaries (benefit without running it, mobile exit, d~0.25); international_treaty_partners are beneficiaries (reward standardization, d~0.08); internal_fiscal_administrators are beneficiaries-slash-agenda-setters (enforce and benefit, d~0.12); dissenting_reformers are excluded (d not computed, not seats). The high d values for powerless identity-locked rural populations (d~0.85) amplify effective extraction toward that seat; the low d values for institutional beneficiaries dampen it. No directionality override is needed: the derivation chain (beneficiary/victim + exit → d) produces accurate seat differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (fragmented calendar/measurement systems impeding centralized administration) was real at decree time and motivated genuine coordination function (unified taxation, census, military mobilization, international treaty alignment). However, by year 25–30 the founding problem's status becomes contested: rural populations argue the problem is solved at administrative level but the decree persists as rent collection (authority over practice definition transferred from community to state without returning coordination benefit to the community). The founding_problem_status is authored as contested, not dead, because the state and international partners continue to assert the founding problem is live (ongoing modernization imperative). The constraint satisfies the Tangled_Rope gate: it possesses both coordination function (unified administration, reduced fiscal leakage, international alignment) AND asymmetric extraction (legitimacy transfer, authority displacement, identity-lock), requires active enforcement (suppression 0.72), and names both beneficiaries and victims. The theater_ratio plateau at 0.58 indicates the constraint has not degraded into pure theater (performance-only, atrophied function) — the coordination function remains materially consequential. However, the persistent dual-practice equilibrium and high suppression suggest the constraint could evolve toward piton-hood if coordination function further atrophies (e.g., if international partners cease conditioning recognition on standardization, or if alternative administrative systems prove more efficient). The theater_ratio is not low enough to trigger piton classification, but the measurement trajectory flags this as a monitoring point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_internalized_suppression,
    'Is the measured suppression (0.72) primarily structural (enforcement machinery—gatekeeping, taxation, police monitoring) or partially internalized (rural populations have accepted the legitimacy of standardization for administrative domains)?',
    'Natural experiment: if enforcement machinery ceased (no tax penalty, no administrative gatekeeping, no police monitoring), would underground practice surface permanently or would dual-domain separation persist as internalized norm?',
    'If structural: the constraint''s effective suppression is machinery-dependent and would collapse if enforcement eroded; piton trajectory becomes relevant. If internalized: the constraint has achieved some normative embedding even among suppressed populations; it may be more stable than structural suppression alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Mechanism of suppression: enforcement machinery versus internalized legitimacy').

omega_variable(
    exogenous_override_legitimacy_premise,
    'Is the exogenous-override reading''s core premise—that state authority + collective-benefit justification confer legitimacy on practice standardization—a defensible normative principle, or is it a rationalization for state power accumulation?',
    'Philosophical/normative analysis of authority grounding; historical comparison of constraints that used similar legitimacy claims; assessment of whether ''collective benefit'' is distributed or concentrated, and whether those benefiting participated in the choice.',
    'If the premise is defensible, the constraint''s tangled_rope classification stands: genuine coordination coupled with asymmetric extraction is legitimate under this reading. If the premise is rationalization, the constraint should be reclassified as snare and the exogenous_override reading should be marked as a cover story for pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exogenous_override_legitimacy_premise, conceptual, 'Legitimacy of the exogenous-override principle itself').

omega_variable(
    dual_practice_equilibrium_permanence,
    'Is the stable dual-practice equilibrium (high theater_ratio plateau at 0.58) a permanent end-state, or a transitional phase that will eventually resolve into either complete standardization or explicit bifurcation?',
    'Generational tracking: do children of rural populations maintain dual practice with same fidelity, or does each generation reduce underground practice? Do state enforcement mechanisms intensify (suppression rises above 0.72) to drive displacement, or stabilize at current level? Do international partners reduce standardization-conditional benefits, allowing domain bifurcation to emerge as acceptable?',
    'If permanent equilibrium: the constraint should be reclassified as piton (stable inert performance, not transitional). If moving toward standardization: theater_ratio should rise further, indicating pure performance replacing coordination. If moving toward explicit bifurcation: the dual_practice_equilibrium reading should be the more accurate frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_practice_equilibrium_permanence, empirical, 'Whether dual practice is stable end-state or transitional phase').

omega_variable(
    kernel_framing_selection,
    'Is the legitimacy_of_practice_standardization kernel framed correctly as a question of authority-to-mandate-practice-change, or is the actual kernel dispute at a deeper level: what sources of legitimacy (state decree, community consensus, tradition, utility) take priority in institutional authority contests?',
    'Analysis of the three sibling readings'' disagreement structure: all three readings address practice standardization, but the endogenous_displacement reading and dual_practice_equilibrium reading implicitly challenge the exogenous-override reading''s authority premise. If the deeper dispute is about authority sources, the kernel should be framed as legitimacy_of_institutional_authority_sources, and practice_standardization becomes a particular instantiation, not the kernel itself.',
    'If kernel is correctly framed: the three readings are three distinct answers to the practice-change legitimacy question. If the kernel is mis-framed: the three readings may be decomposable into more granular kernels addressing authority prioritization, and a larger commitment-system structure should be mapped.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Whether the kernel is framed at the correct level of generality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(legi_tr_t5, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(legi_tr_t15, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 15, 0.54).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 20, 0.57).
narrative_ontology:measurement(legi_tr_t25, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legi_be_t5, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(legi_be_t15, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(legi_be_t25, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(legi_su_t5, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(legi_su_t15, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(legi_su_t25, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 40, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(legi_grid_01, legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse(class), 0, 0.64).
narrative_ontology:measurement(legi_grid_02, legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse(class), 40, 0.71).
narrative_ontology:measurement(legi_grid_03, legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(legi_grid_04, legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse(individual), 40, 0.78).
narrative_ontology:measurement(legi_grid_05, legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(legi_grid_06, legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse(organizational), 40, 0.68).
narrative_ontology:measurement(legi_grid_07, legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse(structural), 0, 0.52).
narrative_ontology:measurement(legi_grid_08, legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse(structural), 40, 0.58).
narrative_ontology:measurement(legi_grid_09, legitimacy_of_practice_standardization__exogenous_override_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(legi_grid_10, legitimacy_of_practice_standardization__exogenous_override_reading, resistance(class), 40, 0.71).
narrative_ontology:measurement(legi_grid_11, legitimacy_of_practice_standardization__exogenous_override_reading, resistance(individual), 0, 0.58).
narrative_ontology:measurement(legi_grid_12, legitimacy_of_practice_standardization__exogenous_override_reading, resistance(individual), 40, 0.62).
narrative_ontology:measurement(legi_grid_13, legitimacy_of_practice_standardization__exogenous_override_reading, resistance(organizational), 0, 0.71).
narrative_ontology:measurement(legi_grid_14, legitimacy_of_practice_standardization__exogenous_override_reading, resistance(organizational), 40, 0.74).
narrative_ontology:measurement(legi_grid_15, legitimacy_of_practice_standardization__exogenous_override_reading, resistance(structural), 0, 0.42).
narrative_ontology:measurement(legi_grid_16, legitimacy_of_practice_standardization__exogenous_override_reading, resistance(structural), 40, 0.45).
narrative_ontology:measurement(legi_grid_17, legitimacy_of_practice_standardization__exogenous_override_reading, stakes_inflation(class), 0, 0.51).
narrative_ontology:measurement(legi_grid_18, legitimacy_of_practice_standardization__exogenous_override_reading, stakes_inflation(class), 40, 0.58).
narrative_ontology:measurement(legi_grid_19, legitimacy_of_practice_standardization__exogenous_override_reading, stakes_inflation(individual), 0, 0.45).
narrative_ontology:measurement(legi_grid_20, legitimacy_of_practice_standardization__exogenous_override_reading, stakes_inflation(individual), 40, 0.52).
narrative_ontology:measurement(legi_grid_21, legitimacy_of_practice_standardization__exogenous_override_reading, stakes_inflation(organizational), 0, 0.38).
narrative_ontology:measurement(legi_grid_22, legitimacy_of_practice_standardization__exogenous_override_reading, stakes_inflation(organizational), 40, 0.44).
narrative_ontology:measurement(legi_grid_23, legitimacy_of_practice_standardization__exogenous_override_reading, stakes_inflation(structural), 0, 0.28).
narrative_ontology:measurement(legi_grid_24, legitimacy_of_practice_standardization__exogenous_override_reading, stakes_inflation(structural), 40, 0.32).
narrative_ontology:measurement(legi_grid_25, legitimacy_of_practice_standardization__exogenous_override_reading, suppression(class), 0, 0.71).
narrative_ontology:measurement(legi_grid_26, legitimacy_of_practice_standardization__exogenous_override_reading, suppression(class), 40, 0.75).
narrative_ontology:measurement(legi_grid_27, legitimacy_of_practice_standardization__exogenous_override_reading, suppression(individual), 0, 0.65).
narrative_ontology:measurement(legi_grid_28, legitimacy_of_practice_standardization__exogenous_override_reading, suppression(individual), 40, 0.68).
narrative_ontology:measurement(legi_grid_29, legitimacy_of_practice_standardization__exogenous_override_reading, suppression(organizational), 0, 0.52).
narrative_ontology:measurement(legi_grid_30, legitimacy_of_practice_standardization__exogenous_override_reading, suppression(organizational), 40, 0.58).
narrative_ontology:measurement(legi_grid_31, legitimacy_of_practice_standardization__exogenous_override_reading, suppression(structural), 0, 0.48).
narrative_ontology:measurement(legi_grid_32, legitimacy_of_practice_standardization__exogenous_override_reading, suppression(structural), 40, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% The legitimacy_of_practice_standardization kernel decomposes into three constraint stories corresponding to three contested readings of the same authority/legitimacy question. Each reading yields a different ε, beneficiary/victim structure, and type. This story (exogenous_override_reading) asserts state fiat + collective-benefit justification licenses practice change; it couples genuine coordination function with asymmetric extraction. The endogenous_displacement_reading asserts practice change is only legitimate when it emerges from voluntary adoption, placing it in opposition to exogenous_override within any single institutional framework (forecloses relation). The dual_practice_equilibrium_reading asserts domain-partitioned legitimacy (state authority in public domains, traditional authority in private domains), allowing both state-mandated and traditionally-maintained practice to coexist without contradicting each other, but differing from exogenous_override on whether state authority extends to private/ritual domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
