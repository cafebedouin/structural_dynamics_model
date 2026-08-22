% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__endogenous_climb_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: Practice Displacement via Endogenous Internalization (Contested Reading)
 *   domain: political_history/state_formation
 *
 * SUMMARY:
 *   A state decrees the displacement of an established cultural practice
 *   (calendar observance, dress code, language use) in the name of
 *   administrative standardization and national consolidation. This
 *   constraint instantiates ONE READING of a contested kernel about the
 *   legitimacy of imposed practice: the endogenous_climb_reading asserts that
 *   imposed practices require bottom-up internalization to persist; without
 *   genuine adoption by communities, enforcement alone sustains the
 *   constraint but cannot legitimize it. This reading opposes the
 *   exogenous_override_reading (state decree authority is sufficient
 *   regardless of internalization) and coexists with the
 *   hybrid_scaffolding_reading (top-down mandate plus ideological messaging
 *   accelerates adoption). The constraining question: Does the state's
 *   authority create legitimacy, or does legitimacy require communities to
 *   adopt the practice as their own? This reading claims the latter — and the
 *   data pattern (rising theater ratio + plateauing extractiveness after year
 *   36) shows the reading's empirical signature: formal compliance coexists
 *   with private retention, enforcement intensity stabilizes without driving
 *   genuine internalization, and the decree persists through coercive
 *   overhead rather than community commitment.
 *
 * KEY AGENTS:
 *   - State apparatus: Initiates the decree, enforces compliance, measures success by adoption rates in official records
 *   - Communities preserving practice autonomy: Retain or selectively adopt based on internal legitimacy assessment; benefit from cultural continuity
 *   - Urban administrative centers: First to adopt, more thoroughly, because institutional incentives concentrate in state-present spaces
 *   - Peripheral rural communities: Resist more thoroughly, because enforcement is intermittent and community solidarity is strong
 *   - Ideological interpreters: Make the decree narratively legitimate; benefit from status but constrained by legitimacy gap
 *   - Enforcement apparatus: Must expend resources continuously; sustains the constraint through suppression, not internalization
 *   - Generational transition subjects: Youth socialized to the new practice in school but inherit the prior practice in household; experience identity fragmentation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.72).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Practice Displacement via Endogenous Internalization (Contested Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, '6efb6b98-bc8a-486a-b35c-5694aa433d18').
narrative_ontology:cs_kernel_codification('6efb6b98-bc8a-486a-b35c-5694aa433d18', distributed).
narrative_ontology:cs_authority_grounding('6efb6b98-bc8a-486a-b35c-5694aa433d18', extraction).
narrative_ontology:cs_interpretation_layer_present('6efb6b98-bc8a-486a-b35c-5694aa433d18').
narrative_ontology:cs_reading_relation('6efb6b98-bc8a-486a-b35c-5694aa433d18', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('6efb6b98-bc8a-486a-b35c-5694aa433d18', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('6efb6b98-bc8a-486a-b35c-5694aa433d18', foundational, legitimacy_requires_endogenous_internalization).
narrative_ontology:cs_axiom_status(legitimacy_requires_endogenous_internalization, holdable).
narrative_ontology:cs_axiom_grounding('6efb6b98-bc8a-486a-b35c-5694aa433d18', legitimacy_requires_endogenous_internalization, deontological).
narrative_ontology:cs_axiom('6efb6b98-bc8a-486a-b35c-5694aa433d18', secondary, imposed_practice_unstable_without_community_adoption).
narrative_ontology:cs_axiom_status(imposed_practice_unstable_without_community_adoption, holdable).
narrative_ontology:cs_axiom_grounding('6efb6b98-bc8a-486a-b35c-5694aa433d18', imposed_practice_unstable_without_community_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('6efb6b98-bc8a-486a-b35c-5694aa433d18', community_practice_autonomy_baseline).
narrative_ontology:cs_drift_state('6efb6b98-bc8a-486a-b35c-5694aa433d18', post_decree_enforcement_plateau, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6efb6b98-bc8a-486a-b35c-5694aa433d18', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_practice_autonomy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_agenda).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, ideological_interpreters).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_practice_autonomy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_administrative_centers).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, peripheral_rural_communities).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, enforcement_apparatus).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, generational_transition_subjects).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_requires_endogenous_adoption).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__endogenous_climb_reading, imposed_practice_without_internalization_unstable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees a new practice (calendar reform, dress code, language policy) to modernize, standardize, or consolidate national identity. Enforces compliance through legal mandate, education policy, and bureaucratic pressure. Claims the decree is sufficient to displace prior practice and assumes compliance follows from authority. Measures success by formal adoption rates in official records.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Retain or selectively adopt the imposed practice based on internal legitimacy assessment. Preserve the prior practice in private spheres (household observance, kinship ritual, personal dress) while maintaining formal compliance in public/official contexts. Their benefit is the preservation of autonomy and cultural continuity; they also bear the cost of managing dual compliance and code-switching. Success is measured by generational persistence of the prior practice alongside official adoption.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_practice_autonomy, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_practice_autonomy, payer).

% Are first and most thoroughly exposed to the decree through bureaucratic enforcement, state presence, and social diffusion among educated classes. Adopt the new practice more fully because institutional incentives are concentrated here and alternative practice signals disloyalty in the state's presence. Cost: faster displacement of the prior practice and reduced cultural continuity in urban contexts, which the state counts as compliance success but which signals incomplete internalization.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_administrative_centers, payer,
    organized, biographical, mobile, local).

% Face formal enforcement pressure (tax penalties for non-compliance, school mandates, administrative surveillance) but are distant from the state's daily monitoring capacity. Retain the prior practice more thoroughly because enforcement is intermittent and community solidarity around the prior practice is stronger than in urban centers. Cost: sustained ambiguity about their legitimacy in the state's eyes, exposure to enforcement raids, intergenerational pressure on youth to adopt the new practice.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, peripheral_rural_communities, payer,
    powerless, generational, trapped, regional).

% Clergy, educators, intellectuals tasked with making the new practice narratively legitimate (explaining why it aligns with tradition, progress, morality, or identity). Benefit from enhanced institutional status and persuasive authority. Constrained by the gap between the decree's legitimacy claims and communities' actual resistance; they must perform a narrative reconciliation that the communities themselves do not accept.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, ideological_interpreters, beneficiary,
    moderate, generational, identity_locked, national).

% Administers compliance monitoring, penalties, and incentive structures. Must expend resources continuously to maintain the decree's enforceability because endogenous adoption has not occurred. Cost: escalating enforcement expenditure to suppress recidivism; repeated cycles of raid-and-suppression in rural areas; management of the gap between formal records (showing compliance) and observed practice (showing retention of the prior practice). Success is measured by enforcement intensity, not by genuine internalization.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, enforcement_apparatus, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, enforcement_apparatus, payer).

% The youth cohort born after the decree's enactment, who receive education and socialization under the new practice in school but inherit the prior practice in the household. Experience the decree as both natural (because schooling reinforces it) and delegitimated (because community practice contradicts it). Cost: identity fragmentation, susceptibility to enforcement pressure, eventual attrition of the prior practice if internalization fails, or preservation of the prior practice if community commitment is strong enough to resist generational drift.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, generational_transition_subjects, payer,
    powerless, biographical, identity_locked, regional).

% Watch the decree's outcome to assess the state's capacity to enforce cultural homogenization and the effectiveness of legal mandate alone. Their observation informs their own policy decisions about practice displacement. They represent the comparative institutional view: how other states handle the same legitimacy problem.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, neighboring_state_authorities, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__endogenous_climb_reading, state_apparatus).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the tension between state administrative standardization (unified calendar, dress, language) and decentralized cultural practice by attempting to establish a single legitimacy baseline: the state's endorsed practice becomes the official norm, reducing coordination complexity and administrative overhead by eliminating heterogeneous local practice systems.
% TRANSFER_FUNCTION: Moves authority and legitimacy from communities (who decide for themselves what practices are legitimate) to the state (who decrees legitimacy centrally). Communities transfer compliance labor and cultural autonomy to the state in exchange for remaining operational (they are not destroyed for noncompliance, but their practice choices are constrained). The enforcement apparatus transfers its coercive capacity into the gap between the decree and actual practice.
% ABSENT_VOICES: Communities in the interior of preservation networks — elders, ritual specialists, intergenerational transmitters of the prior practice — are not at the table where the decree is drafted. They would testify that internalization requires negotiation, not decree, and that enforcement without legitimacy accelerates demographic drift rather than genuine adoption. Their absence from the decision seat is structural: the state frames the decree as a technical/administrative matter, not a legitimacy contest.
% DISAPPEARANCE_RATIONALE: The state reads disappearance as: the world stabilizes into compliance and the prior practice naturally attenuates as a new generation knows only the decreed practice. Communities read disappearance as: the decree collapses the next time enforcement weakens (war, administrative collapse, regime change), and the prior practice re-emerges because it was never truly internalized. The contest is whether the decree's enforceability is endogenous (to the new practice's legitimacy) or exogenous (to institutional power alone).
% FOUNDING_PROBLEM: The state seeks to consolidate territorial identity and administrative capacity by establishing a shared practice framework — a unified calendar, dress code, or language — to reduce the overhead of governing multiple practice systems and to generate a common symbolic order that reinforces state legitimacy. The prior heterogeneous practices represent centrifugal forces: they organize loyalty to local and traditional authorities, not to the state.
% FOUNDING_PROBLEM_CORROBORATION: State authorities and modernization intellectuals attest the founding problem is live and urgent: administrative chaos and identity fragmentation require centralized practice standardization. Communities and historians studying persistence attest the problem is misdiagnosed: the state has confused its own administrative convenience with necessity for social order. External analysts studying state formation note that decrees unaccompanied by internalization pathways face high attrition rates across multiple historical cases (Chinese calendar reform, Ottoman dress codes, colonial language policies).
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness plateaus at 0.68 by year 36 and remains stable thereafter: the decree extracts authority and cultural autonomy from communities and transfers it to the state, but the extraction rate does not increase because internalization has failed. Theater ratio rises throughout the interval (0.35 → 0.58): enforcement activity increasingly defends the constraint's form rather than legitimizing its substance — administrators maintain compliance facades in official records while communities perform code-switching between public and private spheres. Suppression requirement rises and stabilizes at 0.72: the state must maintain high enforcement intensity indefinitely because the prior practice has not been displaced, only suppressed. The coercion grid shows differential trajectories across levels: individual resistance declines slightly (0.68 → 0.62) due to generational diffusion, but organizational and class-level resistance decline more slowly (0.74 → 0.68, 0.71 → 0.66), indicating that community institutions are sustaining the prior practice more effectively than individual households. Structural-level resistance declines (0.65 → 0.58), signaling that the state's institutional capacity is growing, but that growth is not producing endogenous adoption — it is producing surveillance and enforcement capacity instead. The reading asserts that unless internalization mechanisms activate (schooling indoctrination, economic incentives, symbolic prestige), the constraint will remain extractive and theater-heavy, vulnerable to collapse if enforcement capacity weakens.
 *
 * PERSPECTIVAL GAP:
 *   From the state seat: the decree is a coordination mechanism — it solves the administrative problem of heterogeneous practice and creates a shared symbolic order. Internalization is secondary; compliance is primary. From the community seat: the decree is pure extraction because it transfers legitimate practice choice to an external authority and offers nothing in exchange except the right to remain unmolested if outward compliance is maintained. From the enforcement seat: the decree is a tangled rope because it coordinates the state's administrative reach (the coordinated function) but extracts continuous labor from the enforcement apparatus to maintain that reach (the extractive component). These seats compute different types from the same structural data because their exit options and power relationships differ. The engine captures this divergence; the authored claim (tangled_rope) reflects the most general structural assessment, but per-seat computation should show state as rope, communities as snare, enforcement as tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus: d ≈ 0.15 (beneficiary, powerful, arbitrage exit via regime change, but for this regime cycle acts as if institutional). Communities preserving autonomy: d ≈ 0.85 (target, powerless or moderate power, trapped or identity_locked exit, bearing the cost of dual compliance and cultural erosion). Urban centers: d ≈ 0.62 (payer, organized power, mobile exit within the state system but constrained by institutional incentives). Peripheral communities: d ≈ 0.90 (target, powerless, trapped exit, highest suppression exposure but lowest enforcement presence paradoxically creates some buffer). Enforcement apparatus: d ≈ 0.55 (symmetric, institutional power, constrained exit — they administer the constraint but their budget and capacity depend on the constraint's persistence and resistance). Generational subjects: d ≈ 0.75 (target, powerless, identity_locked exit via schooling and household belonging, experiencing the constraint as inescapable).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state consolidation and administrative efficiency) is live from the state's frame but contested from the community frame. The disappearance verdict is also contested: if the decree collapsed tomorrow, the state reads catastrophe (reversion to heterogeneous practice, loss of unified symbol order, administrative chaos), but communities read restoration (return to legitimate cultural autonomy). The rising theater ratio (0.35 → 0.58) and plateau of extractiveness (0.68 by year 36) suggest that the founding problem has been partially solved (formal compliance is achieved and sustained) but at the cost of escalating theater — the state's records show success, but actual practice shows the prior practice persists in private spheres. This is the signature of a constraint whose founding problem has shifted: it began as a coordination/consolidation problem but has become a legitimacy maintenance problem. The constraint persists not because it solves the original problem but because administrative records require the appearance of solution. That shift is the mandatrophy alert: the founding problem's solution is being performed, not achieved. Communities are not internalized; they are compliant, which is structurally different.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_vs_performative_compliance,
    'Is the measured compliance rate (formal adoption in official records) tracking genuine internalization of the new practice, or performative code-switching that masks continued commitment to the prior practice?',
    'Multi-generational ethnographic follow-up in peripheral communities, measuring private practice retention decades after the decree''s enactment. If the prior practice persists in household and ritual contexts despite two generations of state schooling and enforcement, compliance was performative; if it attenuates, internalization was occurring.',
    'If performative, the constraint is extraction (coercive displacement of cultural autonomy) riding on a tangled-rope coordination apparatus; if internalized, the constraint is a genuine rope (communities adopted the practice as legitimate). The reading claims performative, making the constraint extractive and theater-heavy; the exogenous_override reading claims internalized, making compliance sufficient for legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_performative_compliance, empirical, 'Whether the decree produced genuine internalization or merely performative compliance').

omega_variable(
    generational_attrition_mechanism,
    'Does the prior practice attenuate across generations because youth socialized to the new practice find it legitimate, or because enforcement pressure breaks intergenerational transmission chains regardless of legitimacy?',
    'Compare attrition rates in communities with strong enforcement to those with weak enforcement during the same time period. High attrition in both contexts suggests legitimacy-based generational shift; divergent attrition (high where enforcement is strong, low where it is weak) suggests enforcement-driven rather than legitimacy-driven displacement.',
    'If enforcement-driven, the constraint is a snare with high suppression and theater, vulnerable to collapse if enforcement weakens. If legitimacy-driven, it is a rope showing genuine adoption. This reading predicts enforcement-driven; the hybrid_scaffolding reading predicts mixed mechanisms with scaffolding accelerating legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_attrition_mechanism, empirical, 'Whether generational attrition is driven by legitimacy or by enforcement pressure').

omega_variable(
    community_solidarity_binding,
    'Is the persistence of the prior practice in peripheral communities explained by structural factors (weak enforcement, geographic isolation) or by cultural-ideological factors (genuine community belief that the prior practice is legitimate)?',
    'Historical analysis of coded communication, ritual persistence, and leadership statements in peripheral communities during the decree''s enforcement phase. If the prior practice is defended with reference to tradition, autonomy, or moral superiority, community solidarity is binding; if defended only as a resistance strategy against state authority, solidarity is structural.',
    'If cultural, communities possess the capacity to sustain the prior practice indefinitely and reject internalization; if structural, communities will abandon the prior practice if enforcement weakens or alternatives become advantageous. This reading assumes cultural solidarity; the exogenous reading assumes structural constraints are sufficient.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(community_solidarity_binding, conceptual, 'Whether peripheral community practice retention is driven by genuine belief in the prior practice''s legitimacy or merely by structural resistance to enforcement').

omega_variable(
    sibling_reading_empirical_differentiation,
    'What observable pattern would differentiate this endogenous_climb_reading from the exogenous_override_reading and the hybrid_scaffolding_reading?',
    'Measure compliance trajectory, internalization signals (ideology adoption, identity fusion, intergenerational transmission), and enforcement intensity over time. Endogenous reading predicts: rising theater (performative compliance masking private retention), stabilizing extractiveness (enforcement reaching a steady state without driving internalization), high resistance in peripheral communities despite low enforcement. Exogenous reading predicts: declining theater, declining resistance, genuine adoption indicators. Hybrid reading predicts: intermediate patterns with ideological messaging accelerating internalization compared to decree alone.',
    'This reading''s empirical signature is the plateau of extractiveness at 0.68 and rising theater to 0.58 — the pattern of a constraint sustained by enforcement, not legitimacy. If the data instead showed declining resistance and internalization signals, the exogenous reading would be better supported.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_empirical_differentiation, empirical, 'What empirical patterns differentiate the three sibling readings of the legitimacy kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t6, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement_basis(legi_tr_t6, observed).
narrative_ontology:measurement(legi_tr_t12, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement_basis(legi_tr_t12, observed).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 24, 0.55).
narrative_ontology:measurement_basis(legi_tr_t24, observed).
narrative_ontology:measurement(legi_tr_t36, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 36, 0.57).
narrative_ontology:measurement_basis(legi_tr_t36, observed).
narrative_ontology:measurement(legi_tr_t48, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 48, 0.58).
narrative_ontology:measurement_basis(legi_tr_t48, observed).
narrative_ontology:measurement(legi_tr_t60, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 60, 0.58).
narrative_ontology:measurement_basis(legi_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t6, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(legi_be_t6, observed).
narrative_ontology:measurement(legi_be_t12, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(legi_be_t12, observed).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(legi_be_t24, observed).
narrative_ontology:measurement(legi_be_t36, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 36, 0.68).
narrative_ontology:measurement_basis(legi_be_t36, observed).
narrative_ontology:measurement(legi_be_t48, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 48, 0.68).
narrative_ontology:measurement_basis(legi_be_t48, observed).
narrative_ontology:measurement(legi_be_t60, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(legi_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t6, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 6, 0.56).
narrative_ontology:measurement_basis(legi_su_t6, observed).
narrative_ontology:measurement(legi_su_t12, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 12, 0.63).
narrative_ontology:measurement_basis(legi_su_t12, observed).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement_basis(legi_su_t24, observed).
narrative_ontology:measurement(legi_su_t36, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 36, 0.72).
narrative_ontology:measurement_basis(legi_su_t36, observed).
narrative_ontology:measurement(legi_su_t48, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 48, 0.72).
narrative_ontology:measurement_basis(legi_su_t48, observed).
narrative_ontology:measurement(legi_su_t60, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(legi_su_t60, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=60
narrative_ontology:measurement(legi_grid_01, legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse(class), 0, 0.35).
narrative_ontology:measurement(legi_grid_02, legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse(class), 60, 0.41).
narrative_ontology:measurement(legi_grid_03, legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse(individual), 0, 0.38).
narrative_ontology:measurement(legi_grid_04, legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse(individual), 60, 0.45).
narrative_ontology:measurement(legi_grid_05, legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse(organizational), 0, 0.52).
narrative_ontology:measurement(legi_grid_06, legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse(organizational), 60, 0.58).
narrative_ontology:measurement(legi_grid_07, legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse(structural), 0, 0.48).
narrative_ontology:measurement(legi_grid_08, legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse(structural), 60, 0.44).
narrative_ontology:measurement(legi_grid_09, legitimacy_of_imposed_practice__endogenous_climb_reading, resistance(class), 0, 0.71).
narrative_ontology:measurement(legi_grid_10, legitimacy_of_imposed_practice__endogenous_climb_reading, resistance(class), 60, 0.66).
narrative_ontology:measurement(legi_grid_11, legitimacy_of_imposed_practice__endogenous_climb_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(legi_grid_12, legitimacy_of_imposed_practice__endogenous_climb_reading, resistance(individual), 60, 0.62).
narrative_ontology:measurement(legi_grid_13, legitimacy_of_imposed_practice__endogenous_climb_reading, resistance(organizational), 0, 0.74).
narrative_ontology:measurement(legi_grid_14, legitimacy_of_imposed_practice__endogenous_climb_reading, resistance(organizational), 60, 0.68).
narrative_ontology:measurement(legi_grid_15, legitimacy_of_imposed_practice__endogenous_climb_reading, resistance(structural), 0, 0.65).
narrative_ontology:measurement(legi_grid_16, legitimacy_of_imposed_practice__endogenous_climb_reading, resistance(structural), 60, 0.58).
narrative_ontology:measurement(legi_grid_17, legitimacy_of_imposed_practice__endogenous_climb_reading, stakes_inflation(class), 0, 0.38).
narrative_ontology:measurement(legi_grid_18, legitimacy_of_imposed_practice__endogenous_climb_reading, stakes_inflation(class), 60, 0.52).
narrative_ontology:measurement(legi_grid_19, legitimacy_of_imposed_practice__endogenous_climb_reading, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(legi_grid_20, legitimacy_of_imposed_practice__endogenous_climb_reading, stakes_inflation(individual), 60, 0.58).
narrative_ontology:measurement(legi_grid_21, legitimacy_of_imposed_practice__endogenous_climb_reading, stakes_inflation(organizational), 0, 0.55).
narrative_ontology:measurement(legi_grid_22, legitimacy_of_imposed_practice__endogenous_climb_reading, stakes_inflation(organizational), 60, 0.68).
narrative_ontology:measurement(legi_grid_23, legitimacy_of_imposed_practice__endogenous_climb_reading, stakes_inflation(structural), 0, 0.51).
narrative_ontology:measurement(legi_grid_24, legitimacy_of_imposed_practice__endogenous_climb_reading, stakes_inflation(structural), 60, 0.62).
narrative_ontology:measurement(legi_grid_25, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression(class), 0, 0.48).
narrative_ontology:measurement(legi_grid_26, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression(class), 60, 0.71).
narrative_ontology:measurement(legi_grid_27, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression(individual), 0, 0.45).
narrative_ontology:measurement(legi_grid_28, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression(individual), 60, 0.68).
narrative_ontology:measurement(legi_grid_29, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression(organizational), 0, 0.52).
narrative_ontology:measurement(legi_grid_30, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression(organizational), 60, 0.74).
narrative_ontology:measurement(legi_grid_31, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression(structural), 0, 0.51).
narrative_ontology:measurement(legi_grid_32, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression(structural), 60, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (endogenous_climb_reading) of a contested kernel (legitimacy_of_imposed_practice) that decomposes into three structurally distinct claims about how imposed practices displace prior practice. The reading differs from its siblings in the ε referent: the endogenous reading measures extractiveness of the state's decree as assessed by the reading's own lights (high extraction because legitimacy cannot be imposed), while the exogenous reading measures it as decree-sufficient (lower extraction because authority is legitimate by definition), and the hybrid reading measures it as scaffolding-dependent (intermediate extraction). Each reading has its own beneficiary/victim structure, founding_problem assessment, and empirical signature. The three stories are linked by network.affects_constraints because they share a kernel and compete for interpretive authority over the same historical facts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__endogenous_climb_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
