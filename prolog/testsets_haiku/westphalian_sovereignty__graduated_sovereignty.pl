% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty Assessment and Reclassification Regime
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The graduated sovereignty reading of the Westphalian kernel asserts that
 *   sovereignty is not binary or absolute but exists on a spectrum determined
 *   by state capacity (institutional ability to provide public goods, enforce
 *   contracts, maintain monopoly of force) and governance legitimacy (whether
 *   a state's authority is endorsed by its population and meets international
 *   standards of rights protection and rule of law). This reading authorizes
 *   external actors—multilateral institutions, powerful states, and assessor
 *   bodies—to classify states on this spectrum and to impose conditionality,
 *   intervene, or restructure governance in states deemed low-capacity or
 *   low-legitimacy. The structural delta from sibling readings is precisely
 *   this: external discretion to reclassify states, and the transformation of
 *   sovereignty from a status held by consent into a graduated achievement
 *   conditional on external approval. Weak states and post-colonial states
 *   become the victims of reclassification; high-capacity states and external
 *   interveners become the beneficiaries of the authority to assess and
 *   impose conditions.
 *
 * KEY AGENTS:
 *   - High-capacity states (USA, EU, Japan, etc.): institutional power, arbitrage exit, define and assess standards
 *   - External interveners (IMF, World Bank, UN Security Council permanent members): institutional power, collect authority to condition and intervene
 *   - Western governance assessors (academic bodies, policy institutions, expert networks): institutional power, set metrics and certification
 *   - Low-capacity states (fragile states, post-conflict states): powerless, identity-locked, perpetually subject to reclassification
 *   - Post-colonial states (inherited institutional deficits, perpetual assessment targets): moderate power, constrained exit, structurally incentivized to mimic interveners
 *   - States with contested legitimacy (disputed governments, civil war, revolutionary transitions): moderate power, constrained exit, legitimacy itself becomes externally-evaluated
 *   - Absolute sovereignty defenders (excluded from assessment bodies): moderate power, trapped by the classification regime itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.62).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.71).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.62).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty Assessment and Reclassification Regime").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '6867088a-a5b8-444f-9fa7-7ecc39fd06fe').
narrative_ontology:cs_kernel_codification('6867088a-a5b8-444f-9fa7-7ecc39fd06fe', formalized).
narrative_ontology:cs_authority_grounding('6867088a-a5b8-444f-9fa7-7ecc39fd06fe', extraction).
narrative_ontology:cs_interpretation_layer_present('6867088a-a5b8-444f-9fa7-7ecc39fd06fe').
narrative_ontology:cs_reading_relation('6867088a-a5b8-444f-9fa7-7ecc39fd06fe', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('6867088a-a5b8-444f-9fa7-7ecc39fd06fe', westphalian_sovereignty__conditional_sovereignty, influences).
narrative_ontology:cs_axiom('6867088a-a5b8-444f-9fa7-7ecc39fd06fe', foundational, sovereignty_graduated_spectrum).
narrative_ontology:cs_axiom_status(sovereignty_graduated_spectrum, holdable).
narrative_ontology:cs_axiom_grounding('6867088a-a5b8-444f-9fa7-7ecc39fd06fe', sovereignty_graduated_spectrum, empirically_contingent).
narrative_ontology:cs_axiom('6867088a-a5b8-444f-9fa7-7ecc39fd06fe', foundational, external_assessment_legitimacy).
narrative_ontology:cs_axiom_status(external_assessment_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6867088a-a5b8-444f-9fa7-7ecc39fd06fe', external_assessment_legitimacy, conventional).
narrative_ontology:cs_reference_frame('6867088a-a5b8-444f-9fa7-7ecc39fd06fe', capacity_contingent_authority).
narrative_ontology:cs_drift_state('6867088a-a5b8-444f-9fa7-7ecc39fd06fe', contemporary_institutional_proliferation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6867088a-a5b8-444f-9fa7-7ecc39fd06fe', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, high_capacity_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, external_interveners).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, western_governance_assessors).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, low_capacity_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, post_colonial_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, states_with_contested_legitimacy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, subnational_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Institutionalize assessments of state capacity and governance legitimacy, controlling the metrics, the assessment bodies, and the remedial consequences. Justify interventions in weak or contested states as conditional on their failure to meet graduated sovereignty standards. Capture the authority to determine which states qualify as fully sovereign and which must accept external tutelage or conditionality.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, high_capacity_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, high_capacity_states, beneficiary).

% Gain discretionary authority to intervene in, govern, or condition aid to states classified as low-capacity or low-legitimacy. Operate multilateral institutions (IMF, World Bank, UN Security Council) that embed capacity assessment and conditionality. Frame interventions as remedial support rather than extraction.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, external_interveners, beneficiary,
    institutional, generational, arbitrage, global).

% Academic, policy, and multilateral expert communities that define what counts as legitimate governance and adequate state capacity. Their assessments provide the epistemic authority for graduated sovereignty classifications. Shape which states are certified as meeting standards and which face remedial requirements.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, western_governance_assessors, agenda_setter,
    institutional, generational, arbitrage, global).

% Face continuous reclassification as low-capacity or low-legitimacy. Subject to conditionality on aid, loans, and trade access. Must implement governance reforms designed externally to prove they meet graduated sovereignty thresholds. Cannot exit the assessment regime without losing access to critical finance and markets; sovereignty is conditional on passing external review.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, low_capacity_states, payer,
    powerless, generational, identity_locked, global).

% Inherit institutional legacies from colonial administrations and are perpetually assessed as failing to meet Western governance standards. Face interventions justified as helping them build capacity. Confront a structural incentive to replicate the institutional forms of the interveners rather than developing indigenous governance. Sovereignty is framed as achievable through institutional mimicry.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, post_colonial_states, payer,
    moderate, generational, constrained, global).

% Disputed as to whether their governments hold legitimate authority (by whom, and according to which standard?). Face competing assessments from different interveners and standards bodies. Subject to interventions or withdrawal of recognition justified as conditional on legitimacy proof. Legitimacy itself becomes an externally-evaluated criterion they must satisfy, not an internal political achievement.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, states_with_contested_legitimacy, payer,
    moderate, generational, constrained, global).

% States and movements that hold Westphalian or absolute sovereignty readings would argue that graduated sovereignty regimes violate non-interference norms and enable neo-colonialism. They are excluded from the assessment standard-setting bodies and their objections are reframed as anti-accountability or anti-governance rhetoric.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, absolute_sovereignty_defenders, excluded,
    moderate, generational, trapped, global).

% May benefit from graduated sovereignty frameworks when external interveners support their claims against incumbent states, or may face repression when interveners classify the incumbent state as having legitimate authority. Their relationship to the constraint is contingent on the assessment of the state they oppose.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, subnational_movements, beneficiary,
    moderate, biographical, constrained, regional).

% Monitor whether graduated sovereignty assessments operate as capacity support or as neo-colonial reclassification machinery. Examine whether the metrics for capacity and legitimacy are applied uniformly or systematically disadvantage non-Western governance forms.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__graduated_sovereignty, external_interveners).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__graduated_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establish transparent criteria for state capacity and governance legitimacy so that the international community can calibrate support, conditionality, and intervention proportionately rather than arbitrarily.
% TRANSFER_FUNCTION: Transfers authority to assess and judge states (and to impose conditionality) from state governments and their peoples to external experts and interveners. Transfers agency to design governance reforms from elected or indigenous leadership to external advisors and conditionality-setting institutions.
% ABSENT_VOICES: States that contest the legitimacy of external assessment standards have no seat in standard-setting bodies — they are the assessed, not the assessors. Indigenous or non-Western governance frameworks are excluded by definition (they do not match Western bureaucratic capacity definitions). Subnational movements are present only as instrumental tools for interveners, not as agents with autonomous voice.
% DISAPPEARANCE_RATIONALE: If graduated sovereignty assessment regimes disappeared, the authority to intervene in weak states would lose its epistemic justification; states would revert to defending sovereignty on consent-based or absolute grounds; conditionality on aid and loans would lose its legitimacy framing; the global power structure would reorganize around explicit force or alliance rather than capacity certification.
% FOUNDING_PROBLEM: Some states lack the institutional capacity to provide basic public goods (courts, finance, healthcare) and their legitimacy is contested. International actors need a framework for when support is warranted versus when internal governance is adequate. The concern: how to help without imposing, how to establish accountability without enabling coercive intervention.
% FOUNDING_PROBLEM_CORROBORATION: High-capacity states and international institutions attest that state capacity and legitimacy remain variable and justify conditionality and intervention as supportive. Post-colonial states and sovereignty-centered legal traditions attest that the founding problem was solved by decolonization and that graduated sovereignty perpetuates external dependency framed as technical assistance. Subaltern legal scholars and post-colonial analysts corroborate the latter reading with historical documentation of conditionality as neo-colonial extraction.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 at interval end because the graduated sovereignty framework enables systematic extraction of policy autonomy and governance authority from weak states without their consent—authority to set conditions, restructure institutions, and judge legitimacy accrues to external actors. Suppression is 0.71 because dissent from graduated sovereignty framing is suppressed via expert consensus and institutional gatekeeping: objections are reframed as 'anti-accountability' or 'sovereignty maximalism' rather than engaged as legitimate defenses of self-determination. Theater ratio of 0.48 reflects that a genuine coordination function (establishing capacity and legitimacy standards) coexists with a substantial extractive function (authority capture and institutional conditionality that benefits interveners). The measurement series tracks accumulation: extractiveness rises from 0.35 to 0.62 as the graduated sovereignty framework becomes institutionalized, as conditionality spreads, and as low-capacity states find themselves perpetually assessed and reclassified. Theater ratio rises as the framing shifts from humanitarian support to institutional reform conditionality to governance restructuring. Suppression requirement rises as resistance from post-colonial states and sovereignty-defending traditions intensifies and must be suppressed via expert authority and institutional gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of high-capacity states and international institutions, graduated sovereignty is a framework for legitimate conditionality and supportive intervention—a way to help states build capacity and improve governance. From the perspective of low-capacity and post-colonial states, the same framework is a mechanism of neo-colonial extraction: authority over their own governance is transferred to external assessors, conditionality on aid perpetuates dependency, and they face a structural incentive to abandon indigenous institutions and replicate Western bureaucratic forms. The engine should compute this divergence from the structural data: high-capacity institutional actors with arbitrage exit experience the constraint as enabling their beneficial authority (low χ, beneficiary directionality); powerless or identity-locked actors in weak states experience it as coercive reclassification (high χ, target directionality). The authored claim (snare) reflects the target-seat perception; high-capacity seats might perceive it as rope or scaffold (supportive conditionality toward capacity-building). The claim/metric independence principle preserves this gap rather than reconciling it.
 *
 * DIRECTIONALITY LOGIC:
 *   High-capacity states and external interveners are the structural beneficiaries: they control the metrics, the assessment bodies, and the conditionality machinery. Their directionality is near 0.0 (full beneficiary: they collect authority and set rules). Low-capacity and post-colonial states are the targets: they bear the cost of perpetual reclassification, conditionality on aid and trade, and external governance imposition. Their directionality is near 1.0 (full target: they must submit to external assessment and intervention). States with contested legitimacy sit higher on the target end (0.7-0.9) because their legitimacy is the subject of external evaluation—they cannot exit by proving capacity alone; they must satisfy external legitimacy assessments. Subnational movements occupy a split position: when the external intervener opposes the incumbent state they may be beneficiaries (gaining support), but when the intervener supports the incumbent, they are victims. The asymmetry is structural to the constraint: the intervener's classification decision determines whether a subnational movement gains or loses agency.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—'some states lack capacity and legitimacy'—is contested as to whether it is still live. High-capacity states and interveners attest that capacity variation persists and justifies graduated sovereignty. Post-colonial states and sovereignty traditions attest that the problem was solved by decolonization and that graduated sovereignty is a new problem (neo-colonial extraction) disguised as the solution to the old one. The constraint's persistence depends on this contestation: if the founding problem were dead and acknowledged as solved, the graduated sovereignty framework would lose justification and would need to be defended on other grounds (e.g., 'intervention is sometimes justified as humanitarian'). The constraint avoids mandatrophy by keeping the founding problem contested—neither fully alive nor fully dead, leaving the framework in continuous operation. The theater ratio (0.48) reflects this: a substantial fraction of enforcement activity defends the assessment regime itself rather than solving the founding problem, but the problem's contested status preserves the justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_metrics_construct,
    'Are the metrics used to assess state capacity—institutional strength, bureaucratic efficiency, fiscal capacity, security force control—neutral measures of ability, or are they constructed to systematically advantage Western institutional forms and disadvantage alternative governance structures?',
    'Comparative analysis of indigenous governance systems'' outcomes in development, rights protection, and public goods provision versus Western institutions in comparable resource contexts. Cross-cultural institutional assessment not filtered through Western institutional templates.',
    'If the metrics are construct-biased, the assessment regime is not measuring capacity but imposing a specific institutional form as the definition of capacity. The ε and classification would shift toward pure extraction (snare, not rope). If the metrics are culturally neutral, graduated sovereignty may retain some coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_metrics_construct, conceptual, 'Whether capacity metrics are neutral or institutionally constructed.').

omega_variable(
    legitimacy_assessment_locus,
    'Should legitimacy be assessed by external observers or internal populations? Who counts as the judge of whether a government holds legitimate authority?',
    'Empirical measurement of correlation between external legitimacy assessments and internal population support. Cases where the two diverge reveal whose assessment the constraint privileges.',
    'If external assessments diverge from internal consent and the constraint privileges external assessments, the constraint transfers legitimacy judgment from populations to external experts—core extractive function. If internal consent is privileged, graduated sovereignty retains some basis in democratic legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_assessment_locus, preference, 'The locus of legitimacy authority—internal or external.').

omega_variable(
    graduated_vs_absolute_foreclosure,
    'Do the graduated sovereignty and absolute sovereignty readings logically foreclose each other, or can they coexist as live positions held by different parties?',
    'Test whether a state can hold both commitments simultaneously: does accepting that capacity varies logically commit one to accepting external assessment and conditionality? Or can capacity variation be acknowledged while defending absolute non-interference?',
    'If the readings foreclose each other, graduated sovereignty is a contested challenge to the absolute reading, not a reconciliation. If they coexist, the constraint maps onto a stable multipolar legitimacy system rather than a displacement mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduated_vs_absolute_foreclosure, conceptual, 'Logical structure of the relationship between graduated and absolute sovereignty readings.').

omega_variable(
    conditionality_efficacy,
    'Does external conditionality on aid, loans, and trade actually improve state capacity and governance legitimacy, or does it extract policy autonomy while leaving underlying capacity problems unsolved?',
    'Longitudinal study of states subject to IMF/World Bank conditionality: measure capacity and legitimacy before, during, and after conditionality periods. Control for counterfactual (what capacity trajectory would have occurred without conditionality).',
    'If conditionality improves capacity and legitimacy, the constraint has genuine coordination function and is snare-flavored but not pure extraction. If it extracts policy autonomy without improving outcomes, the constraint is pure snare. If it worsens outcomes (dependency trap), the constraint is a snare with degraded performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_efficacy, empirical, 'Whether graduated sovereignty conditionality produces the capacity improvements it justifies itself as producing.').

omega_variable(
    neo_colonial_assessment_bias,
    'Are the actors conducting graduated sovereignty assessments (Western states, IMF, World Bank, academic bodies) sufficiently insulated from benefiting from the assessments they make, or do they systematically assess in ways that justify their own continued authority and conditionality?',
    'Meta-analysis of assessment outcomes: do low-capacity classifications correlate with geopolitical alignment with Western interests, resource dependence, or other factors predictive of intervener benefit? Do assessment standards change over time to keep the assessed states in the low-capacity category?',
    'If assessors benefit from their own assessments, the framework is institutionally captured and the ε should be higher (pure extraction enabling intervener benefit). If assessments are independent, some coordination function is preserved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neo_colonial_assessment_bias, empirical, 'Whether graduated sovereignty assessments are free from institutional capture by benefiting states and institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0, 0.28).
narrative_ontology:measurement(west_tr_t7, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 7, 0.32).
narrative_ontology:measurement(west_tr_t14, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 14, 0.38).
narrative_ontology:measurement(west_tr_t21, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 21, 0.44).
narrative_ontology:measurement(west_tr_t28, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 28, 0.47).
narrative_ontology:measurement(west_tr_t35, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 35, 0.48).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(west_be_t7, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 7, 0.42).
narrative_ontology:measurement(west_be_t14, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 14, 0.51).
narrative_ontology:measurement(west_be_t21, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 21, 0.58).
narrative_ontology:measurement(west_be_t28, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 28, 0.61).
narrative_ontology:measurement(west_be_t35, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(west_su_t7, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 7, 0.59).
narrative_ontology:measurement(west_su_t14, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 14, 0.64).
narrative_ontology:measurement(west_su_t21, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 21, 0.68).
narrative_ontology:measurement(west_su_t28, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 28, 0.7).
narrative_ontology:measurement(west_su_t35, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__graduated_sovereignty, 0.25).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, humanitarian_intervention_doctrine).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, structural_adjustment_programs).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, international_institution_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the WESTPHALIAN KERNEL (state sovereignty). The absolute_sovereignty and conditional_sovereignty constraints are sibling readings from the same kernel. All three share the same referent (the international commitment to state sovereignty) but instantiate different constraint structures: absolute sovereignty denies external authority to assess or condition; conditional sovereignty bases intervention on specific violations; graduated sovereignty institutionalizes continuous assessment and conditionality. The three readings compete to define the legitimacy of external intervention and the scope of state authority. Graduated sovereignty INFLUENCES the others by institutionalizing a regime that displaces binary frameworks and creates a continuous spectrum of authority contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__graduated_sovereignty, powerless, 0.85).
constraint_indexing:directionality_override(westphalian_sovereignty__graduated_sovereignty, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
