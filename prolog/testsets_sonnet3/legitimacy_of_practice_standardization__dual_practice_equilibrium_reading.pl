% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Domain-Partitioned Practice Legitimacy: Gregorian/Bureaucratic vs. Lunar/Ritual Bifurcation
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This story instantiates the dual_practice_equilibrium reading of the
 *   legitimacy_of_practice_standardization kernel: rather than one authority
 *   displacing the other (endogenous_displacement) or the state simply
 *   overriding traditional practice (exogenous_override), this reading holds
 *   that legitimacy itself bifurcates permanently by domain. Gregorian
 *   calendar and Western dress govern taxes, courts, and formal employment;
 *   lunar calendar and traditional dress govern festivals, agriculture, and
 *   home life. Neither domain is expected to converge into the other, and the
 *   population's compliance in each domain is strategic (code-switching to
 *   access what each authority controls) rather than internalized as a single
 *   coherent belief system. The extraction in this reading is not overt
 *   suppression of one practice by the other — it is the invisible
 *   translation tax imposed on those who must operate competently in both
 *   domains simultaneously, borne disproportionately by rural, informal, and
 *   female actors excluded from negotiating either domain's rules.
 *
 * KEY AGENTS:
 *   - state_bureaucracy: agenda_setter/beneficiary (institutional/arbitrage) — governs public domain, gains administrative legibility at no ritual-domain cost
 *   - traditional_ritual_authorities: agenda_setter/beneficiary (organized/constrained) — governs private domain, retains authority by not contesting the public sphere
 *   - rural_agricultural_households: payer (powerless/trapped) — bear the mismatch cost between fiscal and harvest calendars
 *   - women_managing_ritual_labor: payer (powerless/constrained) — bear the doubled and administratively invisible workload the partition assigns to the private sphere
 *   - international_trade_partners: observer (institutional/analytical) — read only the public-facing Gregorian/Western layer as evidence of modernization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.42).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.38).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Domain-Partitioned Practice Legitimacy: Gregorian/Bureaucratic vs. Lunar/Ritual Bifurcation").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/modernization_studies/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '09a0d566-3fbf-40c6-bbe4-5381c0ae35c5').
narrative_ontology:cs_kernel_codification('09a0d566-3fbf-40c6-bbe4-5381c0ae35c5', distributed).
narrative_ontology:cs_authority_grounding('09a0d566-3fbf-40c6-bbe4-5381c0ae35c5', distributed).
narrative_ontology:cs_reading_relation('09a0d566-3fbf-40c6-bbe4-5381c0ae35c5', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('09a0d566-3fbf-40c6-bbe4-5381c0ae35c5', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('09a0d566-3fbf-40c6-bbe4-5381c0ae35c5', foundational, legitimacy_is_domain_indexed_not_universal).
narrative_ontology:cs_axiom_status(legitimacy_is_domain_indexed_not_universal, holdable).
narrative_ontology:cs_axiom_grounding('09a0d566-3fbf-40c6-bbe4-5381c0ae35c5', legitimacy_is_domain_indexed_not_universal, conventional).
narrative_ontology:cs_axiom('09a0d566-3fbf-40c6-bbe4-5381c0ae35c5', foundational, compliance_without_convergence_is_a_stable_end_state).
narrative_ontology:cs_axiom_status(compliance_without_convergence_is_a_stable_end_state, holdable).
narrative_ontology:cs_axiom_grounding('09a0d566-3fbf-40c6-bbe4-5381c0ae35c5', compliance_without_convergence_is_a_stable_end_state, empirically_contingent).
narrative_ontology:cs_reference_frame('09a0d566-3fbf-40c6-bbe4-5381c0ae35c5', pre_partition_unitary_ritual_authority).
narrative_ontology:cs_drift_state('09a0d566-3fbf-40c6-bbe4-5381c0ae35c5', contemporary_dual_domain_settlement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('09a0d566-3fbf-40c6-bbe4-5381c0ae35c5', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, households_navigating_dual_calendars).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_agricultural_households).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, informal_sector_workers).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, women_managing_ritual_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, households_navigating_dual_calendars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers taxes, courts, schooling, and civil registration on the Gregorian calendar and Western dress codes, enforcing compliance in public/administrative domains through licensing, employment eligibility, and legal recognition. Does not attempt to displace lunar or ritual practice inside households or festivals — the partition is deliberate policy, not incomplete conquest. Gains predictable administrative time-coordination and international legibility without bearing the cost of suppressing domestic or ritual life.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_bureaucracy, beneficiary).

% Village elders, temple calendars, and family heads continue to set festival dates, marriage timing, planting cycles, and home dress on the lunar calendar and traditional garb. Their authority is preserved precisely because the state ceded the private/ritual domain rather than contest it. They retain social standing and officiating roles as long as they do not attempt to govern tax filings or civil contracts.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities, agenda_setter,
    organized, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities, beneficiary).

% Wear suits and use the Gregorian calendar at work and in dealings with the state, then switch to kimono and lunar reckoning at home and for festivals. This bifurcation lets them keep both worlds functioning, but requires constant translation work — tracking two calendars, maintaining two wardrobes, code-switching between two legitimacy grammars depending on which counter they are standing at.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, households_navigating_dual_calendars, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, households_navigating_dual_calendars, payer).

% Farming decisions (planting, harvest, market timing) run on lunar/agricultural cycles that do not map cleanly onto the Gregorian fiscal year the state uses for tax deadlines, loan terms, and land registration. The mismatch produces recurring administrative penalties and credit disadvantage timed against a calendar their actual livelihood does not follow. They cannot exit either system: the state controls land title, the lunar calendar controls harvest reality.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_agricultural_households, payer,
    powerless, biographical, trapped, local).

% Work outside formal registration, so they get none of the administrative predictability the Gregorian/Western-dress domain promises the formally employed, while still needing to interface with bureaucratic offices (permits, ID renewal, disputes) that treat any lunar or traditional-dress presentation as evidence of informality or lower status. They pay the compliance cost of the public domain without receiving its legibility benefits.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, informal_sector_workers, payer,
    powerless, immediate, trapped, local).

% Bear the disproportionate labor of maintaining the traditional/ritual domain — festival preparation, ceremonial dress production and upkeep, lunar-calendar household management — that the partition assigns to the 'private' sphere and therefore treats as unpaid and administratively invisible, while often also holding formal jobs that require Gregorian/Western compliance. The domain partition doubles their workload rather than dividing it.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, women_managing_ritual_labor, payer,
    powerless, generational, constrained, local).

% Read the Gregorian/Western-dress administrative surface as evidence of modernization and treaty-readiness, largely unaware of or indifferent to the persistence of the lunar/traditional domain underneath. Their assessment of the country's institutional legitimacy is based on the public-facing layer only.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, international_trade_partners, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Partitioning legitimacy by domain lets the state achieve administrative time-coordination (tax cycles, court calendars, international commerce) without having to fight and lose a total war against deeply held ritual and agricultural time-reckoning — each domain gets a coordination mechanism suited to its actual function.
% TRANSFER_FUNCTION: Moves administrative burden and translation labor onto households and especially onto rural, informal, and female actors who must operate both systems simultaneously, while state bureaucracy and traditional authorities each retain uncontested jurisdiction and legitimacy within their assigned domain at no cost to each other.
% ABSENT_VOICES: Rural agricultural households and informal workers who must reconcile two incompatible temporal grammars have no forum in either the state administrative process or the traditional ritual-authority structure — both of which negotiated the partition without consulting the people who bear its seams. Women managing ritual labor are similarly absent from both the bureaucratic and traditional decision-making bodies that assigned them the invisible half of the workload.
% DISAPPEARANCE_RATIONALE: State bureaucracy and traditional authorities would each say the world stays roughly the same for them if the partition dissolved — each already governs its own domain and would simply expand or contract. But rural households, informal workers, and women managing ritual labor would experience real rearrangement: either forced full assimilation to Gregorian/Western norms (losing agricultural and ritual legibility) or forced full retention of traditional norms (losing administrative access) — the partition's disappearance would resolve their double-burden, one way or the other, which is exactly why the affected parties are split on whether it should end.
% FOUNDING_PROBLEM: Newly bureaucratized or internationally exposed states needed calendar and dress standardization to interoperate with global commerce, taxation, and diplomacy, but full suppression of traditional/ritual practice provoked resistance intense enough to threaten state legitimacy itself — the partition was a negotiated settlement to avoid a legitimacy war neither side could cleanly win.
% FOUNDING_PROBLEM_CORROBORATION: State officials and traditional authorities both attest the partition remains functionally necessary — the former citing continued need for international administrative legibility, the latter citing continued ritual/cultural vitality. Independent ethnographic and labor-economics research from outside both institutions (studies of rural credit penalties and gendered ritual-labor burden) corroborates that the founding problem of avoiding open confrontation was real, but documents that its costs have shifted onto the households doing the translation work rather than being resolved — no source outside the two authorities argues the partition is currently cost-free.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate rather than high because both authorities genuinely deliver something within their domain — administrative interoperability from the state, ritual/social continuity from traditional authority — this is not naked extraction dressed as coordination. Suppression (0.38) is present but declining over the interval as the partition stabilizes into settled expectation rather than contested imposition; the enforcement effort shifts from suppressing cross-domain claims to merely maintaining the boundary. Theater ratio rises substantially (0.25 to 0.55) because as the equilibrium matures, more of what each authority does functions as boundary-performance — ceremonial reaffirmation of jurisdiction — rather than active governance of genuinely contested territory; the domains increasingly police their own edges symbolically once the population has learned the code-switch. Resistance (0.35) is moderate: it comes almost entirely from those who bear the translation cost, not from either authority, since neither authority experiences the arrangement as burdensome.
 *
 * DIRECTIONALITY LOGIC:
 *   State bureaucracy and traditional ritual authorities are structural co-beneficiaries: the partition lets each retain full uncontested legitimacy in its assigned domain at zero cost to the other, which is why they are both agenda_setter and beneficiary. Households navigating dual calendars sit closer to symmetric — they benefit from access to both domains but pay the translation cost, hence the payer secondary role. Rural agricultural households, informal sector workers, and women managing ritual labor are the true targets: they experience the partition as an imposed seam rather than a negotiated convenience, because they lack the resources (legal counsel for the mismatch, formal employment status, recognition of ritual labor as labor) that would let them internalize the partition costlessly the way wealthier dual-calendar households can.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — avoiding a total legitimacy war between state and traditional authority — is genuinely contested rather than simply dead: both authorities can honestly claim their domain still requires active governance. This prevents the classification from being read as pure extraction dressed as settlement. But the founding_problem_corroboration surfaces the mandatrophy candidate precisely at the household level: the problem the ARRANGEMENT was built to solve (elite legitimacy conflict) has been solved, while a NEW cost (translation labor, credit penalties, invisible ritual labor) has grown up inside the settlement and is borne by parties who had no seat at the original negotiation. This is the tangled_rope signature: real coordination function for the negotiating parties, real extraction from the non-negotiating parties, through the identical structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_stability_vs_transition_phase,
    'Is the domain partition a genuinely stable long-run equilibrium (as this reading claims), or is it a slow-motion transition phase that will eventually resolve into either full displacement or full override, making this reading actually a snapshot of the exogenous_override_reading or endogenous_displacement_reading mid-process?',
    'Longitudinal tracking of domain boundary permeability across multiple generations: if the public/private boundary remains fixed or hardens over 50+ years despite continued contact, the equilibrium reading holds; if the boundary erodes systematically in one direction, one of the sibling readings is the more accurate structural description.',
    'If this reading is actually a transition snapshot, the extraction measured here is transitional friction rather than a stable structural feature, and the constraint''s classification would migrate toward whichever sibling reading describes the true endpoint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_stability_vs_transition_phase, empirical, 'Whether the dual-practice partition is a stable equilibrium or a slow transition toward one sibling reading''s outcome.').

omega_variable(
    translation_labor_visibility,
    'Is the translation labor borne by households, rural populations, and women a necessary and unavoidable cost of maintaining two legitimate domains, or is it a cost that could be redistributed or reduced by either authority without threatening either domain''s legitimacy (e.g., harmonized tax deadlines accounting for harvest cycles, formal recognition of ritual labor)?',
    'Comparative case study of jurisdictions that have implemented partial harmonization measures (e.g., flexible fiscal deadlines pegged to agricultural cycles) to test whether translation cost is separable from the coordination benefit.',
    'If separable, the persistence of unmitigated translation cost is closer to extraction riding on a genuine coordination structure (supporting tangled_rope); if inseparable, the cost is closer to an irreducible feature of maintaining two legitimacy domains (pushing toward a more rope-like reading).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(translation_labor_visibility, conceptual, 'Whether the translation burden is a removable extraction or an irreducible cost of dual-domain legitimacy.').

omega_variable(
    cs_framing_kernel_vs_layered_legitimacy_claim,
    'Should the kernel here be read as ''practice legitimacy'' itself (the obvious framing, adopted in this story), or as a higher-order legitimacy claim about WHO gets to declare which domain is public versus private in the first place — a meta-level authority contest this reading''s declared axioms presuppose has already been settled?',
    'Examine historical negotiation records (if extant) for evidence of an explicit or implicit second-order bargain over domain boundaries themselves, distinct from the practices within each domain.',
    'If a distinct meta-level domain-boundary-setting authority exists and is contested, it would constitute a fourth, higher constraint in this family (a domain-boundary-legitimacy kernel) rather than being absorbed into this reading''s axioms; the current framing would then understate the negotiated character of the partition itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_layered_legitimacy_claim, conceptual, 'Whether the practice-legitimacy kernel presupposes an unexamined higher-order domain-boundary-setting authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(legi_tr_t60, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 60, 0.46).
narrative_ontology:measurement(legi_tr_t80, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 80, 0.51).
narrative_ontology:measurement(legi_tr_t100, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(legi_be_t60, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 60, 0.39).
narrative_ontology:measurement(legi_be_t80, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(legi_be_t100, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(legi_su_t60, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(legi_su_t80, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 80, 0.38).
narrative_ontology:measurement(legi_su_t100, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.1).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'legitimacy of practice standardization' per the ε-invariance principle. Each reading (dual_practice_equilibrium, endogenous_displacement, exogenous_override) assigns a different beneficiary/victim structure and a different ε to what a single colloquial label would treat as one debate about modernization legitimacy. This reading's ε (0.42, tangled_rope) reflects a stable domain-partitioned settlement with moderate diffuse extraction on non-negotiating parties; the sibling readings model displacement-driven and override-driven variants with presumably different extraction profiles and beneficiary sets. All three are linked so that contamination or purity analysis on one propagates to the others as evidence about the same underlying kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
