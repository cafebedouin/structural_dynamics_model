% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__integration_primary, []).

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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: EU Free Movement as Constitutive of Citizenship (Integration-Primary Reading)
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   This story instantiates the integration-primary reading of the EU
 *   federation-membership kernel: free movement is not a policy choice member
 *   states can recalibrate but a constitutive element of Union citizenship,
 *   such that national welfare boundaries must yield to mobility rights as
 *   case law develops. Under this reading, ECJ jurisprudence (Dano, Brey, and
 *   successors) progressively enlarges the welfare beneficiary set to include
 *   mobile EU citizens on near-equal terms with nationals, while the
 *   adjustment costs — wage competition, service strain, fiscal burden — land
 *   on displaced local labor and receiving-state tax bases. This is a
 *   genuinely different constraint from the sibling readings:
 *   member_sovereignty_primary would treat national welfare closure as
 *   retained and free movement as conditional (a much lower extraction
 *   profile against local labor, higher against mobile workers denied
 *   access); selective_solidarity would tier access by contribution history
 *   (moderate extraction, different beneficiary/victim mapping entirely,
 *   closer to a rope). The three readings do not share an epsilon — this file
 *   authors only the integration-primary epsilon, from that reading's own
 *   lights, applied to the standing arrangement (current ECJ-shaped free
 *   movement regime) it is about, not to any endorsed alternative.
 *
 * KEY AGENTS:
 *   - mobile_eu_workers: primary beneficiary (moderate/mobile) — gains equal-treatment welfare access via free movement
 *   - displaced_local_labor: primary target (powerless/trapped) — bears wage-competition and service-strain costs
 *   - eu_commission and ecj: agenda-setters (institutional/analytical) — draft and adjudicate the expanding entitlement structure
 *   - member_state_governments: dual-positioned (institutional/constrained) — benefit from outward mobility, pay for inward obligations
 *   - national_courts_and_electorates: excluded — preference for national democratic control subordinated to supranational precedent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.58).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.61).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free Movement as Constitutive of Citizenship (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, '7d20a5af-269a-4a32-a577-f144ddc6f133').
narrative_ontology:cs_kernel_codification('7d20a5af-269a-4a32-a577-f144ddc6f133', fixed_text).
narrative_ontology:cs_authority_grounding('7d20a5af-269a-4a32-a577-f144ddc6f133', lineage).
narrative_ontology:cs_interpretation_layer_present('7d20a5af-269a-4a32-a577-f144ddc6f133').
narrative_ontology:cs_reading_relation('7d20a5af-269a-4a32-a577-f144ddc6f133', federation_membership_obligations__member_sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('7d20a5af-269a-4a32-a577-f144ddc6f133', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('7d20a5af-269a-4a32-a577-f144ddc6f133', foundational, free_movement_constitutive_of_citizenship).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('7d20a5af-269a-4a32-a577-f144ddc6f133', free_movement_constitutive_of_citizenship, conventional).
narrative_ontology:cs_axiom('7d20a5af-269a-4a32-a577-f144ddc6f133', foundational, national_welfare_boundaries_subordinate_to_mobility_rights).
narrative_ontology:cs_axiom_status(national_welfare_boundaries_subordinate_to_mobility_rights, holdable).
narrative_ontology:cs_axiom_grounding('7d20a5af-269a-4a32-a577-f144ddc6f133', national_welfare_boundaries_subordinate_to_mobility_rights, instrumental).
narrative_ontology:cs_reference_frame('7d20a5af-269a-4a32-a577-f144ddc6f133', single_market_completion_mandate).
narrative_ontology:cs_drift_state('7d20a5af-269a-4a32-a577-f144ddc6f133', post_dano_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7d20a5af-269a-4a32-a577-f144ddc6f133', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, single_market_employers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, eu_commission).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, ecj).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, low_income_native_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, member_state_governments).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cross borders to work, drawing on host-state welfare, healthcare, and education systems on equal terms with nationals once residency and work thresholds are met, per ECJ case law extending Article 45 TFEU and citizenship directives. Their exit option is real mobility itself — the constraint is what makes that mobility meaningful rather than merely nominal.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Draw on a continent-wide labor pool without needing to sponsor visas or navigate national immigration bureaucracies, lowering recruitment costs and enabling wage arbitrage across member states. They lobby to preserve and extend free movement precedents.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, single_market_employers, beneficiary,
    organized, generational, arbitrage, continental).

% Drafts and enforces directives operationalizing free movement (2004/38/EC and successors), initiates infringement proceedings against member states that restrict mobile-worker welfare access, and frames free movement as constitutive of Union citizenship rather than a negotiable policy choice.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_commission, agenda_setter,
    institutional, civilizational, analytical, continental).

% Rules in cases like Dano, Brey, and Commission v. UK, progressively defining the boundary of welfare access for mobile citizens. Each ruling both resolves a dispute and expands the Court's own interpretive authority over what member states must yield; it is the primary vehicle by which the integration-primary reading becomes binding law.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, ecj, agenda_setter,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, ecj, beneficiary).

% Compete for jobs and housing against a larger mobile labor pool, absorbing downward wage pressure in low-skill sectors. Lack the mobility, capital, or credentials to exit the national labor market the way mobile workers exit their home states; the free-movement regime is not reciprocally available to them as a remedy.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, displaced_local_labor, payer,
    powerless, biographical, trapped, national).

% Must extend contribution-independent welfare access to newly-arrived mobile citizens under ECJ precedent, straining systems designed around national contribution histories and demographic assumptions. They cannot unilaterally reintroduce residency-based exclusions without risking infringement action; treaty withdrawal is the only full exit, at prohibitive cost.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, receiving_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Fund welfare systems through taxation and bear the fiscal and service-capacity consequences of expanded eligibility, without having voted directly on the free-movement principle that produced the expansion (it was set at treaty and ECJ level, not at national referendum in most cases).
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, low_income_native_taxpayers, payer,
    powerless, biographical, trapped, national).

% Benefit from single-market access and outward mobility for their own emigrant citizens, while bearing the domestic political cost of inward welfare obligations they did not fully anticipate when signing accession treaties. Their formal exit option (Article 50) is real but catastrophically costly, as the UK's withdrawal illustrated.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, member_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, member_state_governments, beneficiary).

% Would prefer welfare boundaries set by national democratic processes and calibrated to contribution history, but this preference is structurally subordinated to ECJ precedent and Commission enforcement once a member state has acceded — national referenda on welfare access specifically are not a mechanism available within the Union framework short of full withdrawal.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, national_courts_and_electorates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__integration_primary, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_obligations__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a genuinely single labor market across 27 member states: workers can move to where jobs exist and employers can hire across the whole continent without per-state immigration friction, which the treaty framers understood as necessary for the single market to function as anything more than a free-trade zone for goods.
% TRANSFER_FUNCTION: Moves welfare, healthcare, and educational entitlements from receiving-state tax bases (funded predominantly by resident/native taxpayers under legacy contribution assumptions) to mobile EU citizens on arrival, and moves labor-market competitive pressure from mobile workers' home-state conditions onto receiving-state local labor.
% ABSENT_VOICES: National electorates who acceded to treaties before ECJ case law expanded welfare-access implications were not consulted on the specific downstream entitlement expansions; displaced local labor has no organized voice comparable to employer lobbies or mobile-worker advocacy networks at the EU institutional level.
% DISAPPEARANCE_RATIONALE: If free movement's constitutive status were reversed overnight, member states would reintroduce residency and contribution tests, mobile-worker welfare access would contract sharply, national labor markets would re-segment, and the single market itself would revert toward being primarily a goods/capital arrangement rather than a labor-mobility union — this is precisely why the reading is contested rather than settled.
% FOUNDING_PROBLEM: The founding problem was making the single market real rather than nominal: a market for goods and capital without labor mobility was judged structurally incomplete, and early guest-worker/bilateral schemes were seen as producing second-class, precarious migrant status incompatible with a union of citizens rather than a trade bloc.
% FOUNDING_PROBLEM_CORROBORATION: The Commission and ECJ attest the founding problem remains live — an incomplete single market without full worker mobility and equal treatment. Independent labor economists and several member-state finance ministries (outside both the mobile-worker and employer beneficiary sets) attest that the original founding problem — market completion — has been substantially achieved by baseline free movement, and that current ECJ welfare-access expansions solve a different, later-arising problem (equal citizenship dignity) that was not part of the original single-market rationale and was never put to national electorates as such.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__integration_primary, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.28 (1993, Maastricht-era, free movement mostly about workers already in employment) to 0.58 (2024, post-Dano/Brey era, extending to job-seekers and economically inactive citizens) reflecting genuine accumulation via case law rather than a fixed design. Suppression climbs in parallel (0.32 to 0.61) as infringement-proceeding enforcement and the near-impossibility of unilateral member-state deviation harden. Theater ratio stays comparatively low and rising slowly (0.10 to 0.28) — the coordination function (a genuinely operating single labor market) is real, not primarily performative, but an increasing share of institutional activity (Commission communications on 'citizenship values') defends the principle rhetorically rather than solving new coordination problems.
 *
 * PERSPECTIVAL GAP:
 *   From the eu_commission/ecj seat, this is coordination completing an unfinished single market — the engine should compute something closer to rope or tangled_rope-as-coordination-dominant from that structural position given institutional power and analytical exit. From the displaced_local_labor seat, the identical structure computes as extraction: trapped exit, powerless standing, and costs delivered through a mechanism they cannot influence or escape. This divergence is exactly what a tangled_rope classification is meant to register — the coordination function is real (single labor market) and the extraction is real and asymmetric (displaced local labor bears costs it did not choose and cannot exit), and both facts hold simultaneously rather than one being a mask for the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers and single-market employers sit near the beneficiary end of directionality: they gain from the arrangement and possess arbitrage-grade or genuinely mobile exit options. Displaced local labor and low-income native taxpayers sit near the target end: trapped exit options, no reciprocal mobility benefit available to them, and they bear costs through the same structure that delivers mobile-worker benefits. Receiving-state welfare systems and member state governments occupy an intermediate, institutionally constrained position — nominal agency (they administer the systems) but effectively locked into ECJ-set obligations, which is why member_state_governments carries a secondary beneficiary role without full directionality relief.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a single market incomplete without labor mobility — is genuinely contested as to whether it remains live in its original form. Baseline free movement (workers moving for jobs) largely completed the original 1993 mandate; the subsequent ECJ-driven expansion to welfare access for job-seekers and economically inactive citizens addresses a different, later problem (equal citizenship dignity) that was layered onto the original coordination function rather than being part of it. Classifying this as tangled_rope rather than snare or pure rope prevents two mislabeling errors: calling it a snare would ignore the real, still-functioning single-market coordination benefit; calling it a rope would ignore that the expansion phase has produced concentrated, undeniable costs on a specific powerless population (displaced local labor) sustained by active enforcement (infringement proceedings) rather than by voluntary continued participation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the integration-primary reading (free movement as constitutive of citizenship, overriding national welfare closure) the correct structural characterization of the current EU legal order, or has the actual settlement drifted toward selective_solidarity (contribution-based tiering) following post-Dano jurisprudence and the 2016 UK renegotiation package?',
    'Track the ratio of ECJ rulings expanding versus narrowing welfare access for economically inactive mobile citizens post-2014; a sustained narrowing trend would indicate the operative kernel reading has shifted toward selective_solidarity even while integration_primary remains the official doctrinal self-description.',
    'If the operative reading has shifted, this story''s extractiveness trajectory would need to be understood as measuring a reading that is doctrinally dominant but increasingly counter-factual in practice — a genealogical lag between stated and applied kernel reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether integration_primary remains the operative reading or has been functionally superseded by selective_solidarity in practice.').

omega_variable(
    displaced_labor_causal_attribution,
    'How much of the wage and employment pressure experienced by displaced_local_labor is causally attributable to free movement specifically, versus automation, deindustrialization, and other concurrent labor market shifts?',
    'Labor economics literature comparing regions with high versus low EU-mobile-worker concentration, controlling for sectoral composition and automation exposure, would isolate the free-movement-specific component of wage pressure.',
    'If the causal contribution is small relative to other factors, the victim designation for displaced_local_labor overstates this constraint''s extractive weight relative to other economic forces; if large, it corroborates the tangled_rope classification''s asymmetric-extraction gate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_labor_causal_attribution, empirical, 'Causal weight of free movement versus other economic forces in displaced-labor wage pressure.').

omega_variable(
    beneficiary_vindicated_proposition_boundary,
    'Is ''equal citizenship dignity'' (the normative principle the ECJ increasingly cites to justify welfare-access expansion) a vindicated proposition that collects no rents itself, or does its invocation function as cover for a beneficiary group (mobile workers, EU institutions accruing legitimacy) that does collect?',
    'Compare rulings'' stated rationale (dignity/equality) against the material distribution of costs and benefits in each case; systematic correlation between dignity-rationale invocation and material benefit accrual to mobile citizens would suggest the proposition functions partly as cover.',
    'If the dignity rationale is substantially cover, the tangled_rope classification is reinforced (extraction dressed as principle); if it is a genuinely separable normative commitment, the coordination-function description should be revised to include a dignity-coordination component distinct from labor-market coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_vindicated_proposition_boundary, conceptual, 'Whether the equal-dignity rationale is a genuine vindicated proposition or cover for material beneficiary interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1993, federation_membership_obligations__integration_primary, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(fede_tr_t1999, federation_membership_obligations__integration_primary, theater_ratio, 1999, 0.13).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_obligations__integration_primary, theater_ratio, 2004, 0.17).
narrative_ontology:measurement(fede_tr_t2010, federation_membership_obligations__integration_primary, theater_ratio, 2010, 0.21).
narrative_ontology:measurement(fede_tr_t2015, federation_membership_obligations__integration_primary, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(fede_tr_t2019, federation_membership_obligations__integration_primary, theater_ratio, 2019, 0.26).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_obligations__integration_primary, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t1993, federation_membership_obligations__integration_primary, base_extractiveness, 1993, 0.28).
narrative_ontology:measurement(fede_be_t1999, federation_membership_obligations__integration_primary, base_extractiveness, 1999, 0.34).
narrative_ontology:measurement(fede_be_t2004, federation_membership_obligations__integration_primary, base_extractiveness, 2004, 0.42).
narrative_ontology:measurement(fede_be_t2010, federation_membership_obligations__integration_primary, base_extractiveness, 2010, 0.49).
narrative_ontology:measurement(fede_be_t2015, federation_membership_obligations__integration_primary, base_extractiveness, 2015, 0.53).
narrative_ontology:measurement(fede_be_t2019, federation_membership_obligations__integration_primary, base_extractiveness, 2019, 0.56).
narrative_ontology:measurement(fede_be_t2024, federation_membership_obligations__integration_primary, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1993, federation_membership_obligations__integration_primary, suppression_requirement, 1993, 0.32).
narrative_ontology:measurement(fede_su_t1999, federation_membership_obligations__integration_primary, suppression_requirement, 1999, 0.38).
narrative_ontology:measurement(fede_su_t2004, federation_membership_obligations__integration_primary, suppression_requirement, 2004, 0.46).
narrative_ontology:measurement(fede_su_t2010, federation_membership_obligations__integration_primary, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement(fede_su_t2015, federation_membership_obligations__integration_primary, suppression_requirement, 2015, 0.57).
narrative_ontology:measurement(fede_su_t2019, federation_membership_obligations__integration_primary, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(fede_su_t2024, federation_membership_obligations__integration_primary, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__integration_primary, 0.12).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, selective_solidarity).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the federation_membership_obligations kernel. integration_primary (this file) authors free movement as constitutive and welfare boundaries as subordinate, producing a tangled_rope with mobile workers/employers/EU institutions as beneficiaries and displaced local labor/receiving-state welfare systems/native taxpayers as victims. member_sovereignty_primary authors the inverse structural priority (national closure retained, mobility conditional), which inverts much of the beneficiary/victim mapping. selective_solidarity authors a contribution-based tiering that produces a different, more rope-like structure with narrower victim exposure. All three share the same underlying kernel text and treaty history but are structurally distinct constraints with independent epsilon values — they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__integration_primary, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
