% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__physical_appropriation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: takings_clause_boundary__physical_appropriation_reading
 *   human_readable: Fifth Amendment Takings Clause: Physical Appropriation Doctrine
 *   domain: constitutional/property
 *
 * SUMMARY:
 *   The Fifth Amendment Takings Clause—'nor shall private property be taken
 *   for public use, without just compensation'—is subject to competing
 *   doctrinal readings about when government action triggers the compensation
 *   obligation. The physical-appropriation reading draws a bright line: only
 *   direct physical seizures or permanent physical occupations count as
 *   takings; regulations that diminish property value or restrict use,
 *   however severe, do not. This reading benefits regulatory agencies and
 *   public-good beneficiaries by keeping compensation obligations narrow and
 *   budgetable. It extracts substantial costs from property owners subject to
 *   value-destroying regulations, who receive no compensation. The doctrine
 *   is claimed as coordination (establishing a clear, judicially
 *   administrable boundary); the metrics reflect substantial active
 *   enforcement of the boundary against property owners seeking to expand
 *   takings protection. The claim/metric divergence is intentional: this
 *   reading frames itself as coordination-enabling clarity, but the
 *   structural data shows asymmetric extraction from regulated property
 *   owners.
 *
 * KEY AGENTS:
 *   - property_owners_physically_dispossessed — direct victims of physical appropriation; narrow, protected class
 *   - property_owners_regulated — broad class bearing uncompensated regulatory costs; no takings protection
 *   - regulatory_agencies — agenda-setters with broad rulemaking power and no compensation obligation for regulations
 *   - legislative_bodies — agenda-setters and beneficiaries; can pursue broad regulatory goals without triggering compensation
 *   - public_beneficiaries — organized class collecting public goods (environmental protection, orderly development) at property-owner expense
 *   - constitutional_courts — observers applying the bright-line test and arbitrating boundary contests
 *   - environmental_advocacy_organizations — beneficiaries of low-cost regulation; partly excluded from doctrinal conversation
 *   - property_rights_advocates — excluded; argue for broader takings protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.68).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.45).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Fifth Amendment Takings Clause: Physical Appropriation Doctrine").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional/property").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, 'fe26ad68-f2a7-4510-86be-28c6762ef74e').
narrative_ontology:cs_kernel_codification('fe26ad68-f2a7-4510-86be-28c6762ef74e', fixed_text).
narrative_ontology:cs_authority_grounding('fe26ad68-f2a7-4510-86be-28c6762ef74e', lineage).
narrative_ontology:cs_interpretation_layer_present('fe26ad68-f2a7-4510-86be-28c6762ef74e').
narrative_ontology:cs_reading_relation('fe26ad68-f2a7-4510-86be-28c6762ef74e', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe26ad68-f2a7-4510-86be-28c6762ef74e', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('fe26ad68-f2a7-4510-86be-28c6762ef74e', foundational, physical_appropriation_is_sole_takings_trigger).
narrative_ontology:cs_axiom_status(physical_appropriation_is_sole_takings_trigger, holdable).
narrative_ontology:cs_axiom_grounding('fe26ad68-f2a7-4510-86be-28c6762ef74e', physical_appropriation_is_sole_takings_trigger, deontological).
narrative_ontology:cs_axiom('fe26ad68-f2a7-4510-86be-28c6762ef74e', foundational, regulation_distinct_from_taking_without_compensation).
narrative_ontology:cs_axiom_status(regulation_distinct_from_taking_without_compensation, holdable).
narrative_ontology:cs_axiom_grounding('fe26ad68-f2a7-4510-86be-28c6762ef74e', regulation_distinct_from_taking_without_compensation, conventional).
narrative_ontology:cs_reference_frame('fe26ad68-f2a7-4510-86be-28c6762ef74e', physical_appropriation_boundary).
narrative_ontology:cs_drift_state('fe26ad68-f2a7-4510-86be-28c6762ef74e', contemporary_environmental_regulation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fe26ad68-f2a7-4510-86be-28c6762ef74e', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, regulatory_agencies).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, legislative_bodies).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, property_owners_physically_dispossessed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, public_beneficiaries).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, environmental_advocacy_organizations).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, property_owners_regulated).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, narrow_takings_doctrine).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, regulatory_deference_presumption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Landowners whose property is directly seized or permanently occupied by the government (e.g., land condemned for public use, airspace appropriated for flight corridors, surface access denied by permanent easement). They bear complete loss of the taken property interest. Under this reading, they have a clear constitutional claim to compensation; outside this narrow category, they receive no protection even if regulations destroy property value. They are trapped: property cannot be relocated and the taking is involuntary.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_owners_physically_dispossessed, payer,
    moderate, biographical, trapped, national).

% Landowners whose property value is substantially diminished or use is severely restricted by environmental, zoning, or safety regulations that do not involve physical appropriation (e.g., wetlands protection preventing development, historic preservation restricting alteration, setback requirements reducing buildable area). Under this reading, they have NO constitutional takings claim even if the regulation destroys 95% of property value. They must absorb the loss as part of the regulatory background. Their exit is constrained: they can sell at diminished value, abandon the land, or challenge on other constitutional grounds, but takings doctrine does not protect them.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_owners_regulated, payer,
    moderate, biographical, constrained, national).

% Federal and state agencies that implement environmental, land-use, and safety regulations. Under this reading, they retain broad power to regulate without compensation obligation so long as they do not physically appropriate property. The physical appropriation boundary gives them a clear, bright-line rule: regulate freely unless crossing into direct seizure or permanent occupation. They have substantial arbitrage: they can adopt regulations affecting property value, shift boundaries between regulation and taking through legislative definition, and navigate between state and federal authority. Their enforcement machinery consists of administrative denial of permits, regulatory orders, and exclusion from use—techniques that do not trigger takings protection.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, regulatory_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Congress and state legislatures that enact environmental, zoning, and public-safety statutes. This reading benefits them: they can pursue broad regulatory goals (species protection, urban planning, flood control) without triggering compensation mandates for every property owner harmed by the regulations. The bright-line rule keeps compensation costs low and budgetable (limited to actual seizures and occupations, not to all value-diminishing regulations). They have arbitrage in framing policy choices: they can direct agencies toward regulatory mechanisms rather than appropriation, or shift the taking/regulation boundary through statutory definition.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, legislative_bodies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__physical_appropriation_reading, legislative_bodies, beneficiary).

% Citizens who benefit from regulation-enabled public goods (clean water, wildlife habitat, orderly urban development, safe buildings). They gain these goods at the expense of property owners whose regulatory burdens are uncompensated. Under this reading, their interest is protected by the narrow takings doctrine: regulations can protect environmental and public-safety values without triggering compensation obligations that would price out protective statutes. They benefit from the asymmetry—they collect the goods, property owners bear the costs.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, public_beneficiaries, beneficiary,
    organized, generational, mobile, national).

% Federal courts interpreting the Fifth Amendment Takings Clause. Under this reading, they apply a bright-line test: physical appropriation triggers compensation; regulation does not, regardless of economic impact. This simplifies judicial review to a categorical inquiry—Is property physically taken?—rather than multifactor balancing tests that require judgment about the degree of harm, public benefit, and reasonable expectations. Courts have analytical exit: they can shift doctrinal frameworks through reinterpretation (to regulatory-takings or categorical-takings readings) but are operationally committed to administering whichever doctrine applies.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% NGOs advocating for species protection, climate mitigation, and ecosystem preservation. This reading benefits them by keeping compensation costs low for broad environmental regulations. However, they are partly excluded from the doctrinal conversation: the Takings Clause frames the debate around property-owner rights, not around public-interest benefits. Their voices are present in legislative advocacy and regulatory comment but structurally marginal to the takings doctrine itself.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, environmental_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__physical_appropriation_reading, environmental_advocacy_organizations, excluded).

% Organizations arguing that regulatory diminution of property value should trigger takings protection. Under this reading, they are structurally excluded: the bright-line rule explicitly rejects their position that regulations causing substantial harm warrant compensation. They would argue for a broader takings doctrine (regulatory or categorical readings) but are kept out of the operative doctrinal boundary by the physical appropriation constraint itself.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_rights_advocates, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__physical_appropriation_reading, regulatory_agencies).
narrative_ontology:fixing_cost_class(takings_clause_boundary__physical_appropriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates property governance across jurisdictions and property types by establishing a bright-line rule: government can regulate without compensation cost so long as it does not physically appropriate. This enables uniform land-use, environmental, and safety regulation without decentralized negotiations over just compensation for every diminished property interest.
% TRANSFER_FUNCTION: Transfers the cost of regulatory protection (environmental, urban planning, safety standards) from government budgets and regulated industries to property owners whose land value is diminished by regulations they do not consent to and receive no compensation for. Only direct physical appropriations generate compensable interests; regulatory harms are borne as background risk.
% ABSENT_VOICES: Property owners subject to value-destroying but non-appropriative regulation; environmental advocates concerned that compensation requirements would price regulatory protection out of reach; state and local governments that would face compensation claims if the doctrine broadened to cover all value-diminishing regulation. The doctrine structurally excludes their participation: it draws the boundary unilaterally, and property owners harmed by regulation have limited avenues to contest the boundary itself within takings doctrine (they must shift to statutory or constitutional-equality claims).
% DISAPPEARANCE_RATIONALE: If the physical-appropriation boundary vanished and were replaced by a regulatory-takings or categorical-takings doctrine, government compensation obligations would expand dramatically. Agencies would face takings claims for regulations that diminish property value; budgets for environmental, zoning, and safety programs would shift to compensation rather than protective action; regulatory velocity would slow as compensation costs priced out rules affecting property value. The property-governance landscape would reorganize around compensation thresholds rather than categorical appropriation.
% FOUNDING_PROBLEM: Early takings doctrine was uncertain about which government actions trigger compensation: seizures clearly did, but regulatory diminution of value was contested. The Fifth Amendment promised just compensation for property taken but was silent on the boundary between taking and regulation. The physical-appropriation reading answered this by drawing a bright line: only direct seizure or permanent occupation counts as taking; regulation does not, however severe the economic harm.
% FOUNDING_PROBLEM_CORROBORATION: Courts and property-rights scholars defend the reading as solving the boundary problem with judicial clarity. Regulatory agencies and legislatures attest it enables broad protective regulation without compensation cost. Environmental advocates and public-interest scholars attest it protects environmental law from compensation claims that would disable protection. However, property-rights advocates and owners of regulated land attest the boundary is unjust and has shifted as courts have explored regulatory-takings doctrine (Penn Central factors, categorical per se takings) in response to pressure that the bright line is too narrow. Independent economic analysis documents that uncompensated regulatory takings impose substantial costs on affected property owners, raising questions about whether the problem is actually solved or merely defined away.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__physical_appropriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__physical_appropriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint systematically transfers costs from public goods to property owners without compensation obligation, and the boundary is actively defended against doctrinal expansion. Suppression is moderate (0.45) because property owners can challenge regulations on other constitutional grounds (substantive due process, equal protection, dormant Commerce Clause) and can litigate the taking/regulation boundary itself, but the physical-appropriation reading makes takings claims difficult. Theater is low (0.22): the doctrine's bright-line rule is functionally clear and not primarily performative, though debate about whether the line is drawn correctly has grown with environmental regulation and judicial exploration of categorical and regulatory takings doctrines. Accessibility collapse is high (0.72) because once the physical-appropriation boundary is established, property owners regulated under its regime understand they cannot claim takings protection unless physically appropriated—alternatives (state compensation statutes, purchase of development rights, voluntary conservation easements) exist but collapse relative to a takings claim. Resistance is moderate-high (0.58) because property owners, state governments concerned about federal takings liability, and property-rights advocates continuously push back against the narrow boundary; courts have partially responded by developing categorical and regulatory takings doctrine, which partly accommodates the pressure while the physical-appropriation reading persists as a floor. The measurement series shows steady modest increase in extractiveness and theater as environmental regulation expands and property owners face larger economic losses; suppression requirement also rises as the constraint must defend its boundary against regulatory-takings and categorical-takings challenges.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory-agency and legislative-body seats should compute as beneficiary-proximate (low d), with the constraint appearing as enabling coordination and protecting public goods. The property-owner seats should compute as target-proximate (high d), with the constraint appearing as uncompensated extraction. Courts sit near symmetric: they benefit from the bright-line rule's simplicity but bear the burden of administering it even as it produces harsh outcomes and faces doctrinal pressure. The engine's per-seat computation should surface this asymmetry: what looks like coordination from the agenda-setter view looks like trapped extraction from the property-owner view. The narrow victim set and the broad regulatory beneficiary set create the leverage: many benefit diffusely, few lose acutely, and the acute losses are absorbed as background risk.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners physically dispossessed have maximum d (close to 1.0): the constraint extracts their property entirely, they have no exit, and the compensation obligation is the only remedy. Property owners regulated have high d (0.70–0.85): they face substantial diminution of property value, constrained exit, but some residual options (sell at loss, challenge on other grounds, shift land use). Regulatory agencies have low d (0.15–0.25): they collect the benefit of uncompensated regulatory latitude and have high arbitrage (they can frame rules as regulation rather than appropriation, shift boundaries, etc.). Legislatures and public beneficiaries have low d (0.10–0.20): they collect public goods at property-owner expense. Courts have near-symmetric d (0.45–0.55): they gain from the bright-line clarity but bear administrative burden and legitimacy cost. Environmental advocates have low d (0.15): they benefit from low-cost regulation. Property-rights advocates have high d (0.65): they are excluded from effective voice and their policy position is structurally disadvantaged by the boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mandatrophy tension: the founding problem was genuine (uncertain boundary between taking and regulation), but the narrow solution may have outlived its function. Environmental regulation, historic preservation, and wetlands protection have grown substantially; courts have developed categorical and regulatory takings doctrines in response to perceived harshness of the physical-appropriation rule. The constraint persists, but its mandate is increasingly contested. A mandatrophy reading would argue that the physical-appropriation boundary was justified when regulation was limited but has become a tool for uncompensated exaction as regulation has expanded. However, defenders argue the bright-line rule remains functional (it is judicially clear and prevents constant takings litigation over minor regulations). The classification as tangled_rope rather than snare reflects this: the constraint has a genuine coordination function (clarity, administrability), but it achieves this by extracting costs from regulated property owners. The extraction is sustained by active enforcement (courts apply the bright line, refuse regulatory-takings claims unless they fit categorical or Penn Central frameworks). The mandatrophy ambiguity is captured in the omegas: Is this a solution to a live problem of doctrinal clarity, or has it become an obsolete boundary that protects public-goods regulation at the expense of fairness to property owners?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_justification_outdated,
    'Is the physical-appropriation boundary still a justified solution to a live problem of doctrinal uncertainty, or has it become an obsolete line that protects regulatory expansion at the expense of fairness to property owners?',
    'Comparative analysis of compensation-claim frequency and adjudication cost under this reading versus regulatory-takings doctrine; empirical study of property-value loss borne by regulated owners without compensation; legislative reconsideration of just-compensation frameworks.',
    'If the boundary is obsolete, the constraint reclassifies from coordination-with-extraction to pure extraction (snare), and constitutional pressure for legislative or doctrinal reform intensifies. If the boundary remains justified, the extraction is a necessary cost of clear regulatory authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_justification_outdated, empirical, 'Whether the problem the physical-appropriation doctrine solves is still live or has been superseded by regulation growth.').

omega_variable(
    per_se_vs_balancing_asymmetry,
    'Why should direct physical appropriation trigger automatic just-compensation (a per se rule) while economic destruction through regulation is evaluated by multifactor balancing (Penn Central) that often finds no takings liability?',
    'Doctrinal analysis of the logical consistency of per se versus balancing approaches; examination of whether the asymmetry reflects principled distinction or path-dependent artifact of litigation history.',
    'If the asymmetry is principled, it justifies the narrow takings doctrine. If it is artifact, categorical or regulatory readings become more compelling, and the constraint''s doctrinal stability weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(per_se_vs_balancing_asymmetry, conceptual, 'Doctrinal coherence: the structural asymmetry between per se physical taking and balanced regulatory evaluation.').

omega_variable(
    uncompensated_regulatory_cost_distribution,
    'Is the distribution of uncompensated regulatory costs across property owners (concentrated losses for some, diffuse benefits for public) justified by the coordination benefit of the bright-line rule, or does it represent unjust exaction from a particularized class?',
    'Empirical analysis of who bears regulatory costs versus who receives regulatory benefits; political-economy analysis of whether compensation mechanisms (conservation easements, purchase of development rights, voluntary agreements) are available as alternatives; cross-jurisdictional study of compensation frameworks.',
    'If uncompensated costs are justified by necessity, the constraint remains tangled_rope (coordination with necessary extraction). If they represent unjust particularized exaction, the constraint is snare, and legislative reform to broaden compensation eligibility becomes normative pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncompensated_regulatory_cost_distribution, empirical, 'Whether uncompensated regulatory costs borne by property owners are justified by coordination necessity or constitute unjust exaction.').

omega_variable(
    doctrinal_boundary_contestation,
    'Given that courts have developed categorical takings (permanent occupations are per se takings) and regulatory takings (Penn Central factors) doctrines alongside the physical-appropriation rule, which reading accurately describes the operative constraint on government?',
    'Doctrinal mapping of takings jurisprudence over time; analysis of how courts apply physical-appropriation, categorical, and regulatory frameworks; observation of which framework dominates new litigation and appellate decisions.',
    'If categorical or regulatory readings better describe operative doctrine, this story''s classification may be descriptively inaccurate even if structurally coherent for the physical-appropriation reading. The engine''s classification might diverge from the narrative''s ground truth about how courts actually adjudicate takings claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_boundary_contestation, empirical, 'Whether the physical-appropriation reading describes the operative takings doctrine or whether categorical and regulatory readings have displaced it in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(taki_tr_t0, observed).
narrative_ontology:measurement(taki_tr_t8, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement_basis(taki_tr_t8, observed).
narrative_ontology:measurement(taki_tr_t16, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement_basis(taki_tr_t16, observed).
narrative_ontology:measurement(taki_tr_t24, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement_basis(taki_tr_t24, observed).
narrative_ontology:measurement(taki_tr_t32, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement_basis(taki_tr_t32, observed).
narrative_ontology:measurement(taki_tr_t40, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(taki_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(taki_be_t0, observed).
narrative_ontology:measurement(taki_be_t8, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement_basis(taki_be_t8, observed).
narrative_ontology:measurement(taki_be_t16, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(taki_be_t16, observed).
narrative_ontology:measurement(taki_be_t24, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(taki_be_t24, observed).
narrative_ontology:measurement(taki_be_t32, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(taki_be_t32, observed).
narrative_ontology:measurement(taki_be_t40, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(taki_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(taki_su_t0, observed).
narrative_ontology:measurement(taki_su_t8, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement_basis(taki_su_t8, observed).
narrative_ontology:measurement(taki_su_t16, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement_basis(taki_su_t16, observed).
narrative_ontology:measurement(taki_su_t24, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement_basis(taki_su_t24, observed).
narrative_ontology:measurement(taki_su_t32, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 32, 0.44).
narrative_ontology:measurement_basis(taki_su_t32, observed).
narrative_ontology:measurement(taki_su_t40, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement_basis(taki_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__physical_appropriation_reading, 0.12).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__categorical_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, property_regulatory_power).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, environmental_protection_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the takings_clause_boundary kernel. The categorical_takings_reading and regulatory_takings_reading are structurally distinct constraints derived from alternative interpretations of the Fifth Amendment Takings Clause. All three readings coexist as live jurisprudential positions; the physical-appropriation reading is the narrowest in scope (fewest compensable takings), regulatory is the broadest (most compensable takings), and categorical sits between (per se rules for permanent occupations and total value elimination, balancing for others). The three constraints form a family linked by shared doctrinal kernel; they should be analyzed together to understand how the takings doctrine distributes compensation obligation across government actors and property owners.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
