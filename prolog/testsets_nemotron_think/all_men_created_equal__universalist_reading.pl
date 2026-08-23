% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Universalist Reading: Equality as Iterative Expansion Principle
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   The universalist reading of 'all men are created equal' treats the phrase
 *   as a generative principle whose logical scope exceeds its authors' intent
 *   and whose application must expand iteratively as new exclusions are
 *   recognized. This reading became doctrinally dominant through the
 *   Reconstruction Amendments, the incorporation doctrine, and the
 *   Warren/Burger Court rights expansions. It operates as a constraint on
 *   legislative and executive power: any classification that denies equal
 *   standing to a group must survive heightened scrutiny, and the universe of
 *   protected classifications expands over time (race, sex, legitimacy,
 *   alienage, sexual orientation, gender identity, potentially disability and
 *   wealth). The constraint extracts coordination costs from institutions
 *   required to implement each expansion (desegregation, voting rights
 *   enforcement, ADA compliance, marriage equality, trans rights) while
 *   benefiting marginalized groups who gain standing. The coordination
 *   function is genuine — the principle provides a stable, legitimate pathway
 *   for inclusion claims that would otherwise require revolutionary rupture.
 *   The extraction is asymmetric — institutional actors bear compliance costs
 *   while marginalized groups gain rights; status quo beneficiaries lose
 *   relative position. Active enforcement is required (judicial review,
 *   congressional enforcement legislation, executive implementation) because
 *   resistant jurisdictions and institutions do not voluntarily expand. The
 *   theater ratio is moderate-low: the equality principle performs real
 *   coordination work, but a growing share of doctrinal energy manages the
 *   expansion process itself (standing doctrine, remedial frameworks,
 *   scrutiny tiers) rather than directly vindicating equality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.42).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.38).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Universalist Reading: Equality as Iterative Expansion Principle").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, '9bf9eb98-0adc-44ae-aa97-1489b82e43ee').
narrative_ontology:cs_kernel_codification('9bf9eb98-0adc-44ae-aa97-1489b82e43ee', fixed_text).
narrative_ontology:cs_authority_grounding('9bf9eb98-0adc-44ae-aa97-1489b82e43ee', lineage).
narrative_ontology:cs_interpretation_layer_present('9bf9eb98-0adc-44ae-aa97-1489b82e43ee').
narrative_ontology:cs_reading_relation('9bf9eb98-0adc-44ae-aa97-1489b82e43ee', all_men_created_equal__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('9bf9eb98-0adc-44ae-aa97-1489b82e43ee', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('9bf9eb98-0adc-44ae-aa97-1489b82e43ee', foundational, equality_scope_unbounded_by_founder_intent).
narrative_ontology:cs_axiom_status(equality_scope_unbounded_by_founder_intent, holdable).
narrative_ontology:cs_axiom_grounding('9bf9eb98-0adc-44ae-aa97-1489b82e43ee', equality_scope_unbounded_by_founder_intent, deontological).
narrative_ontology:cs_axiom('9bf9eb98-0adc-44ae-aa97-1489b82e43ee', foundational, iterative_expansion_as_constitutional_duty).
narrative_ontology:cs_axiom_status(iterative_expansion_as_constitutional_duty, holdable).
narrative_ontology:cs_axiom_grounding('9bf9eb98-0adc-44ae-aa97-1489b82e43ee', iterative_expansion_as_constitutional_duty, instrumental).
narrative_ontology:cs_reference_frame('9bf9eb98-0adc-44ae-aa97-1489b82e43ee', founding_declaration_universal_promise).
narrative_ontology:cs_drift_state('9bf9eb98-0adc-44ae-aa97-1489b82e43ee', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9bf9eb98-0adc-44ae-aa97-1489b82e43ee', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, expansion_advocacy_networks).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, rights_litigation_infrastructure).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, institutional_actors_bearing_expansion_costs).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, status_quo_beneficiaries_losing_relative_position).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, jurisdictions_resisting_expansion_mandates).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, equality_as_universal_principle).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, iterative_expansion_doctorine).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, living_constitutionalism_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups historically excluded from 'all men' (enslaved people, women, indigenous nations, LGBTQ+ communities, disabled persons, immigrants) who invoke the universalist principle to claim equal standing. Their exit is constrained by the very exclusion they challenge — they cannot exit the polity that denies them equality, and alternative polities offer no guaranteed improvement. They gain standing, rights, and resource access through each expansion cycle, but each cycle requires renewed mobilization.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion, beneficiary,
    moderate, generational, constrained, national).

% Civil rights organizations, legal defense funds, academic centers, and movement infrastructure that professionalize the work of iterative expansion. They benefit from the principle's vitality — it funds their operations, legitimates their mission, and provides litigation pathways. They can shift issue focus (mobile exit) but their organizational identity is fused to the expansion project (identity_locked dynamics at the organizational level).
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, expansion_advocacy_networks, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, expansion_advocacy_networks, agenda_setter).

% Courts, judicial clerks, constitutional law clinics, and precedent databases that process expansion claims. They benefit from a steady stream of justiciable controversies, institutional legitimacy as rights-vindicators, and professional specialization. They have arbitrage-grade exit — judges rotate, clerks cycle, clinics can refocus — but the infrastructure as a whole is structurally committed to the expansion docket.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, rights_litigation_infrastructure, beneficiary,
    institutional, generational, arbitrage, national).

% Legislatures, agencies, school districts, police departments, and prison systems required to implement each expansion (desegregation orders, voting rights enforcement, ADA compliance, marriage equality implementation, gender identity protections). They bear compliance costs, administrative burden, and political backlash. Exit is constrained — they cannot opt out of constitutional mandates, and resistance triggers judicial enforcement. Their power is institutional but their horizon is biographical (election cycles, appointment terms).
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, institutional_actors_bearing_expansion_costs, payer,
    institutional, biographical, constrained, national).

% Dominant demographic groups, entrenched economic interests, and political coalitions that benefit from restricted equality (e.g., pre-Civil War slaveholders, pre-1920 male-only electorate, pre-Obergefell marriage traditionalists). They lose relative status, privilege, and control with each expansion. They have mobile exit — they can emigrate, secede (historically attempted), or shift to private alternatives — but the cost of exit is high and the constraint's national scope limits effective alternatives. Their power is substantial but their time horizon is biographical (they fight to preserve position within their lifetimes).
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, status_quo_beneficiaries_losing_relative_position, payer,
    powerful, biographical, mobile, national).

% States and localities that resist federal expansion mandates (Reconstruction-era Southern states, massive resistance to Brown v. Board, contemporary sanctuary jurisdiction conflicts). They bear fiscal penalties, federal oversight, and legitimacy costs. They are trapped — they cannot exit the federal union, and their resistance generates escalating enforcement. They are also excluded from the expansion consensus: their constitutional interpretations are treated as illegitimate by the dominant universalist framework.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, jurisdictions_resisting_expansion_mandates, payer,
    organized, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, jurisdictions_resisting_expansion_mandates, excluded).

% Judges, scholars, and advocates who hold that equality's scope is fixed by 1787/1868 public meaning. They are excluded from the authoritative interpretive loop — their readings are treated as dissent, not governing law. Their exit is identity_locked: their professional identity, intellectual project, and institutional affiliations (Federalist Society, originalist journals, certain courts) are constituted by commitment to this reading. They cannot 'switch' without dissolving their epistemic community.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, originalist_interpretive_community, excluded,
    organized, generational, identity_locked, national).

% Scholars who argue the universal language ('all men,' 'equal protection') creates a performative contradiction when applied restrictively — the text itself undermines the restricted reading. They observe the expansion dynamic from outside the advocacy/opposition binary. Their analytical exit is complete; they hold no stake in any expansion outcome. Their civilizational horizon reflects engagement with the principle as a recurring structure in constitutional orders globally.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, textualist_paradox_scholars, observer,
    analytical, civilizational, analytical, universal).

% The Supreme Court, federal judiciary, law school curricula, and bar examination apparatus that collectively administer the universalist reading as governing law. They set the expansion calendar (which groups, which rights, which remedies), police the boundary between legitimate and illegitimate claims, and legitimate the principle's iterative character. They have arbitrage-grade exit at the individual level (judges retire, professors move) but the orthodoxy as an institution is structurally committed to the expansion logic — it cannot abandon the principle without losing its own legitimacy foundation.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, constitutional_interpretive_orthodoxy, agenda_setter,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, legitimating principle for the iterative inclusion of previously excluded groups into equal constitutional standing, converting contested status claims into justiciable rights claims through a recognized doctrinal pathway (equal protection, due process, privileges or immunities).
% TRANSFER_FUNCTION: Moves constitutional recognition, legal protections, and resource access from institutional gatekeepers and status quo beneficiaries to newly recognized equal-rights-holders, mediated by litigation infrastructure and judicial enforcement. The transfer is not zero-sum in material terms (expansion can grow the pie) but is zero-sum in relative status and gatekeeping authority.
% ABSENT_VOICES: Future generations who will bear the coordination costs of expansions not yet conceived; non-citizens subject to U.S. constitutional power (territorial residents, detained migrants, drone-strike targets) who are structurally excluded from the 'we the people' that authorizes the principle; indigenous nations whose sovereignty claims operate on a nation-to-nation framework that the universalist individual-rights model cannot accommodate.
% DISAPPEARANCE_RATIONALE: If the universalist reading vanished overnight, the doctrinal pathway for inclusion claims would collapse. Marginalized groups would lose the constitutional vocabulary to demand equal standing. Courts would revert to originalist/textualist frameworks that freeze equality at founding-era scope. The litigation infrastructure would lose its central organizing principle. The polity would reorganize around a static, bounded equality — likely triggering constitutional crisis or amendment campaigns.
% FOUNDING_PROBLEM: The Declaration's 'all men are created equal' and the Fourteenth Amendment's 'equal protection of the laws' were promulgated in societies that practiced slavery, gender hierarchy, racial caste, and indigenous dispossession. The founding problem: how to make a universal principle operative in a particular society that violently contradicts it, without requiring a new founding each time a new contradiction is recognized.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Declaration's drafters (Jefferson's own draft condemned slavery while the final version deleted it), the Reconstruction Congress (debates over whether the Fourteenth Amendment reached private action, voting rights, and gender), and every subsequent expansion movement (suffragists, civil rights activists, disability rights advocates, LGBTQ+ litigants) who explicitly invoked the gap between principle and practice. No beneficiary group claims the problem is solved; the universalist reading's own logic entails it is never finally solved.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).
:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the genuine coordination costs of iterative expansion — courts, agencies, and legislatures must continuously adapt doctrine, remedy, and implementation. This is not rent extraction but the overhead of a living coordination mechanism. Suppression (0.38) is moderate: the constraint does not primarily operate by silencing alternatives (originalist dissent is robust) but by making resistant implementation legally untenable. Theater ratio (0.22) captures the proceduralization of expansion (tiers of scrutiny, standing barriers, remedial limits) that channels but does not eliminate the principle's force. Accessibility collapse (0.45) is moderate: alternatives (originalism, textualism, pluralism) remain intellectually and politically viable, but the universalist framework sets the default terms of debate. Resistance (0.68) is high: each expansion cycle meets organized opposition (massive resistance, anti-ERA, anti-busing, religious liberty exemptions, anti-trans legislation), confirming the constraint's extractive bite on status quo beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   From the marginalized group seat, the constraint is a Rope — a genuine coordination mechanism that solves the collective action problem of demanding inclusion without revolutionary violence. From the institutional implementer seat, it is a Snare — an open-ended mandate that extracts compliance resources without finality. From the originalist seat, it is a Piton — a degraded textual constraint maintained by institutional inertia and performative fidelity to a 'living' principle the text does not support. From the orthodoxy seat, it is a Scaffold — a transitional mechanism meant to culminate in a fully inclusive polity, but with no sunset because the endpoint is asymptotically defined. The engine computes these per-seat types from the structural data; the claimed_type (tangled_rope) reflects the author's judgment that the constraint's dominant structural character coordinates AND extracts through the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized groups and their advocacy infrastructure are structural beneficiaries (d near 0.0-0.2): the constraint subsidizes their claims, provides justiciable pathways, and legitimates their demands. Institutional implementers and resistant jurisdictions are structural targets (d near 0.7-0.9): they bear compliance costs, face enforcement, and lose discretionary authority. Status quo beneficiaries are targets (d near 0.6-0.8) but with mobile exit — they can sometimes evade local implementation. Originalist interpreters are identity-locked excluded (d ~0.5 but with identity_locked exit): they are not targeted for extraction but are structurally excluded from authoritative interpretation. The constitutional orthodoxy (agenda_setter) sits near symmetric (d ~0.5): it administers the expansion and gains legitimacy from it, but also bears the institutional burden of managing endless doctrinal iteration.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (make equality universal) has not atrophied — the founding problem remains live per the six_questions. Each expansion reveals new exclusions (non-citizens, AI persons, future generations, ecological subjects). The constraint avoids mandatrophy because its telos is asymptotic: universal equality is a horizon, not a checkpoint. The coordination function remains genuine because without this principle, each new inclusion claim would require a new constitutional amendment or revolutionary rupture. The extraction is the cost of avoiding that higher-cost alternative. The risk of mandatrophy would arise if expansion became purely performative (theater_ratio > 0.5) or if the victim set expanded to include groups whose inclusion imposes costs without corresponding equality claims (e.g., corporate personhood expansions). Current metrics suggest neither threshold is crossed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expansion_asymptote_uncertainty,
    'Does the iterative expansion have a logical terminus, or does the universalist principle generate new inclusion claims indefinitely?',
    'Track whether expansion cycles are converging (fewer new protected classes, narrowing remedial scope) or diverging (new categories: algorithmic discrimination, neurodiversity, climate migrants, AI personhood). A convergent pattern suggests the coordination function is completing; a divergent pattern suggests the principle has become a self-sustaining expansion engine.',
    'If convergent, the constraint trends toward Rope (coordination completing). If divergent, it trends toward Snare (extraction without coordination payoff) or Piton (inertial expansion). The engine''s mandatrophy detection would trigger on sustained theater_ratio rise with extractiveness rise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expansion_asymptote_uncertainty, conceptual, 'Whether the universalist principle''s expansion logic is self-limiting or self-perpetuating.').

omega_variable(
    coordination_cost_distribution,
    'Are the coordination costs of expansion borne proportionally by those with capacity, or do they concentrate on low-capacity institutions (rural school districts, small municipalities, underfunded agencies)?',
    'Empirical study of compliance cost distribution across institutional types and fiscal capacities for major expansion mandates (Brown, ADA, Obergefell, Bostock).',
    'If costs concentrate on low-capacity institutions, the constraint''s extraction is regressive — it extracts most from those least able to pay, shifting the classification toward Snare. If costs are distributed progressively (federal funding, capacity-building mandates), the coordination function is more fairly sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_distribution, empirical, 'Distributional incidence of the constraint''s coordination overhead.').

omega_variable(
    originalist_exclusion_mechanism,
    'Is the originalist reading''s exclusion from authoritative interpretation a structural necessity (the universalist principle logically forecloses originalism) or a contingent power outcome (the orthodoxy controls appointments and precedent)?',
    'Counterfactual analysis: if originalists controlled a durable Supreme Court majority, would they administer a stable originalist equality constraint, or would they be forced into universalist expansions by the logic of the text and precedent?',
    'If structural necessity, the universalist reading forecloses the originalist reading (reading_relation = forecloses). If contingent power, they coexist_with (different parties hold each reading simultaneously). This determines whether the kernel admits stable pluralism or requires hegemony.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_exclusion_mechanism, conceptual, 'Whether the universalist/originalist opposition is logical or political.').

omega_variable(
    suppression_internalization_in_resistant_jurisdictions,
    'In jurisdictions that resist expansion mandates (e.g., post-Brown massive resistance, contemporary anti-trans legislation), is the suppression structural (federal enforcement, court orders) or internalized (resistance becomes identity, compliance becomes betrayal)?',
    'Longitudinal study of resistance rhetoric and compliance behavior: does resistance persist after enforcement capacity is demonstrated, suggesting internalized identity-fusion with the restricted equality model?',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them. This would increase the constraint''s extraction on resistant jurisdictions and support Snare classification for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_resistant_jurisdictions, empirical, 'Structural vs. internalized suppression in resistant institutional actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 1776, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all_men_created_equal__universalist_reading_tr_t1776, all_men_created_equal__universalist_reading, theater_ratio, 1776, 0.05).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_tr_t1787, all_men_created_equal__universalist_reading, theater_ratio, 1787, 0.12).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_tr_t1865, all_men_created_equal__universalist_reading, theater_ratio, 1865, 0.08).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_tr_t1868, all_men_created_equal__universalist_reading, theater_ratio, 1868, 0.1).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_tr_t1896, all_men_created_equal__universalist_reading, theater_ratio, 1896, 0.35).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_tr_t1920, all_men_created_equal__universalist_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_tr_t1954, all_men_created_equal__universalist_reading, theater_ratio, 1954, 0.18).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_tr_t1964, all_men_created_equal__universalist_reading, theater_ratio, 1964, 0.2).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_tr_t1973, all_men_created_equal__universalist_reading, theater_ratio, 1973, 0.22).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_tr_t1990, all_men_created_equal__universalist_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_tr_t2015, all_men_created_equal__universalist_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_tr_t2024, all_men_created_equal__universalist_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(all_men_created_equal__universalist_reading_be_t1776, all_men_created_equal__universalist_reading, base_extractiveness, 1776, 0.15).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_be_t1787, all_men_created_equal__universalist_reading, base_extractiveness, 1787, 0.22).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_be_t1865, all_men_created_equal__universalist_reading, base_extractiveness, 1865, 0.35).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_be_t1868, all_men_created_equal__universalist_reading, base_extractiveness, 1868, 0.38).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_be_t1896, all_men_created_equal__universalist_reading, base_extractiveness, 1896, 0.32).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_be_t1920, all_men_created_equal__universalist_reading, base_extractiveness, 1920, 0.36).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_be_t1954, all_men_created_equal__universalist_reading, base_extractiveness, 1954, 0.42).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_be_t1964, all_men_created_equal__universalist_reading, base_extractiveness, 1964, 0.45).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_be_t1973, all_men_created_equal__universalist_reading, base_extractiveness, 1973, 0.43).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_be_t1990, all_men_created_equal__universalist_reading, base_extractiveness, 1990, 0.41).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_be_t2015, all_men_created_equal__universalist_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_be_t2024, all_men_created_equal__universalist_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(all_men_created_equal__universalist_reading_su_t1776, all_men_created_equal__universalist_reading, suppression_requirement, 1776, 0.1).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_su_t1787, all_men_created_equal__universalist_reading, suppression_requirement, 1787, 0.25).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_su_t1865, all_men_created_equal__universalist_reading, suppression_requirement, 1865, 0.4).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_su_t1868, all_men_created_equal__universalist_reading, suppression_requirement, 1868, 0.45).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_su_t1896, all_men_created_equal__universalist_reading, suppression_requirement, 1896, 0.55).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_su_t1920, all_men_created_equal__universalist_reading, suppression_requirement, 1920, 0.4).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_su_t1954, all_men_created_equal__universalist_reading, suppression_requirement, 1954, 0.45).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_su_t1964, all_men_created_equal__universalist_reading, suppression_requirement, 1964, 0.5).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_su_t1973, all_men_created_equal__universalist_reading, suppression_requirement, 1973, 0.42).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_su_t1990, all_men_created_equal__universalist_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_su_t2015, all_men_created_equal__universalist_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(all_men_created_equal__universalist_reading_su_t2024, all_men_created_equal__universalist_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__universalist_reading, 0.08).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the all_men_created_equal constraint family. The kernel 'all men are created equal' decomposes into three structurally distinct constraints: (1) originalist_reading — ε ≈ 0.15, Mountain-claim (founder intent as natural law), beneficiaries = originalist interpretive community; (2) textualist_paradox_reading — ε ≈ 0.35, Tangled Rope (text's universal language vs. restricted practice), beneficiaries = textualist scholars, victims = originalist practitioners; (3) universalist_reading (this story) — ε = 0.42, Tangled Rope (iterative expansion coordination with asymmetric compliance costs). The ε values differ because each reading instantiates a different standing arrangement: originalist = frozen founding scope; textualist = performative contradiction as extraction; universalist = expansion mandate as coordination overhead. They are linked by network.affects_constraints because the universalist reading's doctrinal dominance structures the contest the other readings respond to.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__universalist_reading, organized, 0.25).
constraint_indexing:directionality_override(all_men_created_equal__universalist_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
