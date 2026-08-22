% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: Public-Health-Primary Reading of Vaccination-Mandate Legitimacy
 *   domain: public health ethics/constitutional law/medical autonomy
 *
 * SUMMARY:
 *   Compulsory-vaccination authority in the United States rests on the
 *   Jacobson-line police-power tradition: school-entry laws cover most
 *   children, non-medical exemptions vary sharply by state, and kindergarten
 *   measles coverage has slipped below the 95 percent community-immunity
 *   threshold, with importation outbreaks recurring. This story is authored
 *   from the public_health_primary seat, for which the standing arrangement's
 *   defining fact is that protection for those who cannot vaccinate is held
 *   hostage to contested, unevenly enforced compulsion: extraction runs from
 *   the can't-vaccinate to the won't-vaccinate through the gaps. Family
 *   decomposition note (epsilon-invariance): the colloquial label 'vaccine
 *   mandate legitimacy' covers three structurally distinct constraints. This
 *   file authors epsilon approximately 0.76 for the gap-ridden standing
 *   arrangement, with the can't-vaccinate as victims; the
 *   bodily_autonomy_primary sibling authors high epsilon for compulsion
 *   itself, with compelled objectors as victims; the proportionality_reading
 *   sibling authors case-variable epsilon indexed to disease severity,
 *   vaccine safety, and alternative availability. This file's epsilon is
 *   fixed to its own referent and is never averaged across readings. KEY
 *   AGENTS (by structural relationship): - immunocompromised_patients:
 *   Primary target (powerless/trapped) — absorbs residual disease risk when
 *   coverage gaps open - infants_below_vaccination_age: Primary target
 *   (powerless/trapped) — pre-schedule window, wholly dependent on
 *   surrounding coverage - frail_elderly_residents: Secondary target
 *   (powerless/constrained) — congregate exposure with waning response -
 *   vaccinated_general_public: Collector with contribution
 *   (moderate/constrained) — collects herd protection, has paid in -
 *   personal_belief_exemption_holders: Principal collector without
 *   contribution (moderate/mobile) — captures protection through exemption
 *   routes - religious_conscience_objectors: Identity-bound collector
 *   (moderate/identity_locked) — refusal fused with faith membership -
 *   public_health_agencies: Administrator (institutional/constrained) — sets
 *   and enforces entry rules within statutory limits - appellate_courts:
 *   Boundary-setter (institutional/constrained) — fixes the constitutional
 *   scope of compulsion - vaccine_hesitant_parents: Duty-bearing swing bloc
 *   (moderate/mobile) — their choices move coverage across thresholds -
 *   access_limited_rural_families: Absent voice (powerless/trapped) — access
 *   failure misread as refusal - frontline_pediatric_oncology_clinicians:
 *   Bedside witness (organized/constrained) — documents the harm the gaps
 *   produce
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.76).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.54).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.76).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "Public-Health-Primary Reading of Vaccination-Mandate Legitimacy").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public health ethics/constitutional law/medical autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, '2ac942d6-5a4c-4287-a0d4-a84c70263e30').
narrative_ontology:cs_kernel_codification('2ac942d6-5a4c-4287-a0d4-a84c70263e30', formalized).
narrative_ontology:cs_authority_grounding('2ac942d6-5a4c-4287-a0d4-a84c70263e30', lineage).
narrative_ontology:cs_interpretation_layer_present('2ac942d6-5a4c-4287-a0d4-a84c70263e30').
narrative_ontology:cs_reading_relation('2ac942d6-5a4c-4287-a0d4-a84c70263e30', mandate_legitimacy_scope__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('2ac942d6-5a4c-4287-a0d4-a84c70263e30', mandate_legitimacy_scope__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('2ac942d6-5a4c-4287-a0d4-a84c70263e30', foundational, vulnerable_protection_trumps_refusal_liberty).
narrative_ontology:cs_axiom_status(vulnerable_protection_trumps_refusal_liberty, holdable).
narrative_ontology:cs_axiom_grounding('2ac942d6-5a4c-4287-a0d4-a84c70263e30', vulnerable_protection_trumps_refusal_liberty, deontological).
narrative_ontology:cs_axiom('2ac942d6-5a4c-4287-a0d4-a84c70263e30', secondary, necessity_condition_bounds_compulsion).
narrative_ontology:cs_axiom_status(necessity_condition_bounds_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('2ac942d6-5a4c-4287-a0d4-a84c70263e30', necessity_condition_bounds_compulsion, instrumental).
narrative_ontology:cs_reference_frame('2ac942d6-5a4c-4287-a0d4-a84c70263e30', communal_immunity_paramount_duty).
narrative_ontology:cs_drift_state('2ac942d6-5a4c-4287-a0d4-a84c70263e30', post_covid_coverage_retrenchment, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2ac942d6-5a4c-4287-a0d4-a84c70263e30', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, vaccinated_general_public).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, personal_belief_exemption_holders).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, religious_conscience_objectors).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, immunocompromised_patients).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, infants_below_vaccination_age).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, frail_elderly_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, vaccinated_general_public).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_parents).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, police_power_public_welfare_doctrine).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, herd_immunity_threshold_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive chemotherapy, transplants, or biologics that block vaccine response, or cannot receive live vaccines at all. They depend on the vaccination choices of people around them for protection they cannot manufacture in their own bodies. When school or neighborhood coverage slips, they face measles, influenza, and covid at hospitalization rates far above baseline; their only exits are strict isolation or relocating to higher-coverage areas, both medically and financially heavy.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, immunocompromised_patients, payer,
    powerless, biographical, trapped, national).

% Are too young for the first doses of the measles and pertussis schedules, protected in early months only by maternal antibodies and the coverage of everyone around them. Outbreaks seeded by travel or by older unvaccinated children reach them directly. Their parents cannot accelerate the schedule; the practical exit is avoiding public spaces, which isolates the whole family.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, infants_below_vaccination_age, payer,
    powerless, biographical, trapped, national).

% Live in congregate settings with waning vaccine response. They benefit from staff and visitor vaccination rules where those exist and absorb severe outcomes where they do not. Moving facilities or into home care is possible but expensive and disruptive late in life.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, frail_elderly_residents, payer,
    powerless, biographical, constrained, regional).

% Accepted the small personal cost and risk of vaccination and now collect the protection of high coverage alongside everyone else. They carry booster decisions and, indirectly, the tax and insurance costs of outbreak response. Their levers are voting and school-board participation; they cannot opt out of the disease environment.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vaccinated_general_public, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__public_health_primary, vaccinated_general_public, payer).

% Hold state-recognized philosophical or personal-belief exemptions where offered, declining vaccination while continuing to use the schools, workplaces, and travel that depend on others' coverage. When a state tightens its rules, some relocate to neighboring jurisdictions with looser forms or join parent networks that track exemption availability. They take community protection without contributing to it and bear mostly social censure.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, personal_belief_exemption_holders, beneficiary,
    moderate, biographical, mobile, national).

% Refuse vaccination as a matter of faith or conscience, in communities where refusal is part of what membership means. Compulsion reads to them as persecution of religious practice; complying, or filing an exemption under a secular label, would fracture identity and standing in the congregation. Exit means leaving the community, not just the clinic.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, religious_conscience_objectors, beneficiary,
    moderate, generational, identity_locked, national).

% Set school-entry requirements, run outbreak response, and issue isolation orders under delegated police powers. They can recommend tightening but depend on legislatures for statutory change and on courts to sustain orders. Aggressive mandates cost them political authority, so they manage coverage through persuasion wherever possible.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Fix the outer boundary of compulsory-vaccination authority, from Jacobson v. Massachusetts through contemporary cases on religious exemptions and agency power. They administer no programs; they set the terms under which legislatures and agencies may act, and precedent binds them as much as anyone else.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, appellate_courts, agenda_setter,
    institutional, generational, constrained, national).

% Worry about vaccine safety, distrust institutions, and delay or selectively decline doses for their children. They face school-entry checkpoints, counseling requirements, and, in some states, loss of non-medical exemption routes. Homeschooling, private schools with loose verification, or relocation are real but costly exits.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_parents, payer,
    moderate, biographical, mobile, local).

% Want vaccines but face clinic deserts, transportation barriers, uninsured visits, and work schedules that make multi-dose series hard to complete. Policy framed as refusal-versus-compulsion counts them as objectors when they are access failures. Organized pro- and anti-mandate voices dominate the hearings where their situation would be described.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, access_limited_rural_families, excluded,
    powerless, biographical, trapped, regional).

% See the casualties of coverage gaps directly: the transplant recipient with measles, the infant with pertussis on ventilatory support. Professional societies submit testimony and publish mortality data, but they hold no vote on exemption statutes; their influence runs through evidence and the public alarm outbreaks produce.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, frontline_pediatric_oncology_clinicians, observer,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__public_health_primary, personal_belief_exemption_holders).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns individual vaccination choices with the coverage thresholds that community immunity requires, solving the free-rider problem in which each household's incentive to decline a small personal cost erodes a protection that everyone, including refusers, consumes.
% TRANSFER_FUNCTION: Moves disease risk and contribution burdens. In the standing arrangement it channels residual outbreak risk onto those who cannot vaccinate while exemption holders capture community protection without contributing; where the doctrine is enforced, it instead places a small liberty and injury-risk burden on those who can vaccinate but decline, for the protection of those who cannot.
% ABSENT_VOICES: Access-limited rural families are absent from a debate structured as refusal-versus-compulsion, so access failure gets counted as objection. The severely immunocompromised, who hold the largest stakes, appear mostly through clinician proxies because illness itself removes them from hearings. Infants have no voice at all beyond their parents'.
% DISAPPEARANCE_RATIONALE: If the doctrine legitimating compulsion vanished overnight, school-entry coverage would erode toward the exemption-propensity rate, measles and pertussis would resume endemic cycling, and pediatric intensive care, oncology units, and long-term-care facilities would reorganize around permanent isolation regimens for the unprotected. The exposure of the can't-vaccinate seats is maintained by this arrangement's presence or absence.
% FOUNDING_PROBLEM: Nineteenth-century epidemic urban disease: smallpox and diphtheria swept cities, voluntary vaccination plateaued below protective coverage, and boards of health turned to compulsion, culminating in Cambridge, Massachusetts's 1902 order and Jacobson v. Massachusetts (1905), which held that individual liberty yields to real or imminent communal danger.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: pediatric transplant and oncology societies publish vulnerability data; CDC and WHO surveillance attribute recurring outbreaks to sub-threshold coverage; multiple courts have made independent imminent-danger findings during measles emergencies (New York City, 2019, among others). Civil-liberties organizations dispute the policy remedy, not the existence of the vulnerability: the problem's liveness is attested, its resolution contested.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76) because the standing arrangement's costs land on those least able to bear them: the can't-vaccinate absorb outbreak risk proportional to every point of coverage slippage, while the benefit of non-participation accrues to identifiable exemption-holding seats. Suppression is moderate (0.54): real enforcement machinery exists (entry checkpoints, counseling requirements, exclusion rules) but is patchy, exemption-laden, and politically oscillating — the reading's complaint is precisely that refusal remains too available. Accessibility_collapse is low (0.38) for the same reason: homeschooling, relocation, and exemption routes keep alternatives open. Resistance is high (0.68): organized anti-mandate movements, litigation, and legislative preemption actively contest the arrangement. Theater is low-to-moderate (0.36) because the underlying function is demonstrably real — coverage drops produce measurable outbreaks — but a growing share of activity is symbolic (unenforced executive orders, ceremonial exemption hearings).
 *   
 *   The temporal series run on one shared nine-point grid and show a ratchet-and-decay cycle: an outbreak concentrates attention and produces tightening (suppression peaks 1947-1963 and 2015-2021); coverage recovers; attention fades; exemption routes widen and coverage erodes until the next outbreak re-arms the machinery (troughs 1980-1998 and after 2021). Extractiveness from the vulnerable tracks the decay phases — it accumulates during relaxation and is briefly compressed by tightening. The oscillation is partly the mechanism itself: each outbreak grants a temporary mandate for tightening, and its decay is what lets the gap reopen, so the cycle functions as intermittent reinforcement of a contested equilibrium. Base properties were measured at the 2026 endpoint: late in a decay phase, with coverage below threshold and enforcement politically retreating.
 *   
 *   Receipt surface: gains demonstrably accrue to personal_belief_exemption_holders, who take community protection at zero contribution and hold the most mobile exit; fixing_cost is prohibitive because the agenda-setting seats that could close the exemption routes bear concentrated backlash costs (primaries, litigation, protest) while the benefit of fixing accrues diffusely to the vulnerable — which is why known gaps persist.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the immunocompromised patient's position the arrangement is lethal negligence: others' discretionary choice sets their mortality risk, with no exit available. From the exemption holder's position it is pluralism: a modest liberty exercise whose risks are hypothetical and distributed. From the appellate court's position it is settled doctrine: Jacobson authorizes what legislatures choose to enact. From the agency's position it is an insufficient toolkit: authority without political cover. Same structure, divergent per-seat classifications — the engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Victim declarations drive high directionality: immunocompromised patients and infants sit nearest the full-target end (trapped exit amplifies d — they cannot vaccinate into protection or leave the pathogen environment); frail elderly sit slightly lower (constrained but not immobile). Beneficiary declarations drive low directionality: personal_belief_exemption_holders sit nearest the beneficiary end (pure collection, mobile exit, arbitrage across jurisdictional lines); religious_conscience_objectors sit low-to-mid (currently collecting, but identity_locked exit means compulsion would push them hard toward the target end — the seat to watch if exemptions close); vaccinated_general_public sits near symmetric (genuine collection, but they contributed). Agenda-setters derive near-symmetric institutional positions: agencies and courts administer and bound the arrangement without collecting its gains. Scope is national for most seats, which modestly amplifies effective extraction through verification difficulty — coverage is audited school district by school district, and gaps hide in the aggregate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, not atrophied: sub-threshold coverage produces documented outbreaks and documented deaths among the can't-vaccinate, corroborated by clinical societies and surveillance systems outside the benefiting parties. mandatrophy_resolved is therefore false and no sunset applies. The classification prevents mislabeling in both directions: calling the arrangement a snare would erase the corroborated coordination function (community immunity is real, measurable, and consumed by everyone including refusers); calling it a rope would erase the asymmetric burdens (a compelled minority where the doctrine bites, and an exposed vulnerable class where it does not). Tangled_rope holds both faces. The necessity-threshold omega tracks where coordination ends and extraction begins: the doctrine's own text bounds compulsion by necessity, so the extraction the reading condemns lives in the gap between the doctrine's scope and its enforcement, not in the doctrine's core.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the public_health_primary reading of the mandate_legitimacy_scope kernel; how would the classification shift under the sibling readings?',
    'Generate the sibling stories (mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__proportionality_reading) and compare computed per-seat classifications over the same factual record.',
    'Under bodily_autonomy_primary the victim set inverts to compelled objectors and exemption holders become rights-holders rather than collectors; under proportionality_reading victims become case-dependent and epsilon varies with disease severity, vaccine safety, and alternative availability. This file''s epsilon is fixed to its own referent and must not be averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of a contested kernel; committer structure carried here rather than in the constraint body.').

omega_variable(
    necessity_threshold_operationalization,
    'At what coverage level and outbreak probability does compulsion become ''necessary'' to protect the vulnerable, as opposed to merely prudent?',
    'Disease-specific community-immunity thresholds combined with outbreak-frequency modeling at observed coverage levels, cross-checked against judicial imminent-danger findings across jurisdictions.',
    'A demanding necessity reading shrinks the doctrine''s legitimate domain toward active-outbreak conditions; a precautionary reading extends it to threshold maintenance. The boundary decides how much of the current gap-driven extraction the doctrine condemns versus permits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_threshold_operationalization, empirical, 'Operational content of the necessity condition bounding legitimate compulsion.').

omega_variable(
    duty_enforceability_dispute,
    'Do the unvaccinated bear an enforceable duty to protect those who cannot vaccinate, or only face permissible incentives and exhortation?',
    'Legal-philosophical analysis of harm-principle applications to communicable disease; comparison of jurisdictions that impose exposure liability versus those limited to condition-setting.',
    'If no enforceable duty exists, this reading''s transfer function loses its moral foundation and the doctrine drifts toward the proportionality sibling; if it exists, compulsion is duty-collection rather than liberty-taking, and the extraction attributed to exemption holders sharpens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duty_enforceability_dispute, preference, 'Whether the duty-to-protect premise grounds enforceable obligation or mere persuasion.').

omega_variable(
    coverage_gap_causal_attribution,
    'Are coverage gaps driven primarily by refusal (duty-shirking) or by access failure (clinic deserts, cost, logistics)?',
    'County-level decomposition of under-vaccination into filed exemptions versus missed-dose and access indicators.',
    'If access failure dominates, mandate-tightening misclassifies access-limited families as violators and the extraction attribution shifts from exemption holders toward systemic neglect; targeted access remedies would substitute for compulsion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coverage_gap_causal_attribution, empirical, 'Causal composition of the coverage gaps that generate the measured extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 1900, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t1900, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1900, 0.1).
narrative_ontology:measurement_basis(mand_tr_t1900, observed).
narrative_ontology:measurement(mand_tr_t1922, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1922, 0.12).
narrative_ontology:measurement_basis(mand_tr_t1922, observed).
narrative_ontology:measurement(mand_tr_t1947, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1947, 0.15).
narrative_ontology:measurement_basis(mand_tr_t1947, observed).
narrative_ontology:measurement(mand_tr_t1963, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1963, 0.14).
narrative_ontology:measurement_basis(mand_tr_t1963, observed).
narrative_ontology:measurement(mand_tr_t1980, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1980, 0.2).
narrative_ontology:measurement_basis(mand_tr_t1980, observed).
narrative_ontology:measurement(mand_tr_t1998, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1998, 0.24).
narrative_ontology:measurement_basis(mand_tr_t1998, observed).
narrative_ontology:measurement(mand_tr_t2015, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2015, 0.28).
narrative_ontology:measurement_basis(mand_tr_t2015, observed).
narrative_ontology:measurement(mand_tr_t2021, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2021, 0.33).
narrative_ontology:measurement_basis(mand_tr_t2021, observed).
narrative_ontology:measurement(mand_tr_t2026, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2026, 0.36).
narrative_ontology:measurement_basis(mand_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(mand_be_t1900, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1900, 0.85).
narrative_ontology:measurement_basis(mand_be_t1900, observed).
narrative_ontology:measurement(mand_be_t1922, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1922, 0.7).
narrative_ontology:measurement_basis(mand_be_t1922, observed).
narrative_ontology:measurement(mand_be_t1947, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1947, 0.55).
narrative_ontology:measurement_basis(mand_be_t1947, observed).
narrative_ontology:measurement(mand_be_t1963, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1963, 0.45).
narrative_ontology:measurement_basis(mand_be_t1963, observed).
narrative_ontology:measurement(mand_be_t1980, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement_basis(mand_be_t1980, observed).
narrative_ontology:measurement(mand_be_t1998, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1998, 0.58).
narrative_ontology:measurement_basis(mand_be_t1998, observed).
narrative_ontology:measurement(mand_be_t2015, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement_basis(mand_be_t2015, observed).
narrative_ontology:measurement(mand_be_t2021, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2021, 0.72).
narrative_ontology:measurement_basis(mand_be_t2021, observed).
narrative_ontology:measurement(mand_be_t2026, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2026, 0.76).
narrative_ontology:measurement_basis(mand_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t1900, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement_basis(mand_su_t1900, observed).
narrative_ontology:measurement(mand_su_t1922, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1922, 0.45).
narrative_ontology:measurement_basis(mand_su_t1922, observed).
narrative_ontology:measurement(mand_su_t1947, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1947, 0.6).
narrative_ontology:measurement_basis(mand_su_t1947, observed).
narrative_ontology:measurement(mand_su_t1963, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1963, 0.65).
narrative_ontology:measurement_basis(mand_su_t1963, observed).
narrative_ontology:measurement(mand_su_t1980, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement_basis(mand_su_t1980, observed).
narrative_ontology:measurement(mand_su_t1998, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1998, 0.48).
narrative_ontology:measurement_basis(mand_su_t1998, observed).
narrative_ontology:measurement(mand_su_t2015, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2015, 0.56).
narrative_ontology:measurement_basis(mand_su_t2015, observed).
narrative_ontology:measurement(mand_su_t2021, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2021, 0.62).
narrative_ontology:measurement_basis(mand_su_t2021, observed).
narrative_ontology:measurement(mand_su_t2026, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2026, 0.54).
narrative_ontology:measurement_basis(mand_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'vaccine mandate legitimacy' decomposes per the epsilon-invariance principle into three linked stories. This file (public_health_primary) authors epsilon approximately 0.76 for the standing gap-ridden arrangement, in which extraction runs from those who cannot vaccinate to those who decline. The bodily_autonomy_primary sibling authors high epsilon for compulsion itself, with compelled objectors as victims. The proportionality_reading sibling authors case-variable epsilon indexed to disease severity, vaccine safety, and alternative availability. Upstream/downstream: the Jacobson-line doctrine this reading instantiates supplies the legal substrate both siblings litigate against, so this reading structurally influences its siblings' operating environment even where it does not foreclose them. Each file links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
