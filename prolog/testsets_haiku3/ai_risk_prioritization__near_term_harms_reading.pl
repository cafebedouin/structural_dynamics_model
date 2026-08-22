% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__near_term_harms_reading, []).

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
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: AI Near-Term Harms Risk Prioritization (Justice Reading)
 *   domain: technology/governance/social_justice
 *
 * SUMMARY:
 *   Under the near-term harms reading of AI risk, the constraint is the
 *   prioritization framework itself: a institutional and normative commitment
 *   to treating deployed system harms to present marginalized populations as
 *   the primary AI risk, demanding immediate justice interventions through
 *   bias audits, worker protections, and surveillance regulation. The
 *   constraint extracts from and suppresses the competing existential-risk
 *   reading by framing it as speculative distraction from urgent present
 *   injustices. This is ONE reading of the contested kernel 'AI risk
 *   prioritization'—the sibling reading (existential_risk_reading)
 *   instantiates a different constraint with a different victim set,
 *   timescale, and resource allocation. Both readings are live; neither
 *   logically forecloses the other within any single framework (different
 *   parties hold each), so the relation is coexists_with. The claim/metric
 *   gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   (coordination of justice interventions + asymmetric extraction of
 *   resource/attention from x-risk), and the metrics describe that structure
 *   honestly.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.72).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "AI Near-Term Harms Risk Prioritization (Justice Reading)").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "technology/governance/social_justice").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, 'de29bcf1-348a-4a5f-8e33-c761684bedf5').
narrative_ontology:cs_kernel_codification('de29bcf1-348a-4a5f-8e33-c761684bedf5', distributed).
narrative_ontology:cs_authority_grounding('de29bcf1-348a-4a5f-8e33-c761684bedf5', distributed).
narrative_ontology:cs_reading_relation('de29bcf1-348a-4a5f-8e33-c761684bedf5', ai_risk_prioritization__existential_risk_reading, coexists_with).
narrative_ontology:cs_axiom('de29bcf1-348a-4a5f-8e33-c761684bedf5', foundational, algorithmic_discrimination_is_present_harm).
narrative_ontology:cs_axiom_status(algorithmic_discrimination_is_present_harm, holdable).
narrative_ontology:cs_axiom_grounding('de29bcf1-348a-4a5f-8e33-c761684bedf5', algorithmic_discrimination_is_present_harm, empirically_contingent).
narrative_ontology:cs_axiom('de29bcf1-348a-4a5f-8e33-c761684bedf5', foundational, present_justice_is_prior_to_future_speculation).
narrative_ontology:cs_axiom_status(present_justice_is_prior_to_future_speculation, holdable).
narrative_ontology:cs_axiom_grounding('de29bcf1-348a-4a5f-8e33-c761684bedf5', present_justice_is_prior_to_future_speculation, deontological).
narrative_ontology:cs_reference_frame('de29bcf1-348a-4a5f-8e33-c761684bedf5', algorithmic_accountability_framework).
narrative_ontology:cs_drift_state('de29bcf1-348a-4a5f-8e33-c761684bedf5', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('de29bcf1-348a-4a5f-8e33-c761684bedf5', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, racialized_populations).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, low_wage_workers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, surveillance_targeted_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, marginalized_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face discrimination through deployed AI systems now: criminal risk assessment that overclassifies them, hiring algorithms that exclude them, facial recognition that misidentifies them at higher rates, credit scoring that denies them access. They benefit from justice interventions—bias audits, worker protections, surveillance regulation—that reduce immediate harms. They also bear the diffuse costs of enforcement: slower deployment of services, higher compliance costs passed to users. Their exit options are constrained: they cannot opt out of systems that govern access to housing, employment, credit, and freedom.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_communities, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, marginalized_communities, payer).

% Receive research funding, institutional authority, and publication venues when near-term harms are centered as the primary AI risk domain. Their career path depends on the measurable, audit-able, regulatory-addressable framing of bias and surveillance as the core problems. They vindicate this reading through empirical studies of algorithmic discrimination. Their mobility is high: they can shift research agendas if funding priorities change, though their sunk expertise in fairness creates path dependence.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, beneficiary,
    institutional, generational, mobile, global).

% Subjected to algorithmic discrimination in real time: arrested at higher rates when flagged by predictive policing, denied loans and housing through biased credit scoring, misidentified by facial recognition deployed without consent or recourse. They depend on justice interventions—regulations mandating bias audits, restrictions on high-risk applications—to reduce discrimination. They cannot exit the systems; they can only advocate for regulation. Their time horizon is biographical because the harms accumulate over a lifetime and regulatory protection is a necessity for survival.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, racialized_populations, payer,
    powerless, biographical, trapped, national).

% Experience labor displacement and surveillance through deployed AI: algorithmic scheduling that fragments their work, automation that displaces them from jobs, algorithmic management that monitors their productivity and attendance. Justice interventions—worker protections, retraining programs, limits on algorithmic surveillance—aim to protect them. Their exit options are limited; they need work and cannot easily move to sectors untouched by automation. The constraint requires enforcement (labor regulation, retraining funding) to function.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, low_wage_workers, payer,
    powerless, biographical, constrained, national).

% Face heightened surveillance through AI-enabled systems: facial recognition, pattern-of-life analysis, social media monitoring. Communities already disproportionately surveilled—immigrants, activists, religious minorities, people with criminal histories—face compounded harms. They cannot exit their visible identities; the only recourse is regulation mandating restrictions on surveillance use. Their identity-lock is structural: they are trapped not by choice but by how they are classified and made visible to discriminatory systems.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, surveillance_targeted_groups, payer,
    powerless, biographical, identity_locked, national).

% Deploy AI systems that produce measurable harms: hiring algorithms, credit scoring, criminal risk assessment, surveillance systems. They claim the systems improve efficiency and reduce human bias (a coordination narrative). Under the near-term harms reading, they face regulatory requirements: bias audits, transparency obligations, restrictions on high-risk applications, worker protections. They can shift deployment strategy, relocate to friendlier regulatory environments, invest in bias-mitigation research, or modify systems to pass audits—their exit options are relatively broad, but the regulatory apparatus constrains their choices in ways that benefit the reading.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_systems_deployers, agenda_setter,
    institutional, biographical, mobile, global).

% Frame AI risk as primarily existential—misaligned AGI poses extinction-level threat. Under the near-term harms reading, their framing is characterized as diverting resources and attention from urgent present injustices. They are not seated at tables where justice interventions are designed; their research priorities and funding are treated as competing, not complementary. They have mobility (can publish in their own venues, attract some funding) but face institutional suppression from the fairness community (rhetorical delegitimization, limited co-venue space, resource competition). Their exclusion is the mechanism through which the constraint maintains extraction.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, x_risk_researchers, excluded,
    institutional, civilizational, mobile, global).

% Decide whether to center near-term harms or existential risk in AI regulation. They receive testimony from marginalized communities, fairness researchers, deployers, and x-risk researchers. Under the near-term harms reading, they are pressured to mandate bias audits, worker protections, and surveillance restrictions now. They are an analytical seat: they observe the constraint's operation and can choose to reinforce or dismantle it through regulation.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, legislative_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__near_term_harms_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a accountability and justice framework for AI harms: bias audits identify discriminatory systems; worker protections mitigate displacement; surveillance regulation reduces present monitoring harms. Coordinates efforts across researchers, regulators, and affected communities to measure, audit, and restrict high-risk AI applications that harm marginalized populations now. The genuine coordination problem: who ensures deployed systems don't discriminate, and how do we align incentives between system deployers and affected communities?
% TRANSFER_FUNCTION: Moves regulatory compliance burden and implementation cost to AI deployers; moves research funding, institutional authority, and publication venues to fairness and accountability researchers; moves protective measures (audit requirements, restrictions, transparency obligations) to marginalized communities in the form of harm reduction (though imperfectly delivered); diverts research attention and funding from existential-risk research toward near-term justice work.
% ABSENT_VOICES: X-risk researchers and long-term AI safety advocates are excluded from near-term justice intervention design tables; their framing of existential risk is treated as a distraction from present suffering rather than a legitimate alternative prioritization. Communities experiencing AI harms in non-English-speaking or non-Western contexts are underrepresented in governance—the reading's empirical base is skewed toward North American and European deployers and harms. Low-wage workers and surveilled populations have limited voice in how their 'protection' is designed; fairness researchers and policymakers often design solutions without deep participation from affected communities.
% DISAPPEARANCE_RATIONALE: If near-term harm prioritization vanished overnight, regulatory pressure for bias audits and worker protections would collapse within months; deployers would optimize systems purely for efficiency and commercial benefit without mandatory fairness constraints; racialized and low-wage populations would face unmitigated algorithmic discrimination; fairness research funding would dry up and researchers would shift to other domains; institutions built to enforce bias audits would be repurposed or disbanded. Research attention would redistribute toward existential-risk domains. The constraint is not natural law or inevitable coordination—it exists because advocacy communities have built institutional and regulatory power around it.
% FOUNDING_PROBLEM: Deployed AI systems cause measurable discrimination, labor displacement, and surveillance harms to marginalized populations starting now—not in hypothetical future scenarios but in current deployments: criminal risk assessment that overclassifies racialized defendants, hiring algorithms that exclude protected groups, facial recognition that misidentifies with racialized error gradients, algorithmic scheduling that fragments low-wage work. These present harms demand immediate intervention through audits, worker protections, and restrictions on high-risk applications.
% FOUNDING_PROBLEM_CORROBORATION: Marginalized communities themselves attest the founding problem is live and worsening through direct experience. Civil rights organizations (ACLU, Center for Democracy & Technology, AI Now Institute), worker advocacy groups, and law enforcement accountability advocates provide corroboration outside the fairness research establishment. Documented cases: COMPAS bias in Wisconsin criminal sentencing; Amazon hiring algorithm that downranked women; facial recognition misidentification of Black citizens; algorithmic pricing discrimination in auto insurance. These witnesses—affected communities, civil rights advocates, law enforcement accountability researchers, journalist investigations—corroborate that measurable harms are present and systemic, not hypothetical.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__near_term_harms_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is high because the reading diverts resources and research attention away from existential risk research toward near-term justice work—a structural transfer. This is not a claim about which is more important (a value question) but a structural fact: under this reading's institutional implementation, x-risk research receives less funding and authority than it would under the existential reading. Suppression (0.72) is high because the reading actively suppresses alternative framings (x-risk as 'speculative,' 'distraction,' 'privileging hypothetical futures over present suffering') through rhetorical and institutional channels. Theater (0.41) is moderate-low: the bias audits and worker protections are real interventions solving genuine problems, but a growing share of the constraint's persistence depends on rhetorical suppression of existential-risk framing rather than on the quality of the justice interventions themselves (the suppression rises over the interval while theater creeps upward, indicating performative enforcement). The measurement series show extraction rising then plateauing, suppression rising and holding steady, theater creeping upward—a pattern of institutional entrenchment with modest performativity drift.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits strong seat divergence: marginalized communities experience it as protective coordination; x-risk researchers experience it as suppressive extraction; deployers experience it as regulatory burden; fairness researchers experience it as institutional authority and resource allocation. The engine computes four different per-seat types from one structural configuration—that divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities and low-wage workers are the direct targets of the constraint's victim-list extraction (high d toward target end). Fairness researchers are the beneficiaries (low d toward beneficiary end). X-risk researchers sit in an interesting middle: they are not declared victims, but the constraint extracts resources and authority from their domain (medium-high d, captured through the suppression mechanism rather than explicit victimhood). Deployers face regulatory compliance burden (medium d, constrained between coordination cost and avoided-harm benefit). The directionality derivation chain runs: beneficiary/victim declarations + power atom + exit options → d. No overrides are needed; the structural data suffices.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (present measurable harms to marginalized populations from deployed AI) is undoubtedly live. The risk is that the constraint persists as tangled_rope—genuine coordination wrapped around resource extraction—and the extracted resources are captured by fairness researchers and institutions while the actual harms to marginalized communities persist or worsen. This is the mandatrophy risk: the constraint vindicates a true proposition (algorithmic bias exists and causes present harm) but the institutional apparatus built around addressing it becomes a career machine and research-funding stream decoupled from actual harm reduction. Monitoring: if the constraint's theater ratio continues rising while base extractiveness plateaus, and if fairness research funding grows while documented algorithmic discrimination in marginalized communities persists, the mandatrophy signal strengthens. If the constraint actually reduces measurable harms to target populations, it remains genuinely coordinative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is AI risk primarily near-term harms to present marginalized populations, or primarily existential threat from misaligned AGI?',
    'This is not resolvable as an empirical fact—both harms are real and both timescales are live. The resolution is institutional: which reading receives research funding, shapes regulation, and captures authority. The contest is about priority and resource allocation under conditions of irreducible uncertainty about the future.',
    'If the near-term reading dominates, resources flow to bias audits and worker protections; if the existential reading dominates, resources flow to alignment research. If neither dominates and resources fragment, both harms are under-addressed. If institutional co-existence holds, the constraint becomes coordination (both agendas pursued) rather than extraction (one suppressing the other).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether AI risk is primarily near-term or existential—a kernel contest not resolvable by facts alone.').

omega_variable(
    suppression_mechanism_distinction,
    'Is the measured suppression of x-risk framing a result of structural resource constraints (zero-sum competition for funding), or is it enforced suppression (active rhetorical and institutional campaigns to delegitimize x-risk research)?',
    'Audit research-funding allocation and institutional-authority patterns; interview researchers in both camps about barriers to cross-reading collaboration; examine rhetoric in fairness and x-risk communities for delegitimization patterns.',
    'If structural competition, the constraint is tangled_rope (genuine coordination of near-term harms + resource transfer). If enforced suppression, the constraint trends toward snare (near-term harms as cover story for suppressing rival frameworks). If rhetorical without institutional enforcement, the constraint is piton-adjacent (performative delegitimization masking the absence of actual exclusion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_distinction, empirical, 'Whether suppression of x-risk framing is structural competition or enforced delegitimization.').

omega_variable(
    beneficiary_capture_risk,
    'Do fairness researchers and institutions actually reduce measurable algorithmic discrimination in marginalized communities, or do they capture harm-reduction resources while harms persist?',
    'Track algorithmic discrimination outcomes in racialized populations, low-wage workers, and surveilled groups over time; compare harm reduction rates to research-funding and institutional-authority growth in fairness scholarship.',
    'If harms reduce at proportional rate, the constraint is coordinative. If harms persist or worsen while fairness infrastructure expands, the constraint becomes mandatroph—the institutional apparatus extracts resources without delivering harm reduction, and the founding problem outlives its vindication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_risk, empirical, 'Whether fairness institutions actually reduce harms or capture harm-reduction resources.').

omega_variable(
    pluralism_foreclosure,
    'Does institutional prioritization of near-term harms foreclose genuine engagement with existential-risk research, or can the two readings co-develop without suppression?',
    'Monitor whether researchers and institutions holding both readings exist and are resourced; track whether fairness venues accept existential-risk research and vice versa; assess whether funding mechanisms permit dual-problem research.',
    'If foreclosed, the constraint suppresses a genuine body of knowledge and the coexists_with relation should be re-evaluated toward influences or forecloses. If co-development is possible, the constraint remains tangled_rope without escalating to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pluralism_foreclosure, conceptual, 'Whether near-term and existential readings can coexist institutionally or are mutually foreclosing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 20, 0.41).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_prioritization__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization__existential_risk_reading).

% DUAL FORMULATION NOTE:
% The 'ai_risk_prioritization' kernel gives rise to two structurally distinct constraints: near_term_harms_reading (this story) prioritizes justice interventions for present harms; existential_risk_reading (sibling constraint) prioritizes alignment research for AGI threat. The kernel itself is the contested commitment about what constitutes 'AI risk.' Each reading instantiates a different constraint because the beneficiary sets, victim sets, timescales, and resource allocations differ fundamentally. The readings coexist in institutional practice (different communities hold each) but are engaged in ongoing contest for research authority, funding, and regulatory priority. This story models the near-term reading as tangled_rope—genuine justice coordination wrapped around resource extraction from the existential-risk domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_prioritization__near_term_harms_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
