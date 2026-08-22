% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__hybrid_coproduction_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Dual-Validation Co-Production Boundary on Legitimate Knowledge
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   In participatory research, public health, environmental governance, and
 *   engaged scholarship, a boundary rule now governs which knowledge claims
 *   count as legitimate: a claim must clear BOTH methodological-rigor review
 *   AND experiential-validity assessment, integrated through formal
 *   co-production processes in which researchers and affected communities
 *   jointly frame questions, gather evidence, and interpret results. Funding
 *   bodies write the requirement into calls; ethics boards and journals audit
 *   it; a professional broker class runs it. The rule solves a real problem —
 *   technocratic knowledge that ignores situated experience fails and breeds
 *   distrust — while simultaneously extracting under-compensated experiential
 *   labor from communities, converting it into academic impact capital, and
 *   filtering out the least-resourced knowledge holders through its own
 *   procedural demands. Claim and metrics are authored independently: the
 *   claimed type records the structure I take to be true (tangled_rope —
 *   genuine coordination entangled with asymmetric extraction under active
 *   enforcement); the metrics record how the arrangement actually operates.
 *   As a kernel reading, epsilon's referent is the standing dual-gate
 *   arrangement as THIS reading's own lights assess it: the reading endorses
 *   genuine integration, so measured extraction is the arrangement's
 *   departure from its own ideal.
 *
 * KEY AGENTS:
 *   - co_production_funding_bodies: agenda setter (institutional/arbitrage) — mandates the dual gate, collects legitimacy and risk-pooling
 *   - participatory_research_brokers: primary beneficiary (organized/mobile) — collects process income, consultancies, and methodological authority
 *   - academic_institutions: primary beneficiary (institutional/constrained) — converts participation into impact capital and grant income
 *   - community_participants: primary target (powerless/trapped) — contributes under-compensated experiential labor, retains a scarce voice channel
 *   - conventional_researchers: secondary target (powerful/constrained) — absorbs compliance overhead and interpretive-sharing demands
 *   - resource_poor_community_groups: excluded-and-paying (powerless/trapped) — filtered out by procedural demands, bearing illegitimacy costs
 *   - sts_epistemic_justice_analysts: analytical observer — sees the full structure, documents tokenism and credit flows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.58).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.5).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Dual-Validation Co-Production Boundary on Legitimate Knowledge").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'de49c0da-e14b-4093-954c-fd4b48ff513c').
narrative_ontology:cs_kernel_codification('de49c0da-e14b-4093-954c-fd4b48ff513c', distributed).
narrative_ontology:cs_authority_grounding('de49c0da-e14b-4093-954c-fd4b48ff513c', distributed).
narrative_ontology:cs_reading_relation('de49c0da-e14b-4093-954c-fd4b48ff513c', legitimate_knowledge_boundary__credentialed_expertise_reading, forecloses).
narrative_ontology:cs_reading_relation('de49c0da-e14b-4093-954c-fd4b48ff513c', legitimate_knowledge_boundary__experiential_pluralism_reading, forecloses).
narrative_ontology:cs_axiom('de49c0da-e14b-4093-954c-fd4b48ff513c', foundational, methodological_rigor_necessary_for_legitimacy).
narrative_ontology:cs_axiom_status(methodological_rigor_necessary_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('de49c0da-e14b-4093-954c-fd4b48ff513c', methodological_rigor_necessary_for_legitimacy, instrumental).
narrative_ontology:cs_axiom('de49c0da-e14b-4093-954c-fd4b48ff513c', foundational, experiential_validity_necessary_for_legitimacy).
narrative_ontology:cs_axiom_status(experiential_validity_necessary_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('de49c0da-e14b-4093-954c-fd4b48ff513c', experiential_validity_necessary_for_legitimacy, deontological).
narrative_ontology:cs_reference_frame('de49c0da-e14b-4093-954c-fd4b48ff513c', integrated_dual_validity_coproduction).
narrative_ontology:cs_drift_state('de49c0da-e14b-4093-954c-fd4b48ff513c', contemporary_participatory_mainstream, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('de49c0da-e14b-4093-954c-fd4b48ff513c', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_funding_bodies).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, participatory_research_brokers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, academic_institutions).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_participants).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, conventional_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, resource_poor_community_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_participants).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__hybrid_coproduction_reading, situated_knowledge_doctrine).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__hybrid_coproduction_reading, epistemic_justice_principle).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__hybrid_coproduction_reading, transdisciplinarity_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes co-production requirements into research funding calls, defines what counts as adequate patient and public involvement, and audits compliance reports. Collects legitimacy and risk-pooling benefits: every funded project arrives pre-legitimized as engaged and accountable. Can shift requirements across portfolios at will and rarely bears the operational cost of meeting them.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_funding_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Professional facilitators, involvement officers, and co-production consultants who design and run engagement processes. Careers, consultancies, and methodological authority rest on the existence of the dual gate; they collect income and standing from mediating between institutions and communities, and can move between universities, agencies, and NGOs selling the same service.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, participatory_research_brokers, beneficiary,
    organized, biographical, mobile, continental).

% Convert co-produced projects into impact case studies, publications, and grant renewals. Absorb compliance overhead but recoup it as reputational and financial capital; community-contributed knowledge enters their output pipelines. Exit is limited because impact-assessment regimes span the whole sector, so opting out means competitive disadvantage.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, academic_institutions, beneficiary,
    institutional, generational, constrained, global).

% Contribute lived experience, time, and testimony to advisory panels and co-designed studies, usually for token stipends or nothing. Their knowledge is formalized into documents and datasets they do not control, and authorship rarely reaches them. They receive a channel of voice they otherwise lack, which makes exit costly: leaving means losing the only route through which their concerns reach decision-makers.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_participants, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_participants, beneficiary).

% Trained in methodological rigor, they must now budget engagement workstreams, share interpretive authority with non-specialists, and accept slower publication cycles. Some find the exchange enriching; many experience it as bureaucratic dilution imposed by funders. Quiet resistance, minimal-compliance designs, or migration to non-participatory fields are the available exits, and funding access binds them to staying nominally inside.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, conventional_researchers, payer,
    powerful, biographical, constrained, global).

% Hold experiential knowledge directly relevant to the questions co-production addresses but lack staff time, proposal-writing capacity, and familiarity with engagement jargon. The procedural demands of formal co-production filter them out precisely where their knowledge matters most, and their un-co-produced claims are then discounted as illegitimate. They bear the cost of illegitimacy from outside the process.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, resource_poor_community_groups, excluded,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, resource_poor_community_groups, payer).

% Document tokenistic participation, trace where credit and compensation flow, and theorize epistemic injustice in knowledge governance. They neither collect from nor pay into the arrangement, and their analyses are the main external check on whether integration is real.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, sts_epistemic_justice_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__hybrid_coproduction_reading, academic_institutions).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__hybrid_coproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the trust-and-relevance problem between expert inquiry and affected publics: provides a single procedure by which claims about shared problems acquire both methodological and experiential standing, and coordinates who may speak with authority in participatory settings.
% TRANSFER_FUNCTION: Moves recognition, funding, and interpretive authority toward claims and processes that satisfy dual validation; moves experiential labor, testimony, and local knowledge from community participants into institutional outputs such as publications, impact cases, and grant renewals; moves compliance overhead onto researchers.
% ABSENT_VOICES: Resource-poor community groups whose experience is most relevant are filtered out by the co-production requirement's own procedural demands and sit outside the venues where their knowledge is adjudicated. Conventional researchers who reject interpretive sharing voice objection in disciplinary spaces rather than in co-production governance. Neither seat is present where the standard is set.
% DISAPPEARANCE_RATIONALE: Funded co-production workstreams, broker employment, impact-case accounting, and participatory governance fora all presuppose the dual gate. Overnight removal would split the field between credential-only and experience-only legitimacy, strand engagement-mandated grant lines, and force communities and researchers to renegotiate recognition from scratch.
% FOUNDING_PROBLEM: Technocratic expertise repeatedly failed affected communities: environmental-health and infrastructure decisions dismissed resident observation, clinical research ignored patient experience, and public trust in expert institutions declined. The boundary was rebuilt so that legitimate knowledge about shared problems must carry both methodological and experiential validity, integrated through joint production.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: documented policy failures in which dismissed local observation later proved decisive, long-run survey series showing sustained public distrust of expertise, and critiques arriving from both flanks — methodologists warning of rigor erosion and epistemic-justice scholars documenting continued exclusion. The problem's liveness is not attested only by the broker-funder complex.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.58 at interval end) because the dual gate converts unpaid experiential labor into institutional capital and imposes compliance overhead decoupled from epistemic payoff, while still delivering real integration in a minority of processes. Suppression is moderate (0.50): single-method venues and activist knowledge circuits persist as alternatives, but inside mandated spaces deviation is penalized through grant rejection and ethics friction. Theater is elevated (0.45) and rising: tick-box involvement, engagement-washing, and participant figures reported but not empowered are extensively documented, indicating Goodhart drift in which engagement metrics substitute for integration. Accessibility collapse is low-moderate (0.35) because credible alternatives remain reachable. Resistance is moderate-high (0.55): researcher disaffection, community withdrawal from advisory roles, and scholarly critique of co-production washing all press against the arrangement. The temporal series run on one shared grid (t=0,6,12,18,24,30) for all three tracked metrics: extraction accumulates as the broker layer thickens, theater grows as compliance reporting matures, and suppression hardens as mandatory co-production plans and audits replace voluntary experimentation. Enforcement picture is dynamic, hence the suppression_requirement series; trajectories are monotonic, not cyclical.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter/beneficiary seats should compute differently. From the funder's seat the arrangement is risk-managed legitimacy it built and can retune at will; from the broker's seat it is a meaningful vocation; from the participant's seat the same structure operates as extraction of testimony without commensurate return; from the conventional researcher's seat it operates as bureaucratic dilution of rigor. The engine computes these per-seat classifications from the structural data — power, exit options, and declared position — and the divergence between seats is the measurement, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Brokers, institutions, and funders are declared beneficiaries with mobile-to-arbitrage exits, deriving low directionality — the constraint subsidizes them, damping or inverting their effective extraction. Community participants are declared victims with trapped exit and no countervailing power, deriving directionality near the full-target end, so effective extraction amplifies sharply on them. Conventional researchers are victims with somewhat better mobility, sitting high but below participants. Resource-poor community groups carry high directionality despite formal absence: their illegitimacy costs are real and borne outside the process. No directionality overrides were needed because the beneficiary/victim declarations plus exit options already separate the seats cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the credibility crisis of expertise and the epistemic exclusion of affected communities — remains live, corroborated from outside the benefiting parties, so no mandatrophy resolution is declared. The tangled_rope classification prevents mislabeling in both directions: a pure-snare reading would erase documented integration successes, where co-produced environmental-health and clinical findings changed outcomes that single-method review had missed; a pure-rope reading would erase the labor capture, procedural exclusion, and growing performative share that the temporal series records. Holding both facts is the point. If the founding problem died — trust restored, exclusion ended — while the mandates persisted, the founding-problem-status x disappearance-verdict mismatch would fire and the theater-heavy decay path toward piton becomes the live hypothesis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the legitimate-knowledge boundary should govern — this hybrid co-production reading, the credentialed-expertise reading, or the experiential-pluralism reading?',
    'Comparative institutional analysis of the three readings'' operation across venues: barrier profiles, victim sets, enforcement costs, and epistemic outcomes under each standard.',
    'Adopting credentialed_expertise_reading removes the experiential gate and changes the victim set to community knowers and integration-oriented researchers; adopting experiential_pluralism_reading removes the methodological gate and changes the victim set to rigor-dependent domains. Either swap changes epsilon and the classification wholesale — this story''s numbers are indexical to this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story instantiates one reading of the legitimate_knowledge_boundary kernel; sibling readings are separate constraints.').

omega_variable(
    integration_vs_tokenism,
    'Does the dual gate produce genuine epistemic integration or predominantly performative compliance?',
    'Participation-depth audits: who sets agendas, who holds interpretive authority, whether co-produced findings alter decisions, versus box-ticking presence counts.',
    'If tokenism dominates, the theater share understates dysfunction and the payer seats'' computed classifications shift toward snare-like extraction maintained theatrically; if integration dominates, the coordination function is stronger than the metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_vs_tokenism, empirical, 'Whether co-production activity is substantively integrative or theatrically compliant.').

omega_variable(
    participation_labor_valuation,
    'Is community participants'' experiential labor compensated at fair value, or extracted and converted into others'' capital?',
    'Track stipend levels, co-authorship rates, data-sovereignty agreements, and downstream credit flows across a portfolio of co-produced projects.',
    'Systematic uncompensated conversion raises effective extraction on the primary payer seat and supports reading the transfer function as extraction-first; fair compensation would shift the arrangement toward the rope end of the hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_labor_valuation, empirical, 'Compensation status of experiential labor contributed under the dual gate.').

omega_variable(
    dual_gate_necessity_status,
    'Is dual validation an epistemic necessity — some claims genuinely requiring both validity types — or an institutional construct serving broker and funder interests?',
    'Identify knowledge claims that demonstrably failed under single-validity review and succeeded under integration (and vice versa) in settings where no broker-class interest was served by the outcome.',
    'If the gate is constructed, the constraint trends toward false-summit or snare dynamics with the beneficiary set exposed as the driver; if necessary, part of the measured extraction is the irreducible price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_gate_necessity_status, conceptual, 'Epistemic necessity versus institutional construction of the dual-validation requirement.').

omega_variable(
    procedural_inclusion_paradox,
    'Does the co-production requirement, in practice, include or exclude the least-resourced holders of relevant experience?',
    'Compare participation demographics in mandated co-production processes against the demographics of the affected populations those processes concern.',
    'If exclusionary, the constraint''s coordination claim weakens materially, the excluded seat''s illegitimacy costs belong formally in the victim ledger, and the inclusion rationale functions as cover for a credential-plus-capacity gate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_inclusion_paradox, empirical, 'Whether the inclusion mechanism includes or filters out the least-resourced knowledge holders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lkb_hybrid_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(lkb_hybrid_tr_t0, observed).
narrative_ontology:measurement(lkb_hybrid_tr_t6, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(lkb_hybrid_tr_t6, observed).
narrative_ontology:measurement(lkb_hybrid_tr_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement_basis(lkb_hybrid_tr_t12, observed).
narrative_ontology:measurement(lkb_hybrid_tr_t18, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 18, 0.33).
narrative_ontology:measurement_basis(lkb_hybrid_tr_t18, observed).
narrative_ontology:measurement(lkb_hybrid_tr_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(lkb_hybrid_tr_t24, observed).
narrative_ontology:measurement(lkb_hybrid_tr_t30, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement_basis(lkb_hybrid_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(lkb_hybrid_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(lkb_hybrid_be_t0, observed).
narrative_ontology:measurement(lkb_hybrid_be_t6, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(lkb_hybrid_be_t6, observed).
narrative_ontology:measurement(lkb_hybrid_be_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement_basis(lkb_hybrid_be_t12, observed).
narrative_ontology:measurement(lkb_hybrid_be_t18, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 18, 0.51).
narrative_ontology:measurement_basis(lkb_hybrid_be_t18, observed).
narrative_ontology:measurement(lkb_hybrid_be_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement_basis(lkb_hybrid_be_t24, observed).
narrative_ontology:measurement(lkb_hybrid_be_t30, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(lkb_hybrid_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(lkb_hybrid_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(lkb_hybrid_su_t0, observed).
narrative_ontology:measurement(lkb_hybrid_su_t6, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 6, 0.34).
narrative_ontology:measurement_basis(lkb_hybrid_su_t6, observed).
narrative_ontology:measurement(lkb_hybrid_su_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement_basis(lkb_hybrid_su_t12, observed).
narrative_ontology:measurement(lkb_hybrid_su_t18, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 18, 0.43).
narrative_ontology:measurement_basis(lkb_hybrid_su_t18, observed).
narrative_ontology:measurement(lkb_hybrid_su_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 24, 0.47).
narrative_ontology:measurement_basis(lkb_hybrid_su_t24, observed).
narrative_ontology:measurement(lkb_hybrid_su_t30, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(lkb_hybrid_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_pluralism_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'legitimate knowledge' decomposes, per the epsilon-invariance principle, into at least three structurally distinct boundary rules: credential-gated rigor, experience-gated pluralism, and dual-gated co-production. Each yields a different epsilon, a different victim set, and different enforcement machinery, so each is authored as its own constraint story and linked here. Direction of influence: the credentialed reading is the historically prior standard whose authority this reading partially inherits and partially supplants; the experiential-pluralist reading supplies the experiential-validity premise that this reading hardens from option into requirement. This file authors ONLY the hybrid co-production reading; measuring the boundary through a different observable belongs to the sibling files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
