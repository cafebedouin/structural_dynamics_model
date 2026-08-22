% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__legalization_reading, []).

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
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Adult Autonomy + Third-Party Harm Boundary (Legalization Reading)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The legalization reading of substance control legitimacy asserts that
 *   competent adults have autonomy over their own substance use, and state
 *   authority is justified only to prevent third-party harms — not to enforce
 *   moral visions or constrain individual choice. Under this reading, the
 *   constraint operates as a tangled rope: it genuinely solves a coordination
 *   problem (where to draw the autonomy/harm boundary) while extracting costs
 *   from victims of third-party harms (impaired drivers' victims,
 *   secondhand-exposed populations) and from prohibited-market incumbents
 *   (suppliers, enforcement bodies). Users exit the victim set because
 *   autonomy is their benefit; the constraint's extractiveness derives from
 *   inadequate third-party harm prevention and from regulatory capture by the
 *   legal market operators. This reading sits in structural contest with the
 *   prohibition reading (autonomy is illegitimate; state duty is moral
 *   prevention) and the harm-reduction reading (substance use is a health
 *   issue, not an autonomy issue; criminalization itself is the harm). The
 *   claim/metric divergence is intentional: the reading claims tangled-rope
 *   structure (genuine coordination + asymmetric extraction), and the metrics
 *   track the specific extractiveness pattern of inadequate harm boundary
 *   enforcement and legal-market rentier dynamics.
 *
 * KEY AGENTS:
 *   - adults_with_autonomy_preference: beneficiary (exit victim set under legalization reading)
 *   - legal_substance_market_operators: agenda_setter + beneficiary (capture regulatory rents)
 *   - regulatory_authority: agenda_setter (enforces autonomy/harm boundary)
 *   - impaired_driver_injury_victims: payer (bear third-party harm cost; not party to autonomy claim)
 *   - secondhand_exposure_populations: payer (constrained exit; residual harm spillover)
 *   - youth_and_adolescents: excluded (structurally outside autonomy framework; exposed to externalities)
 *   - prohibited_substance_suppliers: payer + excluded (lose market to legalized competitors)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.62).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.41).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Adult Autonomy + Third-Party Harm Boundary (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, '29246d91-4f8b-4829-804f-928dbc777b08').
narrative_ontology:cs_kernel_codification('29246d91-4f8b-4829-804f-928dbc777b08', distributed).
narrative_ontology:cs_authority_grounding('29246d91-4f8b-4829-804f-928dbc777b08', distributed).
narrative_ontology:cs_reading_relation('29246d91-4f8b-4829-804f-928dbc777b08', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('29246d91-4f8b-4829-804f-928dbc777b08', substance_control_legitimacy__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('29246d91-4f8b-4829-804f-928dbc777b08', foundational, competent_adult_autonomy_principle).
narrative_ontology:cs_axiom_status(competent_adult_autonomy_principle, holdable).
narrative_ontology:cs_axiom_grounding('29246d91-4f8b-4829-804f-928dbc777b08', competent_adult_autonomy_principle, deontological).
narrative_ontology:cs_axiom('29246d91-4f8b-4829-804f-928dbc777b08', secondary, third_party_harm_boundary_legitimacy).
narrative_ontology:cs_axiom_status(third_party_harm_boundary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('29246d91-4f8b-4829-804f-928dbc777b08', third_party_harm_boundary_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('29246d91-4f8b-4829-804f-928dbc777b08', autonomous_adult_substance_access).
narrative_ontology:cs_drift_state('29246d91-4f8b-4829-804f-928dbc777b08', contemporary_regulatory_maturation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('29246d91-4f8b-4829-804f-928dbc777b08', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, legal_substance_market_operators).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, adults_with_autonomy_preference).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, impaired_driver_injury_victims).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, secondhand_exposure_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, workplace_and_transport_safety_bodies).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, prohibited_substance_suppliers).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, individual_autonomy_doctrine).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, harm_principle_political_philosophy).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, competent_adult_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal access to substances of choice without criminal penalty, medical diagnosis pathway, or therapeutic framing. They can obtain, possess, and consume within the legalized market framework. Their main structural cost is regulation compliance (licensing, quality standards, consumption venue restrictions) and any taxation that applies. Exit is geographically bounded — they can relocate to jurisdictions with legalization but bear relocation costs.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, adults_with_autonomy_preference, beneficiary,
    organized, biographical, mobile, national).

% Operate regulated, licensed distribution and retail infrastructure for formerly prohibited substances (cannabis, potentially psychedelics, alcohol equivalents). They set market supply, pricing, product formulation, and marketing within regulatory bounds. They capture profits from the legalized market and have strong incentives to minimize enforcement (fewer barriers = higher sales). They also bear compliance and regulatory costs.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, legal_substance_market_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, legal_substance_market_operators, beneficiary).

% Enforces the legalization boundary: permits adult consumption, prevents sales to minors, monitors for third-party harm (impaired driving, workplace safety, product contamination), revokes licenses for violations. They adjudicate contested cases (what counts as third-party harm, where drawing the boundary) and collect licensing revenue. Their legitimacy rests on the harm-principle boundary holding in practice.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).

% Suffer injury, death, property damage, or trauma from accidents caused by drivers under the influence of now-legal substances. They did not consent to the consumption decision; they bear the medical, financial, and psychological costs of others' use. Their only structural recourse is post-hoc liability and crash prevention infrastructure (detection, prosecution, rehabilitation).
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, impaired_driver_injury_victims, payer,
    powerless, immediate, trapped, local).

% Experience secondhand exposure (cannabis smoke in shared housing, workplaces, public spaces; airborne drug particles; odor and discomfort). They negotiate venue restrictions (no-use zones, smoking bans) but bear residual exposure from drift, shared HVAC systems, or enforcement gaps. Their exit is constrained by geography and social/economic ties; their power is moderate (can advocate for restrictions but cannot unilaterally exclude exposure).
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, secondhand_exposure_populations, payer,
    moderate, biographical, constrained, local).

% Are structurally excluded from legal consumption (age-gating, sales restrictions) but exposed to market proliferation (increased availability, marketing, peer access, secondhand exposure). They would object to expanded access and aggressive marketing but are not parties to the legalization framework; adults made the autonomy-based claim on their behalf as a separate constituency.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, youth_and_adolescents, excluded,
    powerless, biographical, trapped, local).

% Study the outcomes of legalization: prevalence, consumption patterns, injury rates, long-term health trajectories, workplace safety, and gateway effects. They generate evidence the regulatory authority and courts use to adjudicate third-party harm claims and boundaries. They are not decision-makers but their empirical work reshapes what counts as a legitimate harm constraint.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, public_health_researchers, observer,
    institutional, generational, analytical, global).

% Lose market share and revenue as legalization redirects demand to regulated suppliers. They remain excluded from the legal framework (cannot obtain licenses, cannot undercut prices by avoiding compliance). They survive by serving unmet demand (purity, price, access speed) but face intensified law enforcement as the legal market establishes the permissible boundary.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, prohibited_substance_suppliers, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, prohibited_substance_suppliers, excluded).

% Gain a legible, enforceable boundary (impairment is measurable and prosecutable at work/driving, not a question of moral judgment). They can rely on legal substance status for detection and remediation. They also bear costs of enforcement infrastructure (testing, training, liability rules) and must adjudicate difficult cases (residual metabolites, tolerance, medical necessity).
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, workplace_and_transport_safety_bodies, beneficiary,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__legalization_reading, legal_substance_market_operators).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal boundary between permissible adult autonomy (individual consumption choice) and constrained harm (third-party injury). Solves the coordination problem of how to honor adults' self-determination while maintaining public safety — achieved by delegating consumption decisions to individuals and reserving state authority for externalizable harms only.
% TRANSFER_FUNCTION: Moves the extraction burden from criminalized users (criminal justice processing, incarceration, stigma, economic exclusion) to regulated market participants (licensing costs, taxation, compliance infrastructure) and to victims of third-party harms (injury, secondhand exposure, enforcement gaps). The legalization reading also transfers legitimacy authority from police/prosecutors to health regulators and civil liability courts.
% ABSENT_VOICES: Youth and adolescents are structurally excluded — they cannot consent to the autonomy framework but live in its externalities (peer access, secondhand exposure, marketing). Prohibited-market suppliers are also excluded; they would argue the legalization boundary artificially favors state-licensed producers and excludes them from the rents. Low-income populations bear disproportionate exposure and enforcement costs but are not seated at the boundary-adjudication table.
% DISAPPEARANCE_RATIONALE: If this legalization reading's boundary vanished overnight — if the autonomy claim were repealed and criminalization reinstated — criminal justice processing would resume for millions of users, clandestine markets would expand, and regulatory infrastructure would shift from health oversight to drug enforcement. The legitimacy structure that permits legalization would collapse; the constraint does not describe a natural equilibrium but an authored arrangement.
% FOUNDING_PROBLEM: Criminalization of competent adults' substance use generates disproportionate carceral burden, disrupts livelihoods and families, creates perverse incentives for criminal organizations, and treats a public health problem as a moral failure. The legalization reading was built to solve the harms of prohibition itself.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers, criminal justice reform advocates (outside the beneficiary set), and legalization jurisdictions' empirical records attest the carceral burden of prohibition is real and substantial. Prohibited-market operators and criminal-justice incumbents dispute that legalization solves the problem (they cite new harms: youth access, potency creep, traffic deaths); the founding problem's status is live but contested — the arrangement persists because beneficiaries prioritize autonomy-as-harm-reduction over alternative framings.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 to 0.62 over the interval because legal-market operators' pricing power consolidates (higher monopoly rents as clandestine competition fades) and because third-party harm boundary enforcement reveals gaps — regulatory capacity lags market proliferation, leaving residual harm externalized. Theater rises modestly (0.12 to 0.28) as regulatory messaging emphasizes consumer choice and safety while enforcement focuses on market control (protecting licensed suppliers from unlicensed competitors). Suppression falls sharply (0.68 to 0.41) because the legalization reading REDUCES coercive force against users — they are no longer the suppression target; suppression effort redirects to enforcing the boundary (age-gating, impaired-driving prosecution, licensing exclusivity). The metric series are authored on one shared time grid: every measurement point reports all three metrics' values contemporaneously, preventing false dating of type transitions.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and autonomy-beneficiary seats, the legalization reading is genuine coordination with acceptable residual extraction (third-party harm is unavoidable and proportionate to autonomy gain). From the third-party-harm-victim seats, the same structure is a transfer of extraction burden from politically powerful users to powerless, non-consenting injury victims. The regulatory authority sits between: it captures real coordination credit (establishing a workable boundary) but also increasingly captures market-operator interests (licensing exclusivity, restriction of supply, price floors) — making it partly an agenda-setter and partly a market enforcer. The engine computes these divergences directly from power, time_horizon, exit_options, and the beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   The autonomy beneficiaries (organized adults) and the legal-market operators (institutional) sit near the beneficiary end of the directionality spectrum (low d → low/negative effective extraction from them). The third-party harm victims (powerless, trapped, immediate time horizon) sit near the target end (high d → high effective extraction). The regulatory authority carries dual positioning: it sets the agenda (d lower) but also enforces against violators (d higher when enforcement becomes asymmetric against marginalized populations). The key structural insight is that legalization RELOCATES who bears extraction: criminalized users (high d in prohibition reading) become low-d beneficiaries; third-party harms become the new extraction surface (high d), and their victims are powerless to exit. This reading's directionality structure differs radically from prohibition and harm-reduction readings because the victim set and beneficiary set are redefined by the autonomy claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resists mandatrophy because the autonomy foundation remains contestable: is individual choice over consumption a fundamental right (autonomy reading), a public health risk requiring intervention (harm-reduction reading), or a moral failure warranting prohibition (prohibition reading)? The three readings share a kernel but produce different victim sets and extraction profiles. This legalization reading avoids false summitry because it honestly authors suppression as declining (users are decriminalized, not suppressed) and extraction as shifting rather than disappearing. The third-party harm boundary is performative to some degree (regulatory messaging emphasizes safety while market captures rents), but the performance is not the entire constraint — real boundary adjudication happens in impaired-driving prosecution and youth-access enforcement, where the harm principle is genuinely operative. The mandatrophy question is episodic: does the autonomy claim retain legitimacy if third-party harm prevention systematically fails? That opens to the omega variable addressing harm-boundary enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    third_party_harm_boundary_enforcement,
    'Does the regulatory regime''s capacity to detect, prosecute, and remediate third-party harms (impaired driving, workplace contamination, secondhand exposure) keep pace with expanded legal availability?',
    'Time-series data on impaired-driving arrest rates, traffic fatality attribution, workplace-safety incidents, and pollution measurements in legalization jurisdictions compared to control jurisdictions with comparable enforcement budgets.',
    'If enforcement lags and harms accumulate undetected, the constraint drifts from tangled-rope (coordination + extraction) to snare (extraction disguised as coordination). If enforcement strengthens proportionally, the boundary holds and the tangled-rope classification persists. This gap determines whether the autonomy claim is authentic or performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_harm_boundary_enforcement, empirical, 'Whether third-party harm boundary enforcement is adequate to sustain legalization''s structural legitimacy.').

omega_variable(
    regulatory_capture_by_legal_operators,
    'Do legal market operators influence regulatory standard-setting (potency limits, marketing restrictions, pricing floors) in ways that entrench market power rather than protect third-party harms?',
    'Institutional analysis of regulatory agency funding, personnel overlap, and decision history: do regulations track harm evidence or operator interests? Comparison with jurisdictions using arm''s-length oversight (public health agencies independent of licensing revenue).',
    'If operators systematically capture regulatory boundaries, the constraint''s extraction component becomes decoupled from genuine harm prevention and the constraint drifts toward snare (extraction justified by a harm-prevention boundary that operates in market operators'' favor). If regulatory independence holds, extraction remains bound to real third-party harm and tangled-rope classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_by_legal_operators, empirical, 'Whether legalization''s regulatory framework is captured by market operators or maintains genuine harm-prevention independence.').

omega_variable(
    autonomy_claim_reading_dependence,
    'Is the autonomy principle an irreducible commitment of this reading, or would the constraint be restructured (become harm-reduction or prohibition) if empirical evidence shifted the autonomy claim''s perceived legitimacy?',
    'Counterfactual reasoning: if evidence emerged showing substantial long-term cognitive effects on 18-25 year-old users, or if legalization produced unexpected potency escalation, would the autonomy reading''s endorsers shift to prohibition or harm-reduction framings?',
    'If autonomy is foundational (axiom status = holdable), the reading persists even against contrary evidence; enforcement adapts but the underlying claim stands. If autonomy is contingent on empirical support, the reading is empirically_contingent and foreclosure is possible if evidence undermines competence assumptions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_claim_reading_dependence, conceptual, 'Whether autonomy is a foundational normative commitment or a contingent empirical claim for this reading.').

omega_variable(
    suppression_mechanism_internalization,
    'As suppression declines (criminal penalties lifted), do users internalize stigma or autonomy-reinforcing narratives? Does the lived experience of decriminalization reduce or entrench extraction from third-party-harm victims?',
    'Post-decriminalization ethnographic and survey data tracking users'' self-narrative shifts, harm-mitigation behavior, and community attitudes. Comparison of pre/post decriminalization harm-reduction engagement (treatment seeking, harm-reduction technology adoption, peer accountability).',
    'If decriminalization internalizes responsibility-taking (users self-regulate more, seek treatment, adopt harm-reduction), third-party harms decline and the constraint''s extraction from harm victims falls — tangled-rope becomes more balanced. If decriminalization internalizes entitlement (users externalize costs, reject mitigation), extraction intensifies and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether decriminalization shifts users'' internalized suppression to internalized responsibility or entitlement.').

omega_variable(
    sibling_reading_foreclosure,
    'Is this legalization reading logically foreclosed by the prohibition reading''s moral-duty axiom, or do they coexist as genuinely contested commitments?',
    'Jurisprudential analysis: can a single legal framework hold both autonomy and moral-prevention principles simultaneously, or does endorsing one require denying the other? Or do they persist as competing framings held by different parties?',
    'If they foreclose each other, the kernel is not pluralistic but a zero-sum contest; legalization''s persistence requires suppressing prohibition reading''s authority. If they coexist, both readings remain live options and the kernel embeds genuine disagreement. This determines whether cs_structure.reading_relations = forecloses or coexists_with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether legalization and prohibition readings logically foreclose or coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t5, substance_control_legitimacy__legalization_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(subs_tr_t5, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__legalization_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__legalization_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(subs_tr_t15, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(subs_tr_t20, observed).
narrative_ontology:measurement(subs_tr_t25, substance_control_legitimacy__legalization_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(subs_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t5, substance_control_legitimacy__legalization_reading, base_extractiveness, 5, 0.46).
narrative_ontology:measurement_basis(subs_be_t5, observed).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__legalization_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__legalization_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(subs_be_t15, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(subs_be_t20, observed).
narrative_ontology:measurement(subs_be_t25, substance_control_legitimacy__legalization_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(subs_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t5, substance_control_legitimacy__legalization_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(subs_su_t5, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__legalization_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__legalization_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement_basis(subs_su_t15, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement_basis(subs_su_t20, observed).
narrative_ontology:measurement(subs_su_t25, substance_control_legitimacy__legalization_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement_basis(subs_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__legalization_reading, 0.18).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% substance_control_legitimacy is a contested kernel instantiated in three structurally distinct readings. The legalization reading relocates the victim set (users → third-party-harm populations), centers autonomy as the legitimacy principle, and exhibits tangled-rope extraction through inadequate harm-boundary enforcement and market-operator capture. The prohibition and harm-reduction readings produce different victim sets and directionality profiles from the same kernel. All three stories are linked as constraint family members; network edges establish that legalization's framings downstream-affect both sibling readings by redefining what counts as legitimate harm and who bears extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
