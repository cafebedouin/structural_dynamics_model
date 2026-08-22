% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__proportionality_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Public Health Mandate Authority Under Proportionality Reading
 *   domain: public_health/constitutional_rights
 *
 * SUMMARY:
 *   Public health mandate authority is contested between three readings of
 *   the same constitutional kernel: bodily autonomy (no coercion permitted),
 *   proportionality (coercion permitted only when threat-severity,
 *   alternative-scarcity, coercion-magnitude, and duration are calibrated),
 *   and public-health-primary (coercion is obligation to protect vulnerable
 *   commons). This JSON instantiates the PROPORTIONALITY READING only. The
 *   constraint is written as a dynamic tangled_rope: the same mandate
 *   structure is legitimate under proportionality conditions (high threat, no
 *   alternatives, limited duration) and illegitimate extraction under
 *   proportionality's opposite conditions (low threat, abundant alternatives,
 *   indefinite duration). The measurement series traces a typical crisis arc:
 *   low threat → surge → peak → de-escalation → endemic → institutional
 *   persistence. Theater and suppression rise as threat falls — the classic
 *   signature of a constraint whose primary function has atrophied but whose
 *   enforcement apparatus persists.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.58).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.64).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority Under Proportionality Reading").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public_health/constitutional_rights").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, 'db2b9aac-7235-4994-b692-f700744a4eed').
narrative_ontology:cs_kernel_codification('db2b9aac-7235-4994-b692-f700744a4eed', formalized).
narrative_ontology:cs_authority_grounding('db2b9aac-7235-4994-b692-f700744a4eed', lineage).
narrative_ontology:cs_interpretation_layer_present('db2b9aac-7235-4994-b692-f700744a4eed').
narrative_ontology:cs_reading_relation('db2b9aac-7235-4994-b692-f700744a4eed', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('db2b9aac-7235-4994-b692-f700744a4eed', public_health_mandate_authority__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('db2b9aac-7235-4994-b692-f700744a4eed', foundational, mandate_legitimacy_threat_scaled).
narrative_ontology:cs_axiom_status(mandate_legitimacy_threat_scaled, holdable).
narrative_ontology:cs_axiom_grounding('db2b9aac-7235-4994-b692-f700744a4eed', mandate_legitimacy_threat_scaled, instrumental).
narrative_ontology:cs_axiom('db2b9aac-7235-4994-b692-f700744a4eed', foundational, alternatives_availability_constrains_coercion).
narrative_ontology:cs_axiom_status(alternatives_availability_constrains_coercion, holdable).
narrative_ontology:cs_axiom_grounding('db2b9aac-7235-4994-b692-f700744a4eed', alternatives_availability_constrains_coercion, deontological).
narrative_ontology:cs_axiom('db2b9aac-7235-4994-b692-f700744a4eed', secondary, duration_limits_mandatory).
narrative_ontology:cs_axiom_status(duration_limits_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('db2b9aac-7235-4994-b692-f700744a4eed', duration_limits_mandatory, instrumental).
narrative_ontology:cs_reference_frame('db2b9aac-7235-4994-b692-f700744a4eed', threat_proportionate_democratic_coercion).
narrative_ontology:cs_drift_state('db2b9aac-7235-4994-b692-f700744a4eed', endemic_steady_state_plus_18_months, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('db2b9aac-7235-4994-b692-f700744a4eed', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, healthcare_infrastructure).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, vaccine_hesitant_unvaccinated).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, individuals_with_contraindications).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, individuals_with_contraindications).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, healthcare_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces mandate based on proportionality assessment: measures threat severity (case fatality rate, transmission velocity, healthcare system capacity), evaluates available alternatives (testing, therapeutics, ventilation), calibrates coercive measures (vaccination requirements vs. testing-exemption pathways), and sets duration limits tied to epidemiological thresholds. Justifies mandate as protecting vulnerable populations while minimizing unnecessary coercion from those it governs. Under proportionality reading, has institutional obligation to lift mandate when proportionality conditions invert (threat drops, alternatives available).
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Cannot be safely vaccinated due to medical condition; depend on herd immunity thresholds to reduce infection risk. Have no exit option — vulnerability is permanent. Benefit directly when mandate reaches threshold severity and unvaccinated prevalence is reduced. Face increased mortality when mandate is relaxed while threat level remains high. Under proportionality reading, their benefit is legitimate only when threat justifies the mandate; at low threat they receive no benefit and the constraint does not serve them.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, universal).

% Bear the coercive measure when mandate is in effect: employment restrictions, education access exclusion, public space limitations. Objections may be based on risk calculation (personal risk from vaccination vs. personal risk from infection), religious conviction, or distrust of medical institutions. Under proportionality reading, their extraction is legitimate when threat is high and alternatives scarce (t=12); becomes illegitimate when threat is low and alternatives abundant (t=36). Exit option is constrained because refusing vaccination means accepting exclusion, not escaping coercion.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, vaccine_hesitant_unvaccinated, payer,
    moderate, biographical, constrained, national).

% Cannot be safely vaccinated due to documented medical condition (severe allergic reaction history, myocarditis risk, immunological incompatibility). Face exclusion from employment, education, public spaces under mandate even though vaccination is medically impossible. Are not hesitant — they are trapped. Depend on herd immunity as alternative to vaccination (same as immunocompromised). Under proportionality reading, coercion against them is never legitimate because vaccination is not a real exit option (trapped rather than constrained). Often conflated with volitional refusers, creating spillover suppression and raising measured resistance.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, individuals_with_contraindications, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__proportionality_reading, individuals_with_contraindications, beneficiary).

% Benefit when mandate reduces unvaccinated prevalence, lowering hospitalization surge and staffing crisis. Experience extraction pressure under mandate (staffing mandates, liability exposure, financial uncertainty). Under proportionality reading, the benefit is real when surge threat is high (t=12: legitimately protected); benefit disappears at low threat (t=36: still bearing extraction without coordination function). Experience theater escalation: as threat falls, management must maintain threat narrative to sustain mandate authority and staffing mandates.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, healthcare_systems, beneficiary,
    organized, biographical, constrained, national).

% Would provide alternative exits to mandate (antivirals, monoclonal antibodies, early therapeutics reduce need for universal vaccination mandate). Are structurally excluded from authority reassessment through funding choices (vaccine R&D funded at scale, therapeutic R&D under-resourced) and EUA restrictions. Their participation would lower the proportionality threshold (more alternatives available = less mandate justified). Exclusion is structural to mandate persistence.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, therapeutic_and_antiviral_developers, excluded,
    institutional, biographical, constrained, global).

% Benefit from mandate (guaranteed demand, indemnification against liability). Have incentive to suppress discussion of adverse effects, alternatives, and threat reassessment. Structurally shielded from proportionality assessment through liability protections, emergency declarations, and authority deference. Their financial interest is aligned with mandate persistence regardless of proportionality conditions — this creates structural bias against honest threat reassessment.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, vaccine_manufacturers, excluded,
    institutional, biographical, analytical, global).

% Provides data that feeds proportionality assessment: threat severity estimates, vaccine effectiveness, natural-immunity parameters, therapeutic options, alternative protective measures. Under proportionality reading, their role is structural — they are not advocates for mandate or for refusal, but provide evidence base that determines legitimate coercion level. Face pressure to suppress findings that would lower proportionality threshold (waning threat, adequate alternatives, vaccine adverse effects) and amplify findings that raise it. Journals and advisory boards exclude dissenting voices to maintain narrative coherence.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, epidemiological_research_community, observer,
    institutional, generational, analytical, global).

% The bodily_autonomy_primary reading and public_health_primary reading are the sibling positions in the same kernel. They are not agents but are conceptual commitments that the proportionality reading must negotiate. The proportionality reading occupies the logical middle ground: it acknowledges both autonomy (bodily integrity has weight) and collective protection (vulnerable populations have claims) as legitimate values, but makes their balance context-dependent rather than absolute.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, competing_constitutional_readings, observer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(public_health_mandate_authority__proportionality_reading, competing_constitutional_readings).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__proportionality_reading, public_health_authority).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves collective-action problem: individual incentive to avoid vaccination creates externality (unvaccinated infection risk to immunocompromised populations, healthcare system surge). Mandate coordinates protective behavior at population scale. Under proportionality reading, this coordination is legitimate when the threat severity is high enough that voluntary uptake will not reach herd immunity threshold AND alternatives (therapeutics, testing) are insufficient to protect vulnerable populations. When threat is low or alternatives abundant, the coordination problem no longer exists and mandate becomes illegitimate extraction.
% TRANSFER_FUNCTION: Moves bodily autonomy (control over medical decisions) from vaccine-hesitant and contraindicated individuals to the collective benefit of immunocompromised populations and healthcare system stability. Under proportionality reading, this transfer is justified when threat is high and alternatives are scarce — the transfer is necessary to solve a genuine coordination failure. When threat is low and alternatives abundant, the same transfer is unjustified because no genuine coordination problem requires it.
% ABSENT_VOICES: Individuals who would object on religious or philosophical grounds but fear employment/education retaliation are suppressed into silence; alternative-therapeutic researchers and manufacturers are excluded from advisory bodies (conflict of interest with vaccine manufacturers); epidemiologists skeptical of mandate proportionality are deplatformed or marginalized from policy influence; families of individuals who experienced vaccine adverse effects cannot advocate for caution without suppression; contraindicated individuals are often conflated with volitional refusers, silencing the legitimate objection category.
% DISAPPEARANCE_RATIONALE: If mandate legitimacy depended consistently on proportionality assessment and was automatically lifted when conditions inverted, the entire public health authority structure would become time-limited and conditional. Immunocompromised populations would maintain need for herd immunity, but through different mechanisms (therapeutics, voluntary vaccination, targeted protection) when threat is low. Healthcare systems would require surge planning but not coercive mandates during endemic states. The authority would lose the perpetual emergency frame and would be constrained to reassess threat genuinely. If mandate disappeared under proportionality reading at endemic steady state, this world would rearrange because institutions have become dependent on the mandate's persistence.
% FOUNDING_PROBLEM: Pathogenic threats (emerging diseases with high mortality, novel variants) create a herd immunity coordination failure: vulnerable populations (immunocompromised, too young for vaccine) depend on collective vaccination to reach protective thresholds. Individual incentive to avoid vaccination risk creates free-rider problem that voluntary uptake does not solve.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is live for Ebola-scale threats (70%+ CFR, no alternatives) — epidemiological consensus that herd immunity is necessary. Contested for COVID-19-era pathogens — early threat was clearly existential; by 2022 variant landscape, alternative immunity (prior infection + vaccination), therapeutic availability (antivirals), and lower CFR (0.1-0.3% in vaccinated populations) shifted the proportionality calculus. The founding problem is dead for endemic seasonal respiratory viruses (circulating for generations, widespread immunity, therapeutics routine). Corroboration from epidemiological literature (PNAS, Lancet, CDC data) establishes that threat severity has fallen from 2020 estimates; alternative availability has risen. The public health authority has NOT reassessed founding-problem status proportionately — mandate persistence is decoupled from the foundational coordination need, which is the diagnostic signature of mandatrophy.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__proportionality_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness scores track the proportionality-assessment truth condition: when threat is high and alternatives are scarce (t=12), extractiveness is 0.58 (high, but justified by legitimate collective protection). When threat is low and alternatives abundant (t=30, t=36), extractiveness remains elevated (0.25–0.22) because the mandate persists even after proportionality conditions have inverted — the constraint has become illegitimate extraction sustained by institutional inertia. Theater and suppression tell the same story: at peak threat (t=12), both are moderate because the coercive apparatus tracks genuine threat. By t=36 (endemic+2yr), theater is high (0.71) and suppression high (0.73) despite threat being minimal (0.22). This inversion is the diagnostic signature of a proportionality-violating mandate: coercion and theater persist when the proportionality threshold has passed. The measurement grid uses one shared time axis so every metric is comparable across time. Accessibility_collapse (0.68) reflects the constraint's structure: alternatives do collapse once vaccination is made mandatory (exit options shift from 'mobile' to 'trapped' or 'identity_locked'), but the collapse is reversible IF the proportionality conditions are reversed (threat drops, alternatives return). Resistance (0.72) is high because the proportionality reading makes refusal rational when threat is low — resistance is not irrationality or ideology, but response to illegitimate coercion as measured by proportionality standards.
 *
 * PERSPECTIVAL GAP:
 *   The authority seat (public health official) and the payer seats (vaccine-hesitant, contraindicated) compute different types from this structure. From the authority's position: when threat is high (t=12) the mandate is a legitimate rope (coordination + minimal proportionate coercion to solve herd-immunity problem). When threat has dropped (t=36) the authority rationally perceives the mandate as still legitimate (it 'worked', and vigilance is warranted) — this is the theater frame. From the payer's position under proportionality: when threat was high (t=12) the coercion was unjust but necessary (extractive but unavoidable); when threat dropped (t=36) the same coercion is pure extraction with no justification — the constraint has become a snare from their seat. The engine computes this divergence from the stakeholder situation data (authority has 'analytical' exit options, vaccine-hesitant have 'constrained' exit options; authority has no victim status, payers are named as victims). The claim-metric gap is deliberate and structurally central to the proportionality reading: the constraint is CLAIMED as tangled_rope (legitimate coordination + extraction trade-off) when proportionality conditions hold, but METRICS reveal it becomes snare when those conditions invert. A generator tuning metrics to match the claim would destroy the reading's diagnostic power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from the beneficiary/victim structure modulated by proportionality conditions. Immunocompromised populations are clear beneficiaries (d near 0.0) — they depend on herd immunity and have no exit option (trapped in vulnerability). Healthcare systems are mixed beneficiaries: they benefit when surge threat is high, but experience staffing extraction even at low threat (collective beneficiary role, d ≈ 0.2). Vaccine-hesitant and contraindicated individuals are victims whose directionality swings with threat: when threat is high (t=12, proportionality justified), their d ≈ 0.75 (they are targets, but extraction is legitimate per proportionality). When threat is low (t=36, proportionality inverted), their d ≈ 0.90 (they are full targets, extraction is illegitimate). This dynamic directionality is unusual — most constraints have fixed d. Under the proportionality reading it is essential: the reading's entire logical structure is that directionality toward payers becomes unjustified when proportionality conditions fail. No override is needed because the structural data (threat severity, alternatives availability) already encode the proportionality conditions. As threat drops from 0.12 to 0.22, the engine's directionality derivation should compute d shifting from 0.75 to 0.90 (increasing extraction illegitimacy) — the metrics alone encode this.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (herd immunity threshold for pathogenic threats killing vulnerable populations) is LIVE when threat is high, DEAD when threat has fallen to endemic levels. The constraint's post-threat persistence (t=30, t=36) is the classic mandatrophy symptom: the founding problem is dead (threat endemic, immunity widespread, alternatives abundant), but the constraint persists because institutional actors benefit from it (pharmaceutical continuing revenue, public health bureaucracy, authority expansion) and because suppression has made reversal costly. The measurement series explicitly tracks mandatrophy's rise: as theater climbs and suppression intensifies despite falling threat, the constraint shifts from tangled_rope (legitimate coordination with proportionate extraction) to snare (pure extraction using the facade of health authority). The proportionality reading prevents mislabeling this as continued coordination: proportionality reading forces honest reassessment of whether the founding problem still exists. If authorities refuse that reassessment and maintain the mandate anyway, they are confessing that the constraint has become illegitimate by their own reading's standards. This is not a bug in the reading — it is the reading's diagnostic function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_severity_operationalization,
    'What numeric and epidemiological thresholds define ''high threat severity'' sufficient to justify mandate under proportionality assessment?',
    'Establish consensus thresholds ex-ante: case fatality rate > X%, transmission rate > Y per-contact, healthcare surge > Z% ICU capacity. Compare jurisdictions with different numeric thresholds and track outcomes.',
    'Without numeric thresholds, proportionality assessment is theater: any government claims the threat is ''severe enough'' for their current mandate level. Thresholds constrain authority discretion and make the proportionality reading falsifiable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threat_severity_operationalization, empirical, 'Proportionality''s core axis is threat severity. Operationalization determines whether assessment is honest or performative.').

omega_variable(
    alternatives_development_commitment,
    'Is public health authority structurally committed to developing therapeutics, testing, and alternative protective measures as exits from mandate, or does mandate reliance create perverse incentive to suppress alternatives?',
    'Compare R&D investment: therapeutic development vs. vaccine development over the crisis arc. Track decisions to restrict antiviral supply, slow EUA approvals for alternatives, or de-fund non-vaccine research.',
    'The ''alternatives available'' dimension is the most manipulable component of proportionality. If alternatives can be suppressed, proportionality becomes a permission structure for perpetual mandate. Structural commitment to alternatives development is necessary condition for proportionality reading to constrain authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternatives_development_commitment, empirical, 'Whether authority genuinely pursues alternatives or uses mandate to eliminate alternatives.').

omega_variable(
    duration_limit_automaticity,
    'Must the proportionality reading include an automatic sunset that lifts mandate when threat falls below threshold, or can duration be left to discretionary authority review?',
    'Statutory language: ''mandate automatically expires when CFR drops below X% for 4 weeks'' (automatic) vs. ''authority may extend mandate if threat remains elevated, subject to legislative review'' (discretionary).',
    'Without automatic sunset, discretionary authority has incentive to misrepresent threat data to preserve mandate. Automatic sunset forces honest reassessment and prevents theater escalation into endemic state. This is the decisive test: does proportionality reading actually constrain authority, or does it become a rhetorical justification for indefinite expansion?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duration_limit_automaticity, conceptual, 'Whether proportionality reading requires hard duration limits or permits indefinite discretionary extension.').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression of mandate refusal (content moderation, employment exclusion, social ostracism) structural (external barriers) or internalized (refusers have been culturally persuaded that refusal is immoral)?',
    'Post-mandate surveys of refuser populations: if suppression is removed, do they maintain refusal based on conviction, or did refusal depend on the suppression itself (a sign of internalization)? Compare with pre-mandate surveys.',
    'Suppression can be structural (authorities enforce mandate) or internalized (refusers believe refusal is wrong even absent enforcement). Under proportionality reading, structural suppression ends when proportionality conditions end; internalized suppression persists. If suppression is internalized, the constraint persists as Piton even after proportionality conditions invert.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether resistance to post-mandate mandate persistence is structural refusal or internalized self-suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__proportionality_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__proportionality_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__proportionality_reading, theater_ratio, 18, 0.48).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__proportionality_reading, theater_ratio, 24, 0.58).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__proportionality_reading, theater_ratio, 30, 0.68).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__proportionality_reading, theater_ratio, 36, 0.71).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__proportionality_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__proportionality_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__proportionality_reading, base_extractiveness, 18, 0.56).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__proportionality_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__proportionality_reading, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__proportionality_reading, base_extractiveness, 36, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__proportionality_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__proportionality_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__proportionality_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__proportionality_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__proportionality_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__proportionality_reading, suppression_requirement, 36, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__public_health_primary).

% DUAL FORMULATION NOTE:
% The public_health_mandate_authority kernel has been decomposed into three distinct constraint stories per the ε-invariance principle (OQ-258, OQ-26). The three readings (bodily_autonomy_primary, proportionality_reading, public_health_primary) are not measurements of the same constraint from different angles — they are structurally distinct constraints because their ε values (extractiveness) differ by a wide margin depending on threat level and alternative availability. Bodily_autonomy_primary treats any non-consensual medical intervention as extraction (high ε regardless of threat). Public_health_primary treats any failure to mandate as extraction (low ε because the mandate solves the coordination problem). Proportionality_reading has dynamic ε: high when threat is severe and alternatives scarce (legitimate tangled_rope), low when threat is endemic and alternatives abundant (illegitimate snare). These are not the same constraint viewed from different seats — they are genuinely different structural arrangements with different victim sets, beneficiary structures, and legitimacy conditions. The network links them to track how changes in one reading's status (e.g., empirical demonstration that a threat is endemic) affects the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
