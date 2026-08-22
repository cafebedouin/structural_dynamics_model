% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Scaffolded Dress-Reform Mandate (Hybrid Ideological-Elite Displacement Reading)
 *   domain: political/cultural
 *
 * SUMMARY:
 *   This story instantiates the 'hybrid scaffolding' reading of a contested
 *   kernel about how imposed cultural practice acquires legitimacy. Unlike a
 *   calendar reform imposed by pure decree (which failed, because no
 *   scaffolding infrastructure or ideological reinforcement accompanied it)
 *   and unlike a slow endogenous cultural drift (which would have taken
 *   generations), the dress-reform campaign paired legal mandate with
 *   sustained ideological messaging and elite modeling, concentrated in
 *   cities where the state built the schools, civil-service pathways, and
 *   media apparatus that made the new practice self-reinforcing. The result
 *   is neither pure imposition nor pure organic adoption: partial,
 *   geographically stratified displacement, with genuine quasi-endogenous
 *   uptake among the scaffolded urban population and continued experienced
 *   coercion among the unscaffolded rural population.
 *
 * KEY AGENTS:
 *   - state_modernization_apparatus: agenda_setter, builds and administers the scaffolding
 *   - urban_westernizing_elites: beneficiary, captures status/career gains from the scaffolded practice
 *   - rural_populations_excluded_from_scaffolding: payer, bears mandate cost without scaffolding benefit
 *   - traditional_dress_artisans: payer, loses livelihood with no compensating pathway
 *   - provincial_enforcement_officials: agenda_setter/payer, forced into coercive enforcement without the ideological tools cities have
 *   - state_historians_and_commentators: observer, comparative genealogist of the calendar/dress divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.62).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Scaffolded Dress-Reform Mandate (Hybrid Ideological-Elite Displacement Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political/cultural").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'afae775b-8aef-454d-bf2e-3f1fb1553aee').
narrative_ontology:cs_kernel_codification('afae775b-8aef-454d-bf2e-3f1fb1553aee', distributed).
narrative_ontology:cs_authority_grounding('afae775b-8aef-454d-bf2e-3f1fb1553aee', extraction).
narrative_ontology:cs_interpretation_layer_present('afae775b-8aef-454d-bf2e-3f1fb1553aee').
narrative_ontology:cs_reading_relation('afae775b-8aef-454d-bf2e-3f1fb1553aee', legitimacy_of_imposed_practice__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('afae775b-8aef-454d-bf2e-3f1fb1553aee', legitimacy_of_imposed_practice__endogenous_climb_reading, influences).
narrative_ontology:cs_axiom('afae775b-8aef-454d-bf2e-3f1fb1553aee', foundational, ideological_scaffolding_necessary_for_durable_displacement).
narrative_ontology:cs_axiom_status(ideological_scaffolding_necessary_for_durable_displacement, holdable).
narrative_ontology:cs_axiom_grounding('afae775b-8aef-454d-bf2e-3f1fb1553aee', ideological_scaffolding_necessary_for_durable_displacement, empirically_contingent).
narrative_ontology:cs_axiom('afae775b-8aef-454d-bf2e-3f1fb1553aee', secondary, partial_geographic_displacement_is_the_expected_outcome_not_failure).
narrative_ontology:cs_axiom_status(partial_geographic_displacement_is_the_expected_outcome_not_failure, holdable).
narrative_ontology:cs_axiom_grounding('afae775b-8aef-454d-bf2e-3f1fb1553aee', partial_geographic_displacement_is_the_expected_outcome_not_failure, empirically_contingent).
narrative_ontology:cs_reference_frame('afae775b-8aef-454d-bf2e-3f1fb1553aee', traditional_dress_as_default_identity_marker).
narrative_ontology:cs_drift_state('afae775b-8aef-454d-bf2e-3f1fb1553aee', post_scaffolding_maturity, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('afae775b-8aef-454d-bf2e-3f1fb1553aee', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernizing_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_modernization_apparatus).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations_excluded_from_scaffolding).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_dress_artisans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, provincial_enforcement_officials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the dress mandate and pairs it with sustained ideological messaging — schooling, print media, public ceremony, model officials — that frames the new dress as identity rather than mere compliance. Administers the scaffolding infrastructure (urban schools, civil-service dress codes, media access) that makes uptake self-sustaining in cities while never extending comparable infrastructure to the countryside.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_modernization_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Early adopters embedded in the scaffolding — civil servants, professionals, urban families with access to schools and media that model the new dress as prestige marker. Their compliance is reinforced by genuine social and career advantage, so the mandate reads to them as an opportunity rather than an imposition; hybrid practices (partial adoption, code-switching by context) let them capture status gains while managing cost.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernizing_elites, beneficiary,
    powerful, biographical, arbitrage, national).

% Subject to the same legal mandate and periodic enforcement sweeps (checkpoints, fines, public shaming) but without the schools, media access, or civil-service pathways that make the practice self-reinforcing in cities. Bear the cost of compliance (new clothing, loss of traditional garment industries, social disruption) without the status payoff that makes it feel chosen. For them the mandate remains experienced as pure decree despite the state's ideological framing.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations_excluded_from_scaffolding, payer,
    powerless, generational, trapped, regional).

% Lose livelihood as traditional garment production is delegitimized alongside the practice itself; unlike rural wearers they have no plausible route into the new economy the mandate rewards, since urban tailoring and imported textile trades are already captured by elites.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_dress_artisans, payer,
    powerless, biographical, trapped, local).

% Tasked with enforcing the mandate in areas where the ideological scaffolding has not taken hold, meaning they must rely on coercion (fines, confiscation, public humiliation) rather than persuasion. Bear career risk if enforcement provokes visible unrest, and bear the resentment the state's messaging apparatus does not have to absorb directly.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, provincial_enforcement_officials, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, provincial_enforcement_officials, payer).

% Assess decades later why dress reform partially succeeded while calendar reform failed outright, comparing scaffolding presence, ideological investment, and urban/rural infrastructure gaps across the two campaigns.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_historians_and_commentators, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernizing_elites).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine state-building coordination problem: a visible, common marker of modern national identity that signals civilizational realignment to external and internal audiences, and that (for those inside the scaffolding) provides real access to status, employment, and institutional participation.
% TRANSFER_FUNCTION: Moves social status, career access, and cultural legitimacy toward urban populations positioned inside state-built scaffolding, while moving economic loss (garment trades), compliance cost, and enforcement burden onto rural populations and traditional artisans who receive none of the compensating infrastructure.
% ABSENT_VOICES: Rural households and artisans who bear the mandate's costs without its scaffolding are not represented in the ideological messaging apparatus (schools, press, ceremonial modeling) and have no organized channel to contest either the mandate or its uneven application; their objection would be that the 'choice' the state describes was never available to them.
% DISAPPEARANCE_RATIONALE: In urban centers the practice has become sufficiently self-sustaining (status-linked, career-linked, generationally transmitted) that removing the mandate would likely leave adoption largely intact — the scaffolding did its job. In rural areas removal would likely trigger rapid reversion, since compliance there was never internalized, only imposed; the two populations would experience disappearance completely differently, which is why the verdict is contested rather than settled either way.
% FOUNDING_PROBLEM: The state needed a rapid, visible marker of civilizational realignment (toward a Western-coded modernity) to secure diplomatic legitimacy and to justify sweeping institutional reform, at a pace pure cultural evolution could not match.
% FOUNDING_PROBLEM_CORROBORATION: State modernization apparatus and urban elite beneficiaries attest the founding problem is resolved and the practice now reflects genuine national identity. Independent historians and rural-origin commentators, corroborated by uneven enforcement records and the persistence of hybrid/reversion practices outside scaffolded zones, attest the founding problem was never resolved for the excluded population — only displaced onto them as unremunerated cost.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high and rises over the interval as the urban/rural gap in outcomes hardens: early on the practice looks like pure imposition everywhere (theater_ratio starts high at 0.6, since compliance is mostly performative under decree pressure), but as scaffolding matures in cities the practice becomes genuinely internalized there while remaining coercively enforced in the countryside — so theater_ratio falls as urban theater becomes real practice, even as overall extraction rises because the gap between beneficiary and payer experience widens. Suppression starts very high (0.78, echoing the failed pure-decree phase) and falls as ideological reinforcement substitutes for raw coercion in scaffolded zones, but plateaus rather than continuing to fall because rural enforcement never stops needing force.
 *
 * PERSPECTIVAL GAP:
 *   From the urban elite seat this looks like successful, largely self-sustaining cultural modernization — a rope. From the rural payer seat it remains what pure decree always was: extraction backed by force, now wearing a modernization narrative it was never given access to. The tangled_rope classification captures both facts simultaneously — genuine coordination function (national identity signaling, career/institutional access) coexisting with asymmetric extraction (cost imposed on populations excluded from the very scaffolding that made the coordination function real for others).
 *
 * DIRECTIONALITY LOGIC:
 *   Urban elites derive low d: the scaffolding converts what began as external mandate into something closer to self-interested adoption, so their effective extraction is damped toward subsidy. Rural populations and artisans derive high d: they face the same legal mandate and enforcement apparatus without any of the compensating channels, so the same nominal rule extracts far more from them — same law, opposite structural position. Provincial enforcement officials sit in an unusual dual position: agents of the constraint who are also partially burdened by its uneven design, which is why they carry both agenda_setter and payer roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid-scaffolding reading matters for mandatrophy because it refuses to treat 'the mandate succeeded' and 'the mandate is now costless coordination' as equivalent claims. The founding problem (rapid, legible civilizational realignment) may be functionally dead for urban elites, for whom the practice has become genuinely endogenous — but it remains a live, unresolved imposition for rural populations who never received the internalization pathway. Collapsing these into a single verdict (either 'this succeeded' or 'this is pure extraction') would erase the geographic asymmetry that is the actual structure; tangled_rope with two divergent seat-level experiences is the accurate reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffolding_as_natural_ideological_convergence,
    'Is the urban population''s internalization of the new practice genuine ideological convergence (an authentic shift in preference and identity) or is it sophisticated compliance shaped entirely by the material incentives the state built into the scaffolding — i.e., is ''quasi-endogenous pull'' actually endogenous, or is it extraction so well-designed it no longer feels coercive?',
    'Track post-mandate persistence: if urban adoption survives removal of career/status incentives tied to dress, that supports genuine internalization; if adoption collapses once incentives are withdrawn, the ''quasi-endogenous'' pull was disguised extraction the whole time.',
    'If internalization is genuine, the urban seat''s tangled_rope classification tilts toward rope (real coordination benefit dominates); if it is disguised incentive-compliance, the whole constraint tilts toward snare with a more effective, better-hidden enforcement mechanism than the rural coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_as_natural_ideological_convergence, conceptual, 'Whether urban quasi-endogenous adoption is authentic conviction or sophisticated incentive-driven compliance.').

omega_variable(
    scaffolding_extension_feasibility,
    'Could the state have extended the scaffolding infrastructure (schools, media, civil-service pathways) to rural areas at reasonable cost, or was urban concentration a structural necessity given resource constraints?',
    'Comparative fiscal and infrastructure analysis of contemporaneous rural development spending versus the cost of the urban modernization apparatus; compare with cases where rural scaffolding was attempted.',
    'If extension was feasible and simply not prioritized, the urban/rural asymmetry reflects a genuine extractive choice (concentrating state investment on those who could reciprocate with elite loyalty) rather than an unavoidable resource limit, strengthening the victim designation for rural populations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffolding_extension_feasibility, empirical, 'Whether the urban/rural scaffolding gap was a resource necessity or a choice concentrating investment on politically useful populations.').

omega_variable(
    kernel_framing_choice,
    'Is the hybrid-scaffolding framing itself the most defensible reading of this history, or does the calendar-versus-dress contrast actually better support the exogenous_override_reading''s claim that decree succeeds wherever backed by sufficient enforcement duration, with ''scaffolding'' merely relabeling sustained enforcement plus time?',
    'Compare enforcement duration and intensity curves for calendar reform versus dress reform controlling for scaffolding presence; if calendar reform given equal enforcement duration would also have succeeded, scaffolding may be redundant with persistence rather than a distinct causal ingredient.',
    'If scaffolding reduces to persistence, this reading may collapse into the exogenous_override_reading and the sibling network relationship should shift from coexists_with toward a stronger influences or even partial foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether scaffolding is a distinct causal mechanism or a relabeling of sustained enforcement duration, which would affect the sibling relation to exogenous_override_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 8, 0.52).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 16, 0.46).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.1).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the legitimacy_of_imposed_practice kernel, differentiated by which mechanism they claim is necessary and sufficient for displacement: exogenous_override_reading (decree authority alone), endogenous_climb_reading (bottom-up internalization alone), and this hybrid_scaffolding_reading (decree plus ideological scaffolding producing quasi-endogenous pull). Each reading is evaluated against different empirical cases within the same historical episode: the calendar reform (unscaffolded decree, failed — evidence against exogenous_override_reading and for this reading's scaffolding requirement) and the dress reform (scaffolded decree, partial success — the central case for this reading). ε differs across the three: this reading's ε (0.58) reflects genuine partial coordination success blended with persistent rural extraction, distinct from what a pure-override reading's ε would show (higher, since it would treat all compliance as coerced) or a pure-climb reading's ε would show (lower, since it would treat the outcome as failed imposition where it wasn't organic).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
