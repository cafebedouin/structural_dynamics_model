% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_stone_directive__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone Directive as Commemorative Husk
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi Stone stands in a coastal village in Japan, inscribed with a
 *   directive interpreted as a tsunami warning. The commemorative husk
 *   reading frames the stone as having lost its behavioral force as a
 *   land-use constraint during the 66-year inter-catastrophe period
 *   (1945–2011) between the 1933 tsunami and the 2011 Tōhoku event. Under
 *   this reading, the directive persisted as a cultural memorial and ritual
 *   object, but economic development pressures gradually overwhelmed any
 *   constraining influence it might have retained. The stone became a symbol
 *   of disaster memory—valued for its historical meaning—rather than an
 *   operative rule governing settlement patterns. Development interests
 *   benefited from this reading because it allowed framing land-use
 *   permission as respecting the stone's commemorative importance while
 *   treating its directive function as atrophied. The constraint story models
 *   this reading as a piton: the stone's performative maintenance (annual
 *   ceremonies, scholarly attention) persisted while its behavioral grip on
 *   development decisions weakened and eventually disappeared.
 *
 * KEY AGENTS:
 *   - Aneyoshi community: ritual custodians, structurally powerless but identity-locked to the stone's preservation
 *   - Coastal development interests: powerful beneficiaries of the directive's loss of behavioral force; actively promote 'commemorative husk' framing
 *   - Preservation and heritage advocates: organized payers defending the directive's binding authority through scholarship and litigation
 *   - Municipal governance bodies: agenda-setters managing land-use decisions under conditions of interpretive uncertainty
 *   - Disaster-validation cycle (absent actor): inter-catastrophe silence enabled the reading shift; 2011 vindicated the opposing reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, 0.82).
domain_priors:suppression_score(aneyoshi_stone_directive__commemorative_husk_reading, 0.71).
domain_priors:theater_ratio(aneyoshi_stone_directive__commemorative_husk_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_directive__commemorative_husk_reading, "Aneyoshi Stone Directive as Commemorative Husk").
narrative_ontology:topic_domain(aneyoshi_stone_directive__commemorative_husk_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:requires_active_enforcement(aneyoshi_stone_directive__commemorative_husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__commemorative_husk_reading, 'f2bd5f9e-eb5c-43d4-831f-eda455d36744').
narrative_ontology:cs_kernel_codification('f2bd5f9e-eb5c-43d4-831f-eda455d36744', fixed_text).
narrative_ontology:cs_authority_grounding('f2bd5f9e-eb5c-43d4-831f-eda455d36744', extraction).
narrative_ontology:cs_interpretation_layer_present('f2bd5f9e-eb5c-43d4-831f-eda455d36744').
narrative_ontology:cs_reading_relation('f2bd5f9e-eb5c-43d4-831f-eda455d36744', aneyoshi_stone_directive__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('f2bd5f9e-eb5c-43d4-831f-eda455d36744', foundational, memorial_transcends_mandate).
narrative_ontology:cs_axiom_status(memorial_transcends_mandate, holdable).
narrative_ontology:cs_axiom_grounding('f2bd5f9e-eb5c-43d4-831f-eda455d36744', memorial_transcends_mandate, conventional).
narrative_ontology:cs_axiom('f2bd5f9e-eb5c-43d4-831f-eda455d36744', foundational, inter_catastrophe_silence_exhausts_force).
narrative_ontology:cs_axiom_status(inter_catastrophe_silence_exhausts_force, holdable).
narrative_ontology:cs_axiom_grounding('f2bd5f9e-eb5c-43d4-831f-eda455d36744', inter_catastrophe_silence_exhausts_force, empirically_contingent).
narrative_ontology:cs_reference_frame('f2bd5f9e-eb5c-43d4-831f-eda455d36744', disaster_memory_as_living_behavioral_constraint).
narrative_ontology:cs_drift_state('f2bd5f9e-eb5c-43d4-831f-eda455d36744', post_inter_catastrophe_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f2bd5f9e-eb5c-43d4-831f-eda455d36744', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, preservation_heritage_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, municipal_governance_bodies).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__commemorative_husk_reading, memorial_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__commemorative_husk_reading, disaster_memorial_transcendence_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The village where the stone stands. They maintain the stone and perform yearly commemoration ceremonies. They have historically interpreted the stone's directive as binding memory-work—a solemn obligation to remember the 1896 tsunami and guard against coastal overbuilding. During inter-catastrophe periods (years when no major disaster validated the directive), the community faced mounting pressure to allow development; compliance with the stone's implicit zone restriction meant foregoing economic opportunity. They remain the ritual custodians but lack the authority to prevent development. Identity-locked: they cannot exit the stone's framework without abandoning their cultural role and place identity.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_community, observer,
    powerless, generational, identity_locked, local).

% Real estate developers, municipal revenue interests, and agricultural land-use seekers who benefit from the stone directive's decay as a behavioral constraint. The reading frames the stone as having lost enforcement force during inter-catastrophe periods—years between major disasters when the directive's behavioral grip weakened. They argue the directive is now merely symbolic, a memorial, no longer a binding land-use rule. They actively frame the stone as 'commemorative'—acknowledging its historical importance while asserting it does not mandate development restriction in contemporary practice. They have arbitrage: if the husk reading is accepted, they can exit the constraint's zone and develop; if the competence reading prevails, they can shift development to other regions.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests, beneficiary,
    powerful, biographical, arbitrage, national).

% Organizations and scholars who argue the stone directive retains binding force as cultural authority—a form of knowledge transmission embedded in landscape. They oppose development in the restricted zone, citing the stone's historical role in preventing loss of life in the 1933 tsunami. They are payers in the sense that their advocacy, litigation, and preservation efforts carry costs (legal resources, administrative burden) that would be unnecessary if the development interests prevailed in reframing the directive as 'merely' commemorative. They have constrained exit: they cannot simply abandon the land's protection claim without ceding the entire dispute to development interests.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, preservation_heritage_advocates, payer,
    organized, generational, constrained, national).

% Local government officials who administer land-use decisions. They face competing pressures: community tradition and preservation advocates invoke the stone's authority; development interests invoke economic necessity and the 'commemorative husk' framing. Officials are dual-positioned: as agenda-setters they make or approve development decisions, but as payers they bear the cost of public liability if they permit development that is later struck by tsunami. Their constrained exit reflects the genuine uncertainty—they cannot simply rule the stone binding or non-binding without evidence; the inter-catastrophe period leaves the directive's status ambiguous.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, municipal_governance_bodies, agenda_setter,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, municipal_governance_bodies, payer).

% Academic observers studying institutional memory, disaster anthropology, and landscape governance. They document how the stone's directive retains cultural legitimacy while its behavioral force decayed during inter-catastrophe periods. They identify the reading shift (from binding directive to commemorative memorial) as a structural outcome of disaster absence and development pressure—not as a discovered truth about what the stone 'really' was, but as an interpretation that gained authority when institutional conditions changed.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, anthropological_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone directive originally solved a disaster-memory problem: how to transmit tsunami-avoidance knowledge across generations and enforce land-use restraint without relying on formal written law or governmental capacity. The stone's standing location and cryptic inscription embedded the warning in the landscape, making it visible and memorable. Periodic tsunami validation kept the directive behaviorally potent. Under the commemorative husk reading, this coordination function has atrophied; the stone now coordinates only ceremonial remembrance of the 1933 disaster, not behavioral restraint on settlement.
% TRANSFER_FUNCTION: The constraint transfers economic opportunity from coastal development interests to preservation/memorial maintenance. It moves land-use authority from market-logic (highest-value use) to disaster-memory logic (precautionary restraint). During inter-catastrophe periods, the constraint also transfers legitimacy from the directive's behavioral force (weakened by absence of validation) to its commemorative and historical significance (strengthened by distance from living memory of disaster). The husk reading allows development interests to claim the stone as cultural heritage while bypassing its regulatory implications.
% ABSENT_VOICES: Coastal residents most vulnerable to tsunami who would benefit from precautionary land-use restraint are unorganized and cannot participate in the reading contest. Future disaster victims—whose interests are represented only through the stone's abstract directive—are not present in contemporary decision-making. Indigenous and local knowledge-holders whose original relationship to the landscape included oral disaster traditions are largely absent from formalized preservation and development debates, replaced by academic and governmental proxies. The voices of communities in other disaster-vulnerable regions who maintain similar memorial-directive systems are also absent.
% DISAPPEARANCE_RATIONALE: Development interests assert the world would rearrange toward economic optimization of coastal land if the directive's behavioral force vanished—more housing, aquaculture, tourism. But under the commemorative husk reading, this vanishing has already occurred during the inter-catastrophe period, so disappearance today would formalize what is already true. Preservation advocates counter that the directive's force persists through the stone's standing presence and that its disappearance (destruction or removal) would eliminate a crucial mechanism for transmitting disaster knowledge. The contention is whether the directive ever actually constrained behavior during recent decades, or merely appeared to do so through commemorative observance while development pressures already marginalized its enforcement.
% FOUNDING_PROBLEM: In 1896, a catastrophic tsunami struck the Japanese coast near Aneyoshi, killing thousands. The community faced the problem of transmitting disaster-avoidance knowledge to future generations in a form that would survive without institutional infrastructure—knowledge that could persist in the landscape and motivate precautionary behavior even when living memory of the disaster faded. The stone directive (allegedly inscribed with a warning to build no houses below this stone) was the solution: a permanent, visible, symbolically authoritative marker that would endure and remind.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem—ensuring tsunami-avoidance knowledge survives across generations in the absence of institutional record-keeping—was real and well-attested by historical documentation and survivor accounts (corroborated by disaster historians and anthropologists outside the preservationist advocacy community). However, the status of that problem as *currently live* is contested. Development interests and some municipal officials argue the founding problem is now solved by modern warning systems, building codes, and institutional memory; it no longer requires the stone's behavioral force. The 2011 Tōhoku tsunami vindicated the opposing reading (behavioral_competence_reading), demonstrating the stone's predictive accuracy and forcing reassessment of whether the founding problem was truly dead. This reading (commemorative husk) takes the position that the founding problem, though real historically, was effectively dead in behavioral terms during the inter-catastrophe period—the stone persisted as memorial precisely because its original problem-solving function had atrophied.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's extractiveness rises from 0.38 (1945, immediately post-war when the stone's directive was recently reinforced by 1933 memory) to 0.82 (2011, after 66 years of inter-catastrophe silence and accumulated development pressure). Theater ratio climbs from 0.22 to 0.68, indicating the stone's function shifted from behavioral constraint toward performative commemoration. The coercion grid shows resistance declining at every level as the directive's binding force decayed—individual-level resistance (community members feeling obligated to honor the stone) fell from 0.52 to 0.45; organizational and class-level resistance followed similar trajectories. Simultaneously, suppression requirement rose, because maintaining the 'commemorative husk' reading required active cognitive work—reframing the stone's authority away from behavioral force and toward symbolic value. Suppression also captures the institutional effort to prevent preservation advocacy from reverting the directive's status. The measurements track the inter-catastrophe period: the directive weakened not from any change in its physical presence, but from the absence of validating disaster and the accumulation of economic opportunity cost. Suppression_requirement rises sharply 1975–1990, the period of most intense development pressure and most active repositioning of the stone as 'memorial' rather than 'rule.'
 *
 * PERSPECTIVAL GAP:
 *   The development-interest seat experiences the constraint as theatrical memorial observance that imposes diminishing actual cost on their economic plans. The community seat experiences it as a binding moral obligation whose weakening is a form of cultural loss. The heritage advocate seat experiences it as active suppression of the stone's original authority through rhetorical reframing. The municipal seat experiences it as ambiguity in the inter-catastrophe period that permits development while preserving plausible compliance with the stone's historical importance. The engine's per-seat computation would show: development interests drift toward near-beneficiary directionality (d → 0.1–0.2) as the directive's behavioral force vanishes; heritage advocates remain pinned near target (d → 0.8–0.9) because the suppression of their reading requires active resistance; community remains trapped (d → 0.7) by identity-lock to the stone even as its behavioral force atrophies.
 *
 * DIRECTIONALITY LOGIC:
 *   Development interests benefit from the directive's loss of behavioral force—they are the structural beneficiaries of the 'commemorative husk' reading. Heritage advocates bear the cost of defending the opposing reading against institutional pressure to accept the husk framing; they are victims in the sense that their preferred interpretation is suppressed, though no direct economic transfer occurs. The community occupies a paradoxical position: they are nominally the stone's custodians (suggesting beneficiary status) but actually the victims of its weakening, because they lose the directive's structural support for coastal restraint. Their situation exemplifies identity-lock: they cannot simply exit the stone's authority framework without abandoning their cultural role, even as the framework's behavioral force decays. Municipal officials are quasi-payers: they must bear the liability and reputation cost of permitting development that might later prove disaster-vulnerable, and they must mediate the interpretive contest without authoritative resolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The commemorative husk reading resolves an apparent mandatrophy by reframing it. A literal reading would suggest the mandate to 'warn against coastal building' is now obsolete (founding problem solved by modern systems) but persists through institutional inertia—classic piton mandatrophy. The husk reading avoids that diagnosis by arguing the mandate never persisted as behavioral force in the inter-catastrophe period; it persisted as *memorial*, which is a different mandate entirely. The memorial mandate (preserve and commemorate the 1933 disaster) remains live. This move is analytically elegant but empirically contestable: it requires treating the behavioral and commemorative dimensions as separable, whereas the behavioral_competence_reading treats them as aspects of a single continuous directive. The mandatrophy is not resolved—it is reframed out of existence through a reading shift. The coercion grid and measurement trajectory show the husk reading's dependence on suppression (rising suppression requirement even as theater rises) to maintain the reframing against the opposing reading's claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    directive_behavioral_force_discontinuity,
    'Did the stone directive''s behavioral force exhibit sharp discontinuity during inter-catastrophe periods, or did it decay gradually and remain partially operative throughout?',
    'Archival evidence of land-use decisions, municipal records documenting rejection or approval of coastal development proposals, oral history from community members and officials regarding the directive''s role in their decision-making, archaeological evidence of settlement patterns and construction dates in the restricted zone.',
    'If force remained operative (even weakly), the constraint is tangled_rope with attenuation, not a full piton husk. If force was substantially eliminated by inter-catastrophe silence, the husk reading holds. The binary determines whether suppression (required to hold the reading against the opposing reading) is genuine active enforcement or theatrical maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(directive_behavioral_force_discontinuity, empirical, 'Whether the directive''s behavioral grip decayed as a continuous attenuation or exhibited sharp loss during inter-catastrophe periods.').

omega_variable(
    memorial_vs_mandate_separability,
    'Are the directive''s memorial/commemorative function and its behavioral/regulatory function structurally separable, or are they inseparable aspects of a single land-use authority?',
    'Comparative analysis of similar memorial-directive systems in other disaster-vulnerable regions; ethnographic documentation of how community members articulate the relationship between remembering the 1933 tsunami and deciding whether to permit construction; analysis of whether ''honoring the memorial'' and ''obeying the directive'' were ever consciously distinguished by historical actors.',
    'If separable, the husk reading is correct: the stone can persist as memorial while the directive atrophies. If inseparable, the readings are not truly alternatives but describe the same phenomenon at different temporal scales; the distinction is rhetorical rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_vs_mandate_separability, conceptual, 'Whether memorial and regulatory authority are distinct or unified aspects of the stone''s directive.').

omega_variable(
    reading_shift_timing_and_causation,
    'Was the shift toward the ''commemorative husk'' reading a deliberate, interested reframing by development actors, or an organic outcome of inter-catastrophe silence that development interests later opportunistically leveraged?',
    'Documentary evidence of who first articulated the ''husk'' framing and under what institutional conditions; analysis of whether preservation advocacy against the husk reading is reactive (responding to development opportunism) or proactive (attempting to pre-empt reframing); timeline of when the reading appears in municipal documents, scholarly work, and development proposals.',
    'If deliberate reframing by development actors, the constraint is snare-adjacent (cynical cover story). If organic outcome of disaster absence, the constraint exhibits genuine structural attenuation. The distinction affects classification of suppression: is it maintenance of a plausible reading, or is it suppression of a true competing reading in bad faith?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_shift_timing_and_causation, empirical, 'Whether the husk reading was deliberately constructed or organically emerged from inter-catastrophe conditions.').

omega_variable(
    kernel_reading_contest_foreclosure_risk,
    'Is the contest between commemorative_husk_reading and behavioral_competence_reading genuinely open (both remain live positions), or does one reading foreclose the other—rendering the contest merely a temporary framing battle before inevitable resolution?',
    'Monitoring of future disaster-validation events (tsunamis that either vindicate the directive or demonstrate its irrelevance); analysis of whether the 2011 Tōhoku tsunami genuinely validates the behavioral_competence_reading or whether the husk reading accommodates it through post-hoc reinterpretation.',
    'If foreclosure is genuine (one reading logically rules out the other if sufficiently validated), the readings are structural rivals and the kernel contest has built-in resolution conditions. If both readings can accommodate future evidence (husk reading explains disasters as vindication of the memorial''s historical importance; competence reading explains them as evidence of the directive''s persistence), the contest is perpetual and suppression of one reading may be institutionally permanent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_foreclosure_risk, conceptual, 'Whether the kernel contest contains foreclosure conditions or is potentially perpetual.').

omega_variable(
    inter_catastrophe_silence_counterfactual,
    'Would the directive have maintained behavioral force if catastrophes had struck more frequently (10-year cycle instead of 66-year gap), or is inter-catastrophe silence merely one factor in a broader attenuation process that would have occurred regardless?',
    'Comparative analysis of disaster-memory directives in regions with high frequency of destructive events (Japan''s volcanic zones, Pacific Ring of Fire settlements); modeling of institutional memory decay rates independent of validation cycles; ethnographic analysis of factors preserving behavioral force in high-frequency-disaster zones.',
    'If silence is the primary driver, the husk reading points to a genuine structural vulnerability in disaster-memory systems—attenuation is expected and perhaps inevitable without periodic reinforcement. If broader factors dominate, the inter-catastrophe period is less explanatory and the development-interest narrative gains credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_catastrophe_silence_counterfactual, empirical, 'Whether inter-catastrophe silence is the primary driver of the directive''s behavioral attenuation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__commemorative_husk_reading, 1945, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1945, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1945, 0.22).
narrative_ontology:measurement_basis(aney_tr_t1945, observed).
narrative_ontology:measurement(aney_tr_t1960, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1960, 0.28).
narrative_ontology:measurement_basis(aney_tr_t1960, observed).
narrative_ontology:measurement(aney_tr_t1975, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1975, 0.39).
narrative_ontology:measurement_basis(aney_tr_t1975, observed).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1990, 0.52).
narrative_ontology:measurement_basis(aney_tr_t1990, observed).
narrative_ontology:measurement(aney_tr_t2005, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 2005, 0.63).
narrative_ontology:measurement_basis(aney_tr_t2005, observed).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 2011, 0.68).
narrative_ontology:measurement_basis(aney_tr_t2011, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t1945, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1945, 0.38).
narrative_ontology:measurement_basis(aney_be_t1945, observed).
narrative_ontology:measurement(aney_be_t1960, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement_basis(aney_be_t1960, observed).
narrative_ontology:measurement(aney_be_t1975, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement_basis(aney_be_t1975, observed).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1990, 0.71).
narrative_ontology:measurement_basis(aney_be_t1990, observed).
narrative_ontology:measurement(aney_be_t2005, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 2005, 0.78).
narrative_ontology:measurement_basis(aney_be_t2005, observed).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 2011, 0.82).
narrative_ontology:measurement_basis(aney_be_t2011, observed).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1945, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement_basis(aney_su_t1945, observed).
narrative_ontology:measurement(aney_su_t1960, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1960, 0.41).
narrative_ontology:measurement_basis(aney_su_t1960, observed).
narrative_ontology:measurement(aney_su_t1975, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1975, 0.48).
narrative_ontology:measurement_basis(aney_su_t1975, observed).
narrative_ontology:measurement(aney_su_t1990, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement_basis(aney_su_t1990, observed).
narrative_ontology:measurement(aney_su_t2005, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement_basis(aney_su_t2005, observed).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 2011, 0.71).
narrative_ontology:measurement_basis(aney_su_t2011, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1945, tn=2011
narrative_ontology:measurement(aney_grid_01, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(class), 1945, 0.38).
narrative_ontology:measurement(aney_grid_02, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(class), 2011, 0.55).
narrative_ontology:measurement(aney_grid_03, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(individual), 1945, 0.42).
narrative_ontology:measurement(aney_grid_04, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(individual), 2011, 0.58).
narrative_ontology:measurement(aney_grid_05, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(organizational), 1945, 0.52).
narrative_ontology:measurement(aney_grid_06, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(organizational), 2011, 0.68).
narrative_ontology:measurement(aney_grid_07, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(structural), 1945, 0.48).
narrative_ontology:measurement(aney_grid_08, aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse(structural), 2011, 0.64).
narrative_ontology:measurement(aney_grid_09, aneyoshi_stone_directive__commemorative_husk_reading, resistance(class), 1945, 0.48).
narrative_ontology:measurement(aney_grid_10, aneyoshi_stone_directive__commemorative_husk_reading, resistance(class), 2011, 0.38).
narrative_ontology:measurement(aney_grid_11, aneyoshi_stone_directive__commemorative_husk_reading, resistance(individual), 1945, 0.52).
narrative_ontology:measurement(aney_grid_12, aneyoshi_stone_directive__commemorative_husk_reading, resistance(individual), 2011, 0.45).
narrative_ontology:measurement(aney_grid_13, aneyoshi_stone_directive__commemorative_husk_reading, resistance(organizational), 1945, 0.58).
narrative_ontology:measurement(aney_grid_14, aneyoshi_stone_directive__commemorative_husk_reading, resistance(organizational), 2011, 0.51).
narrative_ontology:measurement(aney_grid_15, aneyoshi_stone_directive__commemorative_husk_reading, resistance(structural), 1945, 0.55).
narrative_ontology:measurement(aney_grid_16, aneyoshi_stone_directive__commemorative_husk_reading, resistance(structural), 2011, 0.42).
narrative_ontology:measurement(aney_grid_17, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(class), 1945, 0.28).
narrative_ontology:measurement(aney_grid_18, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(class), 2011, 0.51).
narrative_ontology:measurement(aney_grid_19, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(individual), 1945, 0.22).
narrative_ontology:measurement(aney_grid_20, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(individual), 2011, 0.45).
narrative_ontology:measurement(aney_grid_21, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(organizational), 1945, 0.32).
narrative_ontology:measurement(aney_grid_22, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(organizational), 2011, 0.58).
narrative_ontology:measurement(aney_grid_23, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(structural), 1945, 0.35).
narrative_ontology:measurement(aney_grid_24, aneyoshi_stone_directive__commemorative_husk_reading, stakes_inflation(structural), 2011, 0.62).
narrative_ontology:measurement(aney_grid_25, aneyoshi_stone_directive__commemorative_husk_reading, suppression(class), 1945, 0.28).
narrative_ontology:measurement(aney_grid_26, aneyoshi_stone_directive__commemorative_husk_reading, suppression(class), 2011, 0.62).
narrative_ontology:measurement(aney_grid_27, aneyoshi_stone_directive__commemorative_husk_reading, suppression(individual), 1945, 0.32).
narrative_ontology:measurement(aney_grid_28, aneyoshi_stone_directive__commemorative_husk_reading, suppression(individual), 2011, 0.65).
narrative_ontology:measurement(aney_grid_29, aneyoshi_stone_directive__commemorative_husk_reading, suppression(organizational), 1945, 0.35).
narrative_ontology:measurement(aney_grid_30, aneyoshi_stone_directive__commemorative_husk_reading, suppression(organizational), 2011, 0.68).
narrative_ontology:measurement(aney_grid_31, aneyoshi_stone_directive__commemorative_husk_reading, suppression(structural), 1945, 0.38).
narrative_ontology:measurement(aney_grid_32, aneyoshi_stone_directive__commemorative_husk_reading, suppression(structural), 2011, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__commemorative_husk_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_directive__commemorative_husk_reading, 0.18).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The Aneyoshi Stone Directive kernel gives rise to two structurally distinct constraints corresponding to competing readings. The commemorative_husk_reading models the directive as a memorial whose behavioral force attenuated during inter-catastrophe periods, generating a piton classification with high extractiveness (development interests benefit from attenuation). The behavioral_competence_reading models the directive as retaining binding authority across the same 66-year period, generating a tangled_rope classification with sustained extraction (heritage advocates must suppress the husk reading to maintain the directive's authority). The two constraints share a kernel (the stone, the directive, the interpretive contest) but instantiate different epsilon values and stakeholder structures. Neither reading is privileged in the constraint framework; each is modeled as a complete, coherent interpretation with its own structural data and classification. The readings coexist as live positions held by different institutional actors; neither logically forecloses the other within a single framework, though each influences the other's operating conditions (suppression requirement rises for the husk reading as the competence reading defends its interpretation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aneyoshi_stone_directive__commemorative_husk_reading, powerless, 0.72).
constraint_indexing:directionality_override(aneyoshi_stone_directive__commemorative_husk_reading, powerful, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
