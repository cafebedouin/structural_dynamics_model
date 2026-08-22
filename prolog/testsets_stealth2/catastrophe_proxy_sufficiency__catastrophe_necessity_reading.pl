% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_necessity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
 *   human_readable: Catastrophe-Necessity Doctrine in Competence Maintenance (kernel reading: catastrophe_proxy_sufficiency / catastrophe_necessity_reading)
 *   domain: safety engineering / organizational learning / high-reliability organizations
 *
 * SUMMARY:
 *   In high-reliability domains — aviation, nuclear power, emergency
 *   medicine, maritime operations, military readiness — a long-standing
 *   doctrine holds that genuine operational competence is maintained only by
 *   actual catastrophic events: the stress, uncertainty, and stakes of a real
 *   emergency are held to be irreducible, so competence decays during
 *   catastrophe-free periods and no simulator can prevent it. This story
 *   instantiates the catastrophe_necessity_reading of the
 *   catastrophe_proxy_sufficiency kernel as one clean, epsilon-invariant
 *   constraint. The epsilon referent is the standing arrangement under
 *   contest — the simulation-reliant competence-maintenance regime that
 *   certifications and readiness ratings rest on — assessed by this reading's
 *   own lights: the regime collects certification credit, readiness
 *   legitimacy, and training budgets against a competence stock it cannot
 *   replenish, and the drawdown is deferred to the day a real event arrives.
 *   The reading's endorsed alternative (a regime organized around real-event
 *   exposure) is NOT the referent. Sibling readings —
 *   simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading,
 *   simulation_fidelity_threshold — are separate constraints with their own
 *   epsilon values and victim sets; they are linked through the network, not
 *   averaged here. The claim/metric gap is deliberate data: the reading
 *   CLAIMS a mountain (a natural limit on substitution, hence
 *   emerges_naturally) while the authored metrics describe a doctrine that is
 *   actively enforced, meets substantial resistance, and concentrates rents
 *   on identifiable seats — the false-summit signature evaluates whether the
 *   naturality framing survives the declared beneficiaries. Temporal series
 *   span the modern simulation era (interval units index that era from the
 *   doctrine's post-war consolidation to the present) and show the
 *   disaster-reform cycle: each real catastrophe vindicates the doctrine's
 *   currency and briefly narrows the overclaim gap; each calm period regrows
 *   it, with the floor ratcheting upward as organizations lean harder on the
 *   proxy the doctrine devalues. Base-properties scalars reflect the
 *   interval-end state.
 *
 * KEY AGENTS:
 *   - safety_certification_regulators: agenda-setting seat (institutional/constrained) — writes the recurrency and experience rules that decide what counts as maintained competence and what simulation may stand in for; cannot require real catastrophes, so administers a proxy it officially holds insufficient
 *   - veteran_real_event_operators: primary beneficiary (powerful/identity_locked) — collects the scarcity premium on real-event experience; their authority, standing, and self-concept are constituted by having been through the real thing
 *   - live_exercise_establishments: beneficiary and co-administrator (institutional/generational) — collects the budgets, facilities, and institutional purpose the doctrine protects; the seat where the material gains accrue
 *   - high_reliability_operating_crews: primary target (organized/constrained) — bears the erosion of stress-response and tacit coordination that certifications assert is not happening
 *   - high_reliability_operating_organizations: target (institutional/constrained) — funds both currencies, collects the certification legitimacy, and carries the liability when the deficit meets a real event
 *   - catastrophe_exposed_public: target (powerless/trapped) — bears the residual casualty risk of degraded competence behind the certifications it relies on
 *   - simulation_training_industry: excluded seat (organized/constrained) — in the market but not in the credentialing conversation; its central claim is what the doctrine forecloses
 *   - competence_research_community: analytical observer (analytical/analytical) — produces the transfer and decay evidence any resolution of the kernel contest would have to rest on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.64).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.56).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.37).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0.37).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "Catastrophe-Necessity Doctrine in Competence Maintenance (kernel reading: catastrophe_proxy_sufficiency / catastrophe_necessity_reading)").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "safety engineering / organizational learning / high-reliability organizations").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).
domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '089b9487-6127-426b-aa57-f106d2e84bf9').
narrative_ontology:cs_kernel_codification('089b9487-6127-426b-aa57-f106d2e84bf9', formalized).
narrative_ontology:cs_authority_grounding('089b9487-6127-426b-aa57-f106d2e84bf9', lineage).
narrative_ontology:cs_interpretation_layer_present('089b9487-6127-426b-aa57-f106d2e84bf9').
narrative_ontology:cs_reading_relation('089b9487-6127-426b-aa57-f106d2e84bf9', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('089b9487-6127-426b-aa57-f106d2e84bf9', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('089b9487-6127-426b-aa57-f106d2e84bf9', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, forecloses).
narrative_ontology:cs_axiom('089b9487-6127-426b-aa57-f106d2e84bf9', foundational, catastrophic_stress_irreducibility).
narrative_ontology:cs_axiom_status(catastrophic_stress_irreducibility, holdable).
narrative_ontology:cs_axiom_grounding('089b9487-6127-426b-aa57-f106d2e84bf9', catastrophic_stress_irreducibility, empirically_contingent).
narrative_ontology:cs_axiom('089b9487-6127-426b-aa57-f106d2e84bf9', secondary, simulation_competence_currency_exclusion).
narrative_ontology:cs_axiom_status(simulation_competence_currency_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('089b9487-6127-426b-aa57-f106d2e84bf9', simulation_competence_currency_exclusion, empirically_contingent).
narrative_ontology:cs_reference_frame('089b9487-6127-426b-aa57-f106d2e84bf9', catastrophe_as_sole_valid_teacher).
narrative_ontology:cs_drift_state('089b9487-6127-426b-aa57-f106d2e84bf9', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('089b9487-6127-426b-aa57-f106d2e84bf9', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, veteran_real_event_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, live_exercise_establishments).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_operating_crews).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_operating_organizations).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_exposed_public).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, stress_inoculation_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, experience_as_only_valid_currency_of_competence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and enforce the certification and recurrency rules that decide what counts toward operational competence in aviation, nuclear, maritime, and emergency-services domains. Their standards specify how much simulator time counts, what must be demonstrated in live exercises, and which real-event qualifications gate promotion to command roles. They cannot require real catastrophes as training, so the rules they can write are shaped by what simulation is allowed to stand in for; exit means leaving the regulatory body, not changing the standard.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, safety_certification_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Senior pilots, reactor operators, incident commanders, and military officers whose authority rests on having been through actual emergencies. Their judgment is consulted because they have seen the real thing; their standing in promotion boards, investigation panels, and training design depends on that experience remaining scarce and unforgeable. Stepping away from the identity of the tested veteran would cost them the standing their careers are built on.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, veteran_real_event_operators, beneficiary,
    powerful, biographical, identity_locked, national).

% Run the large-scale live exercises, drills, and real-world training deployments that the doctrine of real-event necessity protects. Their budgets, facilities, and institutional purpose are justified by the claim that only live exposure maintains readiness; they administer the exercises, publish the readiness assessments, and advocate for the formats that keep them funded.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, live_exercise_establishments, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, live_exercise_establishments, agenda_setter).

% Line pilots, control-room crews, surgical and emergency teams who carry the operational load. Their stress-response sharpness and tacit coordination skills erode during long catastrophe-free stretches, and recurrency training in simulators does not fully restore them, yet their certifications state that they are maintained. Leaving the profession means abandoning the career; they experience the gap between certification and condition directly but have no standing to report it until an event reveals it.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_operating_crews, payer,
    organized, biographical, constrained, national).

% Airlines, nuclear utilities, hospital systems, and military services that operate the safety-critical systems. They fund both simulator fleets and live exercises, collect the readiness ratings, insurance terms, and public-trust benefits that certification confers, and carry the liability when degraded performance meets a real event. Their training investments are allocated between the two currencies by the rules the regulators write; exit from the certification regime is not available to them.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, high_reliability_operating_organizations, payer,
    institutional, generational, constrained, global).

% Passengers, plant neighbors, patients, and communities downstream of high-reliability operations. They rely on the competence the certifications promise; when long catastrophe-free periods have eroded the skills behind those certifications, the deficit surfaces as casualty and loss in the rare event that arrives anyway. They cannot exit the airspace, the grid, or the emergency-response system their lives depend on.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_exposed_public, payer,
    powerless, biographical, trapped, national).

% Build and sell the simulators, scenario engines, and training curricula that carry most routine competence maintenance. Their products are accepted for procedure and drill but categorically devalued for the competencies that matter most; the research program that would establish at what fidelity their products become sufficient is dismissed by the doctrine as a category error. They are in the market but not in the credentialing conversation; their commercial exit would mean abandoning the domain.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_training_industry, excluded,
    organized, biographical, constrained, global).

% Human-factors, organizational-learning, and stress-inoculation researchers who study whether and when simulation transfers to real-event performance. They produce the transfer studies, decay measurements, and fidelity comparisons that any resolution of the doctrine's central claim would have to rest on; they hold no enforcement power and collect no rents from either side.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, competence_research_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws a defensible boundary between demonstrated and merely rehearsed competence, and coordinates trust allocation, certification, and command role assignment around a currency — real-event experience — that cannot be accumulated in a simulator.
% TRANSFER_FUNCTION: Moves certification authority, status, and training budgets toward holders and providers of real-event experience, and moves the deferred cost of un-maintained competence — eroded stress response and tacit coordination — onto operating crews, their organizations, and the public, where it lands when a real event finally arrives.
% ABSENT_VOICES: The simulation-sufficiency research program and the simulation industry are present commercially but excluded from the credentialing conversation — their central claim (that fidelity can close the gap) is precisely what the doctrine forecloses. Crews who experience decay between catastrophes have no standing to report it until an event reveals it; by then the testimony is post-hoc. Younger simulation-trained operator cohorts, whose career currency the doctrine devalues, are heard only as cultural noise rather than as evidence.
% DISAPPEARANCE_RATIONALE: Overnight removal would force certification regimes to re-derive competence standards from simulation-outcome and transfer evidence, compress the veteran experience premium, put live-exercise budgets into open competition with simulator investment, and fund the fidelity-threshold research program the doctrine currently forecloses; readiness ratings would have to state what they actually maintain, and the disaster-reform cycle would lose its legitimating mechanism.
% FOUNDING_PROBLEM: When high-reliability operations formed, real emergencies were the only place competence under genuine stakes could be acquired or proven; the doctrine was built to answer how trust, certification, and command role assignment should be organized around a competence currency that only catastrophe could mint.
% FOUNDING_PROBLEM_CORROBORATION: Accident-investigation bodies and the stress-inoculation and organizational-learning literatures attest, from outside the beneficiary set, that the founding problem was real and that decay during catastrophe-free periods is measurable; the same literatures dispute the doctrine's claim that the problem remains categorically unsolved by simulation. No source outside the beneficiary set attests the categorical version — the irreducibility claim is asserted from within the veteran and live-exercise constituency that the doctrine's operation benefits, and that absence is itself signal.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.64: from this reading's lights the extracted substance is the overclaim — certification and readiness credit collected against a competence stock the doctrine itself says the regime cannot replenish; the gap is real but partial (simulation does maintain procedural skill, which bounds epsilon below the categorical-fraud reading). Suppression 0.56: enforcement runs through recurrency rules, experience requirements, and budget gatekeeping, and the doctrine forecloses the fidelity-threshold research program, but its gatekeeping capacity has eroded as simulator fidelity rose — hence the falling suppression series with event-driven hardening spikes. Theater 0.37: real catastrophes are non-performative by definition, but a growing share of the doctrine's maintenance activity is staged exercise and experiential ritual standing in for the real thing the doctrine says cannot be stood in for. Accessibility_collapse 0.52: the alternative (simulation-based maintenance) persists in practice but is categorically devalued at the credentialing layer, so alternatives are half-collapsed, not natural-law-collapsed. Resistance 0.60: the simulation industry, the transfer-research program, and younger operator cohorts contest the doctrine continuously, with organized labor adding incident-reporting pressure. The three temporal series share one eleven-point grid; end-state values match the base_properties scalars; the oscillation is the disaster-reform cycle, and its ratcheting floor (each cycle's trough exceeds the prior cycle's start) is the extraction-accumulation signal. fixing_cost is authored 'prohibitive': whoever could fix the doctrine (the certification regulators) would need the fidelity-threshold evidence base the doctrine itself suppressed, plus re-credentialing of a veteran workforce whose standing is denominated in the old currency — the cost is immediate and concentrated while the benefit is deferred and invisible.
 *
 * PERSPECTIVAL GAP:
 *   From the veteran and live-exercise seats the doctrine is the honest description of how competence is earned and kept: the limit is real, the premium is deserved, the exercises are necessary, and the catastrophe-free calm is exactly when decay bites. From the crew, organization, and public seats the same structure operates as an enforced overclaim — certifications assert a maintenance the doctrine itself declares impossible, while the alternative that might close the gap is dismissed as a category error. The regulator seat is structurally caught: it cannot require real catastrophes, so it certifies through a proxy it officially holds insufficient, which makes its attestations complicit in the overclaim regardless of the kernel's truth. Identity-lock does real work on the beneficiary side: the veteran's exit is not merely costly but self-dissolving — 'the one who was there' is who they are — so the scarcity premium is defended as epistemic honesty rather than interest. If that identity frame broke (a cohort of simulation-trained operators commanding incident credibility), the beneficiary seat's resistance to the sibling readings would collapse and the doctrine's enforcement would lose its cultural enforcers. Crew organization (unions, professional associations) is the main coalition vector on the target side: individually constrained, collectively capable of forcing decay data into the open between events.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: veteran_real_event_operators (scarcity rent on unforgeable experience; identity_locked exit holds them deep at the beneficiary end) and live_exercise_establishments (budget and institutional purpose; the material-gain seat). Targets: high_reliability_operating_crews, high_reliability_operating_organizations, and catastrophe_exposed_public, who bear the deferred competence deficit; constrained and trapped exits hold them at the target end, and the public's powerlessness at national scope amplifies effective extraction. The excluded simulation industry sits outside the beneficiary/victim derivation — its exclusion is the enforcement object, and its position is best read as target-adjacent (it pays in devalued product and foreclosed research). The research community is the analytical seat. No directionality_overrides are declared: the beneficiary/victim declarations plus exit atoms produce the right structure, and the override surface is keyed per power atom — too coarse to separate the three institutional seats (regulators, establishments, organizations) without conflating seats that genuinely differ.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to organize trust, certification, and command role assignment when only real catastrophe could mint validated competence — was live at the doctrine's formation and is now contested: simulator fidelity has risen by orders of magnitude since the doctrine codified 'the real thing or nothing.' The classification machinery blocks two opposite mislabels. Reading the doctrine as pure mountain would launder a rent-bearing, actively enforced credential regime as natural law — the false-summit signature exists precisely to test whether the naturality framing survives the declared beneficiaries, and the ratcheting extraction series feeds the abductive trigger for investigation. Reading it as pure extraction would erase the genuine epistemic core: the stress-inoculation literature does support irreducible components of real-event exposure at current technology, so the decay cost is not fabricated. The omega structure keeps both doors open. If transfer research shows fidelity thresholds close the gap, the doctrine is a false summit whose enforcement decays toward theatrical maintenance of a legacy currency; if irreducibility holds componentially, the mountain claim stands and the rents are the price of a real scarcity. The disaster-reform cycle complicates any static verdict: each real event refreshes the doctrine's legitimacy, which is why the ratcheting floor, not any single measurement, is the diagnostic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the categorical insufficiency of simulation a genuine natural limit on competence maintenance — a physical/psychological law — or a constructed doctrine whose naturality framing sustains scarcity rents for real-event credential holders and budget protection for live-exercise programs?',
    'Ecologically valid stress-inoculation transfer studies across simulation fidelity tiers; natural experiments where jurisdictions or firms replaced real-event requirements with high-fidelity simulation and catastrophe-era performance was compared; disclosure of who funds and staffs the doctrine''s enforcement machinery.',
    'If the limit is genuine, the mountain claim stands and the decay cost is a natural law''s price; if constructed, the constraint is a false summit — an extractive credential regime wearing naturality — and the victim set expands to include the foreclosed fidelity research program.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, empirical, 'Whether the catastrophe-necessity limit is natural law or constructed doctrine (false-summit question).').

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is the catastrophe_necessity_reading of kernel catastrophe_proxy_sufficiency; what would adopting a sibling reading change structurally, and where exactly is the disagreement located?',
    'Adoption of simulation_as_proxy_catastrophe_reading dissolves the competence-decay victim set entirely and drops epsilon toward zero for the same arrangement; hybrid_degradation_reading confines decay to tacit knowledge and stress-response capacity over generational timescales, shrinking the victim set and epsilon; simulation_fidelity_threshold makes sufficiency a moving technology target, converting the fixed victim set into a technology-indexed one. The disagreement is located in whether insufficiency is categorical or contingent, and in what object ''genuine competence'' names.',
    'Epsilon and classification are reading-indexed over this fixed referent; cross-reading comparison is valid only through the family edges, never by averaging epsilon across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: one reading of the catastrophe_proxy_sufficiency kernel; sibling readings re-author epsilon and victim sets.').

omega_variable(
    irreducibility_mechanism,
    'Which component of real catastrophe is actually irreducible — the physiological stress response under genuine lethal stakes, true non-ergodic uncertainty, or the social and moral weight of real command responsibility?',
    'Componential psychophysiological studies comparing stress markers, decision quality, and transfer across fidelity tiers and real events; longitudinal cohort designs separating the components.',
    'If irreducibility reduces to one component, targeted fidelity research may close the gap for that component and pull this reading toward the threshold or hybrid siblings; if it is distributed across components, the categorical claim strengthens and the mountain reading firms up.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreducibility_mechanism, empirical, 'The mechanism of irreducibility: physiological, epistemic, or social-moral.').

omega_variable(
    decay_attribution_confound,
    'Is competence decay in catastrophe-free periods actually attributable to simulation''s insufficiency, or to confounds — automation skill fade, staffing and experience dilution, organizational drift, or reduced event frequency independent of training modality?',
    'Cohort studies comparing decay rates across organizations matched on real-event frequency but differing in simulation reliance; within-organization natural experiments when simulation programs changed.',
    'If confounds dominate, the doctrine''s victim attribution fails and epsilon for the standing arrangement drops sharply; if simulation reliance independently predicts decay, the reading''s overclaim story is confirmed and the target seats'' directionality firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_attribution_confound, empirical, 'Attribution of the decay the doctrine names: simulation insufficiency or confounds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cat_nec_reading_tr_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cat_nec_reading_tr_t3, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement(cat_nec_reading_tr_t6, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement(cat_nec_reading_tr_t9, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 9, 0.3).
narrative_ontology:measurement(cat_nec_reading_tr_t12, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(cat_nec_reading_tr_t15, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(cat_nec_reading_tr_t18, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 18, 0.33).
narrative_ontology:measurement(cat_nec_reading_tr_t21, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 21, 0.36).
narrative_ontology:measurement(cat_nec_reading_tr_t24, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(cat_nec_reading_tr_t27, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 27, 0.36).
narrative_ontology:measurement(cat_nec_reading_tr_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 30, 0.37).

% Extraction over time
narrative_ontology:measurement(cat_nec_reading_be_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cat_nec_reading_be_t3, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(cat_nec_reading_be_t6, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(cat_nec_reading_be_t9, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 9, 0.6).
narrative_ontology:measurement(cat_nec_reading_be_t12, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(cat_nec_reading_be_t15, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(cat_nec_reading_be_t18, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(cat_nec_reading_be_t21, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 21, 0.64).
narrative_ontology:measurement(cat_nec_reading_be_t24, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(cat_nec_reading_be_t27, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 27, 0.63).
narrative_ontology:measurement(cat_nec_reading_be_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 30, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(cat_nec_reading_su_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(cat_nec_reading_su_t3, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 3, 0.61).
narrative_ontology:measurement(cat_nec_reading_su_t6, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(cat_nec_reading_su_t9, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 9, 0.63).
narrative_ontology:measurement(cat_nec_reading_su_t12, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 12, 0.59).
narrative_ontology:measurement(cat_nec_reading_su_t15, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(cat_nec_reading_su_t18, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 18, 0.57).
narrative_ontology:measurement(cat_nec_reading_su_t21, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 21, 0.6).
narrative_ontology:measurement(cat_nec_reading_su_t24, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(cat_nec_reading_su_t27, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 27, 0.56).
narrative_ontology:measurement(cat_nec_reading_su_t30, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 30, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% The colloquial claim 'simulation can substitute for real catastrophe in competence maintenance' decomposes into four structurally distinct constraints (the catastrophe_proxy_sufficiency family): categorical necessity (this story), categorical sufficiency, partial/generational hybrid decay, and technology-dependent threshold sufficiency. Each has its own epsilon, victim set, and type; this reading authors epsilon for the standing simulation-reliant maintenance arrangement at 0.64 from its own lights, while the proxy sibling would author the same arrangement near zero. The upstream claim (real events are the only teacher — historically uncontested) lends the doctrine its naturality framing, which the downstream contested claims inherit; the family edges make that inheritance visible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
