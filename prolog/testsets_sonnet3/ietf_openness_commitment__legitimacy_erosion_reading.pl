% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__legitimacy_erosion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__legitimacy_erosion_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ietf_openness_commitment__legitimacy_erosion_reading
 *   human_readable: IETF Rough Consensus Mechanism — Legitimacy Erosion Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This story reads the IETF's rough-consensus mechanism from the position
 *   that the mechanism itself — not merely particular standards outcomes — is
 *   the thing being extracted from. Well-resourced vendor coalitions do not
 *   need to overturn the process; they need only outlast unfunded objectors
 *   within it, and the chairs who certify 'consensus' face structural
 *   pressure to declare convergence achieved rather than expose the process
 *   as gridlocked or captured. What is extracted is the presumption of
 *   legitimacy the RFC label carries: each contested consensus call that
 *   actually reflects delegation size rather than technical merit spends down
 *   the credibility commons that makes 'this is an IETF standard' mean
 *   something distinct from 'this is what the largest vendors agreed to.'
 *   This is one of three linked readings of the same kernel
 *   (ietf_openness_commitment): the commons_stewardship_reading treats the
 *   same standards process as largely successful public infrastructure; the
 *   capture_substrate_reading treats the process as a substrate whose
 *   gatekeeping function is the primary structural fact. This reading is
 *   narrower than either — it is specifically about the fragility and
 *   contestedness of the legitimacy-conferring mechanism, and treats the
 *   mechanism's own credibility as the asset under extraction, distinct from
 *   the standards' interoperability value (commons_stewardship) or from the
 *   gatekeeping outcomes themselves (capture_substrate).
 *
 * KEY AGENTS:
 *   - well_resourced_vendor_coalitions: Primary beneficiary (organized/arbitrage) — extracts legitimacy through sustained procedural presence
 *   - incumbent_chairs_and_area_directors: Agenda-setter (institutional/constrained) — certifies consensus under structural pressure to declare success
 *   - independent_engineers: Primary target (powerless/trapped) — objections procedurally noted then aged out
 *   - underfunded_implementers: Secondary target (moderate/constrained) — bears downstream costs of captured standards
 *   - the_consensus_mechanism_itself: Non-agent payer (institutional/trapped) — the credibility asset being spent down
 *   - excluded_regional_and_civil_society_voices: Excluded (powerless/trapped) — structurally absent from the rooms where consensus is negotiated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.71).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.58).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough Consensus Mechanism — Legitimacy Erosion Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, '77e7b114-7251-4ee1-9110-5384ac68363d').
narrative_ontology:cs_kernel_codification('77e7b114-7251-4ee1-9110-5384ac68363d', implicit).
narrative_ontology:cs_authority_grounding('77e7b114-7251-4ee1-9110-5384ac68363d', practice).
narrative_ontology:cs_interpretation_layer_present('77e7b114-7251-4ee1-9110-5384ac68363d').
narrative_ontology:cs_reading_relation('77e7b114-7251-4ee1-9110-5384ac68363d', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('77e7b114-7251-4ee1-9110-5384ac68363d', ietf_openness_commitment__capture_substrate_reading, influences).
narrative_ontology:cs_axiom('77e7b114-7251-4ee1-9110-5384ac68363d', foundational, consensus_certification_is_a_scarce_credibility_asset).
narrative_ontology:cs_axiom_status(consensus_certification_is_a_scarce_credibility_asset, holdable).
narrative_ontology:cs_axiom_grounding('77e7b114-7251-4ee1-9110-5384ac68363d', consensus_certification_is_a_scarce_credibility_asset, empirically_contingent).
narrative_ontology:cs_axiom('77e7b114-7251-4ee1-9110-5384ac68363d', secondary, procedural_safeguards_designed_against_obstruction_do_not_transfer_to_coordinated_resource_pressure).
narrative_ontology:cs_axiom_status(procedural_safeguards_designed_against_obstruction_do_not_transfer_to_coordinated_resource_pressure, holdable).
narrative_ontology:cs_axiom_grounding('77e7b114-7251-4ee1-9110-5384ac68363d', procedural_safeguards_designed_against_obstruction_do_not_transfer_to_coordinated_resource_pressure, empirically_contingent).
narrative_ontology:cs_reference_frame('77e7b114-7251-4ee1-9110-5384ac68363d', rough_consensus_running_code_meritocratic_ideal).
narrative_ontology:cs_drift_state('77e7b114-7251-4ee1-9110-5384ac68363d', post_commercialization_multistakeholder_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('77e7b114-7251-4ee1-9110-5384ac68363d', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_chairs_and_area_directors).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, independent_engineers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, underfunded_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, the_consensus_mechanism_itself).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Send large, coordinated delegations of paid engineers to working groups, sustain presence across every meeting and mailing-list thread over years, and can outlast any individual objector's attention span. They shape what counts as 'rough consensus' by sheer persistence and volume of participation, then cite the resulting RFC as neutral technical consensus in downstream commercial and regulatory contexts.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions, agenda_setter).

% Declare when rough consensus has been reached, a judgment call with no formal vote to appeal against except a slow, rarely-used process (Last Call objections, IESG review). Their institutional legitimacy and career standing depend on the process appearing to work; they have structural incentive to certify consensus even when it reflects coordinated pressure rather than genuine convergence.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_chairs_and_area_directors, agenda_setter,
    institutional, generational, constrained, global).

% Participate as unpaid or minimally-supported individuals, attending on personal time. They can raise objections but cannot sustain multi-year presence to outlast a funded coalition's repetition; their technical objections get procedurally noted and then aged out as the well-resourced side simply keeps showing up. Exit means abandoning influence over standards that will govern their own future implementation work.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, independent_engineers, payer,
    powerless, immediate, trapped, global).

% Small companies and open-source projects that must implement whatever standard emerges, regardless of whether it reflects their interests, because market compatibility requires conformance. They pay downstream costs of standards shaped to favor larger competitors' existing architectures, without having had the resources to shape the standard themselves.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, underfunded_implementers, payer,
    moderate, biographical, constrained, global).

% The procedural fiction of 'rough consensus, running code' is the asset being spent. Each instance where declared consensus is later shown to reflect coordinated capture rather than genuine convergence, the mechanism's claim to produce legitimate, non-captured technical judgments is degraded; this is a non-agent entity (the credibility of the process) held for narrative completeness, not a party that can act.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, the_consensus_mechanism_itself, payer,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(ietf_openness_commitment__legitimacy_erosion_reading, the_consensus_mechanism_itself).

% Reviews contested consensus calls on appeal, can in principle overturn a chair's declaration, but rarely does so and faces the same resource-asymmetry pressures in evaluating who objects loudest and longest.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, iesg, observer,
    institutional, generational, analytical, global).

% Users in regions and communities without funded technical delegations, and civil-society technologists concerned with rights-preserving design, are structurally absent from the rooms and calls where rough consensus is negotiated. They would object to standards shaped around vendor interoperability priorities that deprioritize privacy or accessibility, but are not present to make the objection register procedurally.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, excluded_regional_and_civil_society_voices, excluded,
    powerless, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rough-consensus process solves a genuine coordination problem: producing interoperable technical standards without a formal voting bureaucracy, using engineering judgment and sustained working-group deliberation to converge on workable specifications faster than a treaty-style process would allow.
% TRANSFER_FUNCTION: Moves legitimacy — the presumption that an RFC reflects disinterested technical convergence — from the commons of all implementers to whichever coalition can sustain the longest, most resourced presence in the room. The transfer is not of money directly but of standing: the right to have one's technical preference ratified as 'the consensus.'
% ABSENT_VOICES: Independent engineers who cannot sustain years of unpaid participation are procedurally present but functionally worn down; regional and civil-society voices are largely absent from working-group rosters entirely. Both would object that declared consensus reflects attendance and funding rather than technical merit, but neither has a mechanism to make that objection dispositive.
% DISAPPEARANCE_RATIONALE: Vendor coalitions would say the process is essential infrastructure and its disappearance would fragment the internet into incompatible vendor-specific standards — the world rearranges toward chaos. Critics of capture would say the credible-consensus claim already functions mostly as legitimation theater for outcomes vendors would have reached anyway through bilateral negotiation, so its disappearance would mainly strip a false neutrality claim rather than change technical outcomes. The parties genuinely dispute which is true.
% FOUNDING_PROBLEM: Early internet standards bodies needed a way to converge on interoperable specifications without the capture risks of formal ballot-based standards bodies (like some ITU/ISO processes, seen at the time as more vulnerable to national and vendor bloc voting) and without the slowness of unanimous agreement. Rough consensus plus running code was designed to reward working code and engineering merit over political maneuvering.
% FOUNDING_PROBLEM_CORROBORATION: Long-time IETF participants and several IESG alumni have published retrospectives (mailing-list post-mortems, academic case studies of specific working groups) documenting instances where declared consensus tracked corporate delegation size rather than technical merit — corroboration from inside the process but critical of its current beneficiaries. Independent academic studies of standards-body capture (outside IETF itself) corroborate the general pattern in comparable bodies. No source entirely outside all participating factions has audited IETF consensus calls specifically; the strongest outside corroboration is comparative institutional research on standards-setting capture generally, not a dedicated independent audit of IETF.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, contested).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ietf_openness_commitment__legitimacy_erosion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) and rising over the interval because the specific harm this reading tracks is not a bad standard but a degrading legitimacy claim — each contested consensus call where declared convergence is later shown to track resourcing rather than merit compounds. Theater ratio is authored as substantial and rising (0.62 at T=25) because as genuine convergence becomes harder to achieve organically (more commercial stakes, more coordinated participation), more of what passes as 'rough consensus process' is procedural performance — Last Call periods, appeal mechanisms — that rarely reverses outcomes but preserves the appearance of contestability. Suppression is moderate (0.58): there is no formal barrier to participation, but sustained unfunded participation is practically suppressed by resource asymmetry, which is a real structural mechanism even without explicit coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor-coalition seat, the process is working exactly as designed — sustained engagement and technical investment ARE the legitimate basis for influence, and calling this capture mistakes commitment for capture. From the independent-engineer seat, the same sustained engagement is indistinguishable from attrition warfare that has nothing to do with technical merit. The chair seat experiences genuine difficulty distinguishing the two in real time, which is exactly the structural vulnerability this reading names: procedural safeguards (Last Call, IESG appeal) exist but were designed against different capture vectors (single-actor obstruction) than the one now operative (coordinated resourced presence).
 *
 * DIRECTIONALITY LOGIC:
 *   Vendor coalitions sit near the full-beneficiary end: they extract the legitimacy premium (their preferred outcome gets to be called 'consensus' rather than 'what we wanted') while bearing minimal structural cost — their resource advantage IS their exit option (arbitrage: if one working group resists, redirect effort to another venue or fork the spec elsewhere). Independent engineers sit near the full-target end: trapped, because opting out means losing all influence over standards they must still implement. The consensus mechanism itself is authored as a non-agent payer specifically because credibility cannot act to defend itself — it is degraded as a byproduct of the other seats' interactions, which is the structural signature this reading is built to surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabelings. First, against treating this purely as a rope (successful coordination) simply because rough consensus IS solving a real problem (interoperability without formal balloting) — the coordination function is real and named in six_questions, but coexists with the extraction. Second, against treating it purely as a snare with no coordination value — that would miss why the mechanism persists and retains buy-in from all factions including the ones it disadvantages: it still produces genuinely useful interoperable standards often enough that abandoning it is not obviously better for anyone, including underfunded implementers who still prefer a contestable process to a purely closed vendor consortium. Tangled rope is the structurally honest reading: genuine coordination function, genuine and asymmetric extraction, riding the same mechanism, requiring active certification (chair rulings, IESG review) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_vs_genuine_convergence_indistinguishability,
    'In any given contested working-group decision, is the declared rough consensus tracking genuine technical convergence that happens to align with a well-resourced coalition''s preference, or is it tracking resource-asymmetry attrition dressed as convergence?',
    'Comparative analysis of working-group outcomes against independent technical merit assessments (e.g., post-hoc interoperability testing, adoption patterns outside the sponsoring vendors) across a sample of contested RFCs, cross-referenced against participant funding/affiliation records.',
    'If outcomes systematically track funding rather than merit even after controlling for technical quality, this reading''s extraction claim is substantially strengthened and the mechanism moves further toward snare; if outcomes track merit independent of funding, the erosion claim weakens toward a genuine rope with occasional noise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_vs_genuine_convergence_indistinguishability, empirical, 'Whether declared consensus outcomes are distinguishable from resource-driven attrition outcomes.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is ''the IETF openness commitment'' best understood as fundamentally a commons-stewardship success story with capture as a contained pathology (commons_stewardship_reading), a gatekeeping substrate whose distributive function is primary (capture_substrate_reading), or a legitimacy mechanism whose credibility is the actual site of contest (this reading)? These are not merely different emphases — they imply different remedies and different ε referents.',
    'No single empirical test resolves this because the three readings select different referents for what counts as the ''main'' phenomenon. Longitudinal tracking of which reading better predicts future institutional trajectory (does the process reform its legitimacy mechanisms, does gatekeeping outcomes worsen, does interoperability value persist) would provide comparative evidence over a multi-decade horizon.',
    'Choosing this reading over its siblings determines whether the appropriate intervention target is procedural reform of consensus certification (this reading), redistribution of gatekeeping power (capture_substrate), or protection of the commons function as already largely adequate (commons_stewardship).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which of three kernel readings best characterizes the dominant structural fact about IETF consensus governance.').

omega_variable(
    chair_certification_incentive_structure,
    'Do chairs and area directors have a structural incentive to certify contested consensus as achieved (because non-certification reflects poorly on their stewardship and stalls their working group''s output metrics), independent of the merits of any particular case?',
    'Survey or interview data from former chairs about pressure to reach closure; comparison of certification rates against working-group tenure and career-advancement patterns within IETF leadership.',
    'If chairs face genuine career incentive to certify, the suppression/theater metrics in this story are understated rather than overstated, and the legitimacy-erosion mechanism is self-reinforcing rather than incidental.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chair_certification_incentive_structure, empirical, 'Whether chair incentives are structurally biased toward premature consensus certification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ietf_tr_t5, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement(ietf_tr_t15, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 15, 0.49).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement(ietf_tr_t25, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 25, 0.62).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ietf_be_t5, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(ietf_be_t15, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ietf_be_t25, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 25, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0, 0.31).
narrative_ontology:measurement(ietf_su_t5, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 5, 0.36).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(ietf_su_t15, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(ietf_su_t25, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__capture_substrate_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the ietf_openness_commitment kernel. commons_stewardship_reading treats the standards process as largely-successful public infrastructure (low ε). capture_substrate_reading treats it as a coordination substrate whose gatekeeping outcomes are the primary structural fact (moderate-high ε concentrated in distributive outcomes). This reading (legitimacy_erosion_reading) isolates the narrower claim that the consensus-certification mechanism's own credibility is the asset under contest and extraction (high ε, rising). Each reading authors its own ε against the same standing arrangement, assessed by its own lights; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
