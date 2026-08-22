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
 *   constraint_id: ietf_openness_commitment__legitimacy_erosion_reading
 *   human_readable: Rough Consensus Mechanism — Legitimacy Erosion Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the legitimacy-erosion reading of the IETF
 *   openness-commitment kernel: rough consensus as a procedural mechanism
 *   whose own credibility is the object being extracted. Unlike the
 *   commons-stewardship reading (which treats the open-standard as a public
 *   good preserved for all implementers) or the capture-substrate reading
 *   (which treats the standards process as a coordination substrate where
 *   resource advantage is encoded directly into technical gatekeeping), this
 *   reading's claim is narrower and more corrosive: the
 *   consensus-determination act itself — the chair's declaration of 'rough
 *   consensus' — is a legitimacy-bearing artifact that well-resourced
 *   factions have learned to manufacture through sustained presence rather
 *   than earn through technical argument, and every successful manufacture of
 *   that kind degrades the credibility of the mechanism for the next dispute.
 *   The victim here is not (only) any single excluded implementer's design
 *   preference; it is the future evidentiary value of a 'consensus was
 *   reached' claim.
 *
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
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "Rough Consensus Mechanism — Legitimacy Erosion Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, '4843c449-2084-4356-bf85-92c821584ea9').
narrative_ontology:cs_kernel_codification('4843c449-2084-4356-bf85-92c821584ea9', distributed).
narrative_ontology:cs_authority_grounding('4843c449-2084-4356-bf85-92c821584ea9', practice).
narrative_ontology:cs_interpretation_layer_present('4843c449-2084-4356-bf85-92c821584ea9').
narrative_ontology:cs_reading_relation('4843c449-2084-4356-bf85-92c821584ea9', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('4843c449-2084-4356-bf85-92c821584ea9', ietf_openness_commitment__capture_substrate_reading, influences).
narrative_ontology:cs_axiom('4843c449-2084-4356-bf85-92c821584ea9', foundational, consensus_declaration_is_a_credibility_bearing_act).
narrative_ontology:cs_axiom_status(consensus_declaration_is_a_credibility_bearing_act, holdable).
narrative_ontology:cs_axiom_grounding('4843c449-2084-4356-bf85-92c821584ea9', consensus_declaration_is_a_credibility_bearing_act, conventional).
narrative_ontology:cs_axiom('4843c449-2084-4356-bf85-92c821584ea9', foundational, sustained_presence_is_not_a_valid_proxy_for_technical_merit).
narrative_ontology:cs_axiom_status(sustained_presence_is_not_a_valid_proxy_for_technical_merit, holdable).
narrative_ontology:cs_axiom_grounding('4843c449-2084-4356-bf85-92c821584ea9', sustained_presence_is_not_a_valid_proxy_for_technical_merit, empirically_contingent).
narrative_ontology:cs_reference_frame('4843c449-2084-4356-bf85-92c821584ea9', rough_consensus_as_merit_convergence).
narrative_ontology:cs_drift_state('4843c449-2084-4356-bf85-92c821584ea9', post_professionalized_participation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4843c449-2084-4356-bf85-92c821584ea9', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_working_group_chairs).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, independent_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, underfunded_participants).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, future_standards_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund multiple full-time engineers to attend every meeting, author every draft revision, and staff the mailing-list threads that determine what counts as 'the sense of the room.' Coordinate positions across delegates so that any dissenting voice appears isolated rather than representative. Can walk away and ship a de facto standard through market power if the working group does not ratify their preferred design, so their exit option undercuts the coordination stakes for everyone else.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions, agenda_setter).

% Declare rough consensus, decide which objections are 'considered but overridden,' and control the agenda and document editorship. Depend on continued participation from well-funded coalitions to keep the working group active and are professionally identified with the process's legitimacy, which makes them reluctant to rule against the loudest, best-staffed faction even when its position is a minority view.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_working_group_chairs, agenda_setter,
    institutional, generational, constrained, global).

% Build and maintain implementations without corporate sponsorship, attend meetings on personal time, and cannot staff the sustained thread presence needed to be heard as 'the room.' Their technical objections are frequently noted in minutes and then overridden by declared rough consensus. Exit means either implementing a spec they believe is technically inferior or maintaining an incompatible fork that loses interoperability.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, independent_implementers, payer,
    moderate, biographical, constrained, global).

% Represent civil-society, academic, or Global South interests with no travel budget and no dedicated staff time; participate sporadically via mailing list. Their positions arrive late, are read as 'not really part of the discussion,' and are the first excluded when a chair narrates who was 'considered.' They have no credible exit — they either accept whatever emerges or lose any voice in a standard that will govern infrastructure they depend on.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, underfunded_participants, payer,
    powerless, immediate, trapped, global).

% Everyone who will build on top of the ratified standard after the fact — developers, downstream vendors, end users of interoperable systems. Never had a seat in the room and bear the long-run cost of a standard shaped to favor incumbents' architecture rather than open interoperability, discovered only once switching costs have made the design load-bearing.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, future_standards_users, payer,
    powerless, civilizational, trapped, global).

% The IESG and IAB hear appeals of consensus calls and can, in principle, overturn a chair's declaration. In practice they rarely reverse a working-group consensus determination absent egregious procedural failure, and their review criteria (was the process followed) do not reach whether the underlying consensus was captured by resource asymmetry rather than technical merit.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_leadership_bodies, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__legitimacy_erosion_reading, ietf_leadership_bodies, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_vendor_coalitions).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__legitimacy_erosion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rough consensus is meant to let a technically diverse, geographically distributed community converge on a single interoperable specification without a formal vote that a minority could weaponize or a single vendor veto that could stall progress indefinitely.
% TRANSFER_FUNCTION: Moves the authority to define what counts as 'the sense of the working group' — and thereby the shape of the resulting standard — from whichever position is technically strongest to whichever position is best staffed and most persistently voiced, transferring architectural control from independent and underfunded participants to well-resourced coalitions.
% ABSENT_VOICES: Underfunded participants and future standards users are structurally quietest: the former cannot sustain the thread presence rough consensus actually measures, the latter are not born yet or not present as an organized interest. Their objections, when raised at all, are the ones most often narrated by chairs as 'considered but not enough to block consensus.'
% DISAPPEARANCE_RATIONALE: Well-resourced coalitions would say the world stays the same — they would simply route around a defunct IETF via de facto market standards, having the market power to do so. Independent implementers and underfunded participants would say the world rearranges significantly: without even a contested legitimacy claim to appeal to, there would be no forum in which a well-documented technical minority objection could ever be recorded, let alone occasionally prevail. The dispute over which is true is itself part of what the legitimacy-erosion reading identifies as under contest.
% FOUNDING_PROBLEM: Early internet standards bodies needed a way to reach durable technical agreement across an open, voluntary, non-hierarchical community without the paralysis of unanimity requirements or the capture risk of formal majority voting controlled by whoever shows up with the most delegates.
% FOUNDING_PROBLEM_CORROBORATION: IETF leadership and long-tenured chairs attest the mechanism still functions as designed and point to successful multi-stakeholder standards as evidence. Independent researchers studying IETF participation data (academic network-governance scholarship, not funded by working-group participants) and several public post-mortems from implementers who left contested working groups attest that participation and influence now correlate strongly with employer travel budgets and staffing, and that 'considered but overridden' has become a formula for dismissing under-resourced objections regardless of technical merit — corroboration from outside the coalitions that benefit from the current reading of consensus.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, contested).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises over the interval (0.38 to 0.71) as sustained-presence tactics (staffed delegations, thread persistence, coordinated late-stage objections) become more professionalized among vendor coalitions, while theater_ratio rises even faster (0.25 to 0.62) because the actual technical deliberation that rough consensus is supposed to measure increasingly gives way to performative process-following — extensive minute-taking and 'last call' rituals that document that objections were heard without altering outcomes. Suppression is moderate and rising (0.32 to 0.58): there is no formal exclusion, but sustained procedural fatigue and the framing of persistent objectors as 'blocking' functions as an increasingly effective informal filter. Accessibility_collapse is comparatively low (0.4) because the mailing lists and meetings remain nominally open to anyone — the capture operates through differential capacity to use the open door, not through closing it.
 *
 * PERSPECTIVAL GAP:
 *   From the chairs' and vendor coalitions' seats, each individual consensus call looks procedurally sound — objections were heard, minutes reflect deliberation, no rule was violated. From independent implementers' and underfunded participants' seats, the same sequence of individually-defensible calls aggregates into a pattern where their technically substantive objections are reliably the ones narrated as 'considered but overridden.' Neither seat is lying; the divergence is structural, arising from who has the resources to make persistence itself the deciding factor.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendor coalitions sit near the beneficiary end: their arbitrage-grade exit option (they can ship de facto standards regardless of IETF ratification) paradoxically increases their leverage inside the process, since they have the least to lose if a given consensus call goes against them and the most staff to make sure it usually doesn't. Independent implementers and underfunded participants sit near the target end: constrained-to-trapped exit options mean the outcome of any given consensus call is load-bearing for their actual product or advocacy work, with no comparable fallback. Future standards users are structurally maximal-target despite having zero present voice — their d is high not because they are exploited today but because the standard's design costs will be theirs to bear indefinitely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reaching durable technical agreement without unanimity-paralysis or majority-vote capture — has not disappeared; large open standards efforts still need SOME convergence mechanism. That is why this is authored as tangled_rope rather than snare: a genuine coordination function persists (the working group does eventually produce interoperable specifications that ship and work). But the specific instrument for measuring 'the sense of the room' has drifted from a technical-merit proxy toward a staffing-and-persistence proxy, and that drift is not visible in the process's own self-description — chairs and leadership bodies still narrate every consensus call as a merit-based technical determination. The mandatrophy is partial and reading-specific: the mechanism's ORIGINAL mandate (durable convergence) is still live; what has eroded is the CLAIM that the mechanism reliably measures the thing it purports to measure. That erosion is precisely what the sibling capture_substrate_reading treats as already fully realized in the technical output, while this reading locates the erosion one level up, in the credibility of the determination procedure itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ietf_openness,
    'The IETF openness commitment kernel admits at least three structurally distinct readings (commons_stewardship, capture_substrate, legitimacy_erosion); which reading a given dispute over a specific working group invokes determines whether the relevant failure mode is measured as design capture, credibility erosion, or no failure at all.',
    'No single resolution mechanism disambiguates the readings, because they are not competing empirical claims about one fact — they are different structural lenses on the same kernel, each tracking a different variable (output interoperability, encoded gatekeeping, procedural credibility). A sibling reading would change which variable is treated as the victim: commons_stewardship tracks whether the resulting standard remains implementable by all; capture_substrate tracks whether resource advantage is directly encoded into technical choices; legitimacy_erosion (this reading) tracks whether the consensus-declaration act itself retains evidentiary value.',
    'Adopting the capture_substrate_reading instead would shift the analysis from the credibility of the process to the content of the resulting standard, likely producing a starker victim set (implementers locked into an inferior architecture) and possibly a snare rather than tangled_rope classification, since capture_substrate''s coordination story is thinner. Adopting commons_stewardship would produce a much lower extractiveness reading focused on whether interoperability was actually preserved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ietf_openness, conceptual, 'Which reading of the openness-commitment kernel a given IETF dispute should be evaluated under, and where structural disagreement is located.').

omega_variable(
    consensus_measurement_validity,
    'Is ''rough consensus'' still measuring anything distinguishable from ''sustained staffed presence,'' or has the proxy fully replaced the target it was meant to approximate?',
    'Comparative analysis of working groups with mandatory participation-diversity requirements or resourced travel grants for underfunded participants against working groups without such support, examining whether consensus outcomes and technical quality differ.',
    'If diversity-supported working groups produce measurably different (and more broadly adopted or more interoperable) outcomes, this would corroborate the legitimacy_erosion reading''s claim that resource asymmetry — not technical merit — is currently driving consensus declarations. If outcomes are indistinguishable, the erosion claim weakens considerably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_measurement_validity, empirical, 'Whether rough consensus retains discriminating power independent of participant resourcing.').

omega_variable(
    appeal_mechanism_toothlessness,
    'Does the IESG/IAB appeal process for consensus calls provide a genuine check on capture, or is it structurally unable to reach resource-asymmetry-driven capture because its review standard is procedural compliance rather than substantive fairness?',
    'Review of historical appeal outcomes: how many appeals alleging resource-driven capture (as distinct from procedural irregularity) have succeeded, and what standard of review was applied.',
    'If appeals essentially never succeed on capture grounds because the review standard cannot reach the underlying asymmetry, this substantially strengthens the case that requires_active_enforcement (chairs'' authority to declare consensus, backed by an appeal process that rarely overturns it) sustains the tangled-rope structure rather than checking it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appeal_mechanism_toothlessness, empirical, 'Whether the formal appeal mechanism can actually reach resource-driven capture of consensus calls.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ietf_tr_t6, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement(ietf_tr_t12, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(ietf_tr_t18, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 18, 0.49).
narrative_ontology:measurement(ietf_tr_t24, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 24, 0.56).
narrative_ontology:measurement(ietf_tr_t30, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ietf_be_t6, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(ietf_be_t12, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(ietf_be_t18, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(ietf_be_t24, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(ietf_be_t30, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 30, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(ietf_su_t6, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(ietf_su_t12, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(ietf_su_t18, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 18, 0.49).
narrative_ontology:measurement(ietf_su_t24, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(ietf_su_t30, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__legitimacy_erosion_reading, 0.12).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__capture_substrate_reading).

% DUAL FORMULATION NOTE:
% Three linked readings of one kernel (ietf_openness_commitment): commons_stewardship_reading (low extraction, output-focused, treats the standard as preserved public infrastructure), capture_substrate_reading (high extraction, artifact-focused, treats the process as already encoding resource advantage into technical outcomes), and this legitimacy_erosion_reading (moderate-high extraction, procedure-focused, treats the consensus-declaration mechanism's own credibility as the contested resource). Each reading authors its own ε against the same underlying kernel text and institutional history; they are not averaged or reconciled — per DP-001 each is a distinct constraint with a stable ε from its own reading's lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
