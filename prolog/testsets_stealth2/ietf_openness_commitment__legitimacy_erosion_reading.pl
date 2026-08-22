% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__legitimacy_erosion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: ietf_openness_commitment__legitimacy_erosion_reading
 *   human_readable: IETF Rough Consensus Under Organized Capture Pressure (Legitimacy-Erosion Reading)
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the ietf_openness_commitment
 *   kernel: the rough-consensus mechanism itself as contested and vulnerable
 *   to organized capture despite procedural safeguards. The referent of
 *   epsilon is fixed: the standing arrangement under contest — the IETF's
 *   rough-consensus procedure as actually operated (mailing lists, meetings,
 *   hums, chair determinations, last calls, IESG approval) — assessed by this
 *   reading's own lights, which see well-resourced factions extracting
 *   procedural legitimacy to ratify self-serving outcomes. The victim named
 *   by this reading is the consensus mechanism's credibility: a commons
 *   depleted by each contested closure, modeled here as a non-agent
 *   stakeholder (agent=false) so it documents the depletion without feeding
 *   directionality arithmetic as if it collected or paid. The sibling
 *   readings (commons_stewardship, capture_substrate) are separate constraint
 *   files with their own epsilon values over the same referent; they are
 *   linked through network.affects_constraints and are deliberately NOT
 *   averaged into this story. Calendar anchoring for the interval: T=0
 *   approximates 1992 (Dave Clark's 'rough consensus and running code'
 *   formulation), T=32 approximates 2024; intermediate points track
 *   commercialization-era professionalization, RFC 7282's consensus guidance
 *   (approx T=22), and the ombudsteam's creation (approx T=27).
 *
 * KEY AGENTS:
 *   - ietf_chairs_and_area_directors: agenda_setter (institutional/constrained) — administers hums, declares consensus, gates progression; structurally exposed to the factions they adjudicate
 *   - hyperscaler_standards_delegations: primary beneficiary (institutional/arbitrage) — converts ratified outcomes into deployed product advantage; strongest exit in the system
 *   - incumbent_equipment_vendors: secondary beneficiary (powerful/constrained) — collects specifications aligned with installed product lines; bound by customer expectations
 *   - independent_implementers: primary payer (moderate/constrained) — absorbs outcomes negotiated past them; cannot leave interoperability
 *   - unaffiliated_working_group_participants: payer (moderate/identity_locked) — supplies unpaid expert labor; mission-identity fusion makes exit feel like betrayal
 *   - ietf_consensus_legitimacy_commons: depleted asset (non-agent) — the credibility stock each contested closure draws down
 *   - rival_sdos_and_forums: excluded (institutional/trapped) — would contest venue jurisdiction; kept outside by the community's own norms
 *   - process_researchers_and_ombuds: analytical observer — measures participation asymmetry and mediates conduct disputes without decision authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.7).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.6).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough Consensus Under Organized Capture Pressure (Legitimacy-Erosion Reading)").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, '50dc84a1-f009-4a41-a04f-f131b6410a68').
narrative_ontology:cs_kernel_codification('50dc84a1-f009-4a41-a04f-f131b6410a68', formalized).
narrative_ontology:cs_authority_grounding('50dc84a1-f009-4a41-a04f-f131b6410a68', lineage).
narrative_ontology:cs_interpretation_layer_present('50dc84a1-f009-4a41-a04f-f131b6410a68').
narrative_ontology:cs_reading_relation('50dc84a1-f009-4a41-a04f-f131b6410a68', ietf_openness_commitment__commons_stewardship_reading, influences).
narrative_ontology:cs_reading_relation('50dc84a1-f009-4a41-a04f-f131b6410a68', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_axiom('50dc84a1-f009-4a41-a04f-f131b6410a68', foundational, procedural_safeguards_insufficient_against_organized_capture).
narrative_ontology:cs_axiom_status(procedural_safeguards_insufficient_against_organized_capture, holdable).
narrative_ontology:cs_axiom_grounding('50dc84a1-f009-4a41-a04f-f131b6410a68', procedural_safeguards_insufficient_against_organized_capture, empirically_contingent).
narrative_ontology:cs_axiom('50dc84a1-f009-4a41-a04f-f131b6410a68', foundational, legitimacy_is_extractable_commons).
narrative_ontology:cs_axiom_status(legitimacy_is_extractable_commons, holdable).
narrative_ontology:cs_axiom_grounding('50dc84a1-f009-4a41-a04f-f131b6410a68', legitimacy_is_extractable_commons, empirically_contingent).
narrative_ontology:cs_reference_frame('50dc84a1-f009-4a41-a04f-f131b6410a68', clark_era_open_participation_norm).
narrative_ontology:cs_drift_state('50dc84a1-f009-4a41-a04f-f131b6410a68', contemporary_professionalized_participation_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('50dc84a1-f009-4a41-a04f-f131b6410a68', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, hyperscaler_standards_delegations).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_equipment_vendors).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, independent_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, unaffiliated_working_group_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Working-group chairs, Area Directors, and IESG members run the consensus machinery: they open and close discussion, call and interpret hums, judge whether rough consensus exists, and approve or block progression to standard. Many hold day jobs at the same large vendors that field the biggest delegations. Stepping down or ruling against a well-organized faction carries career and social cost inside the community, so their practical option set is narrow even though the office itself is nominally revocable.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_chairs_and_area_directors, agenda_setter,
    institutional, biographical, constrained, global).

% Large cloud and platform companies maintain standing standards teams that pre-negotiate positions internally, fund attendance at every meeting, sustain continuous mailing-list presence, and coordinate messaging across many simultaneous working groups. When a contested decision closes in their preferred direction, the ratified specification flows directly into product roadmaps deployed to billions of users. If the process stops delivering, they can move work to consortia or simply deploy de facto standards unilaterally.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, hyperscaler_standards_delegations, beneficiary,
    institutional, generational, arbitrage, global).

% Established network-equipment and systems vendors hold decades of accumulated working-group expertise, staff continuity, and chair positions. They collect ratified specifications aligned with installed product lines and certification programs. Their customers expect IETF-standardized interfaces, so relocating standardization work elsewhere would strand that expectation; their leverage is deep but bounded by dependence on the process's continued output.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_equipment_vendors, beneficiary,
    powerful, generational, constrained, global).

% Small companies and open-source projects build products against ratified specifications. They absorb the costs of outcomes negotiated past them: features sized for hyperscale operators, patent-encumbered or complexity-heavy mechanisms, and rework when contested decisions reverse earlier drafts. They cannot walk away from interoperability, and their objections during last calls routinely arrive with fewer person-hours behind them than the opposition's.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, independent_implementers, payer,
    moderate, biographical, constrained, global).

% Individual engineers, academics, and hobbyists contribute review, code, and argument without organizational backing. Their participation is sustained substantially by identification with the IETF's open-participation mission; repeated experience of objections being procedurally outlasted erodes that attachment, but leaving feels like abandoning the project itself rather than changing venues. What they supply is unpaid expert labor; what they receive when decisions close against them is a ratified document they must nonetheless implement.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, unaffiliated_working_group_participants, payer,
    moderate, biographical, identity_locked, global).

% The stock of credibility that makes 'IETF standard' mean something to implementers, regulators, and operators. Each contested decision closed over visible objection withdraws from this stock faster than routine successful standardization replenishes it. It has no guardian of its own; it is maintained incidentally by the very actors whose withdrawals deplete it, and it cannot relocate to another venue.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_consensus_legitimacy_commons, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(ietf_openness_commitment__legitimacy_erosion_reading, ietf_consensus_legitimacy_commons).

% Other standards bodies and industry consortia capable of hosting the same protocol work sit outside the IETF's consensus conversation. When the IETF claims jurisdiction over a technology space, these bodies are structurally not in the room where the ratification happens; they would argue for their own venues, voting rules, and membership models, and their exclusion is maintained by the community's attachment to doing it 'the IETF way'.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, rival_sdos_and_forums, excluded,
    institutional, generational, trapped, global).

% Academic students of standards governance together with the IETF's own ombudsteam track participation asymmetries, chair-decision patterns, and complaint volumes. They publish findings, mediate conduct disputes, and recommend procedural adjustments, but hold no vote over technical decisions and depend on the organization's willingness to act on what they report.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, process_researchers_and_ombuds, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__legitimacy_erosion_reading, hyperscaler_standards_delegations).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__legitimacy_erosion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates the dispersed technical judgment of thousands of geographically distributed, largely voluntary participants into single ratified protocol specifications, without formal voting or central command, so that independently built systems interoperate.
% TRANSFER_FUNCTION: Moves procedural legitimacy — the 'the community agreed' imprimatur attached to a ratified specification — from the general participant body to whichever faction works the process most effectively; secondarily, it moves unpaid expert labor (review, implementation feedback, meeting attendance) from individual contributors into documents whose deployment benefits concentrate among large vendors.
% ABSENT_VOICES: Future implementers and network operators not yet in the room bear consequences of today's ratifications with no seat; participants who disengaged after losing contested calls are gone precisely because they objected; rival standards bodies would contest venue jurisdiction but are outside the conversation by construction.
% DISAPPEARANCE_RATIONALE: If the rough-consensus procedure vanished overnight, protocol standardization would reorganize around formal-voting bodies, vendor consortia, or unilateral de facto deployment by the largest platforms; the volunteer review economy would disperse, and the meaning of 'Internet Standard' would be renegotiated among whoever picked up the function.
% FOUNDING_PROBLEM: Fragmented, proprietary networking protocols of the 1970s–1980s: heterogeneous vendor stacks could not interoperate, and access to protocol specifications was gated by licensing and corporate secrecy. The IETF's open, rough-consensus process was built to produce vendor-neutral specifications anyone could implement.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the historical record of pre-IETF protocol fragmentation (competing proprietary stack wars of the 1980s), documented reliance by network operators and governments on openly licensed specifications, and the SDO-economics literature treating interoperability failure as the baseline counterfactual. Hyperscaler delegations also attest the problem, but the attestation does not depend on them.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ietf_openness_commitment__legitimacy_erosion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.70) because what is taken — procedural legitimacy — is decoupled from what is contributed: a faction's positional advantage in the process scales with resourcing, not with the merit of its arguments, and each ratified-but-contested outcome transfers communal imprimatur onto private roadmaps. Suppression (0.60) is real but procedural rather than coercive: closure authority, agenda control, and the capacity to outlast objection are the operative forces, and roughly 60% of it is structural (who controls closure) while roughly 40% is internalized (participants' belief in the process's fairness discourages escalation before it starts) — the split is carried as an uncertainty in the omegas rather than resolved in the scalar. Theater ratio (0.45) reflects the growing ritual share: hums and consensus calls increasingly ratify outcomes already settled off-floor, though genuine deliberation persists in less contested work. Accessibility collapse (0.45): alternatives exist (other SDOs, consortia, unilateral deployment) but are costly once interoperability expectations anchor on IETF ratification, so alternatives are degraded, not eliminated. Resistance (0.55): mailing-list pushback, appeal attempts, splinter efforts, and public criticism are recurrent and organized enough to impose real costs. The temporal series run on one shared nine-point grid (every tracked metric authored at every point, endpoints matching the scalar base_properties values); the underlying dynamics are a rising trend punctuated by episodic controversy spikes, folded here into the monotone series rather than modeled as oscillation, since the spikes are reactions to specific decisions rather than a self-sustaining cycle. Coalition potential among the payer seats is limited by collective-action costs: independent implementers and unaffiliated participants share interests but lack a coordination venue that the process itself does not control.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the hyperscaler delegation seat, the procedure is a productive coordination surface: participation costs are budgeted line items and ratified outcomes are assets, so effective extraction sits near the beneficiary end. From the unaffiliated participant seat, the same procedure operates as the steady conversion of volunteered labor and voiced objection into documents they must implement anyway, with exit priced in identity rather than logistics. From the chair seat, the procedure is an administrative burden carried under scrutiny from every side, with personal exposure when a contested call goes wrong. The depleted credibility commons is experienced by no single seat directly, which is precisely why no seat rations its withdrawal. The engine derives these divergent classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (hyperscaler delegations, incumbent equipment vendors) derive low directionality — the arrangement subsidizes them with ratified legitimacy. Payers (independent implementers, unaffiliated participants) derive high directionality, amplified for the identity_locked seat, whose trapped-and-fused position places it near the full-target end despite its moderate nominal power. The chairs derive near-symmetric from their administrator role, with their capture exposure (employer overlap with the largest delegations) noted in commentary rather than forced through an override, because the available override keys on power_atom alone and would simultaneously distort the two institutional beneficiaries that share that atom. The legitimacy commons is authored with agent=false and therefore contributes to the story's structure without entering the d-to-chi derivation — a depleted commons must not masquerade as a paying agent. No directionality_overrides are declared: the beneficiary/victim declarations plus exit differentiation already produce the correct qualitative ordering, and the one candidate correction (chair capture tilt) cannot be expressed at the required granularity without collateral distortion.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what prevents mislabeling in both directions. Reading the arrangement as pure rope (the stewardship temptation) hides the asymmetric transfer: the coordination function is genuine — thousands of dispersed volunteers do get aggregated into interoperable specifications — but the same closure machinery that aggregates judgment also converts resource advantage into ratified outcomes. Reading it as pure snare (the substrate temptation) erases the real output: the process still produces specifications the internet runs on, and its persistence does not depend solely on suppressing exits. Mandatrophy risk here is subtler than usual: the founding problem (fragmented proprietary protocols) remains live, so there is no dead mandate to declare resolved — the danger is instead premature satisfaction, the inference that because standards still ship, the mechanism still works as designed. The rising theater_ratio and suppression_requirement series are the countersignal: increasing shares of process activity defend the procedure's appearance rather than perform its function, which is the signature of a coordination structure quietly rebalancing toward its extraction component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which reading of the ietf_openness_commitment kernel does the standing arrangement actually instantiate — stewardship of a commons, substrate for encoded gatekeeping, or an eroding legitimacy mechanism — and is the choice resolvable at all, or indexical to the observing seat?',
    'Cross-reading comparison on the shared referent: audit the same corpus of contested decisions under each reading''s diagnostic criteria and check whether the classifications converge; persistent divergence marks the disagreement as located in what counts as the constraint''s primary function.',
    'Sibling readings assign materially different epsilon and different victim sets to the identical procedure; resolving (or proving irresolvable) the reading choice determines whether reform targets safeguard design, resource parity, or legitimacy accounting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'This constraint is one reading of a contested kernel; the reading choice itself is the deepest open variable.').

omega_variable(
    capture_incidence_vs_perception,
    'Is the measured extractiveness driven by actual distortion of technical outcomes toward factional positions, or by heightened perception of capture that erodes credibility regardless of outcomes?',
    'Outcome-level audit: code contested working-group decisions for alignment with the resource-advantaged faction''s pre-discussion position versus the post-discussion technical merits, blind to authorship.',
    'If distortion is real, remedies target decision mechanics; if perception leads reality, the extraction is partly self-fulfilling and legitimacy repair (transparency, explanation of chair reasoning) matters more than procedural redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_incidence_vs_perception, empirical, 'Whether the credibility drain tracks real outcome capture or capture beliefs.').

omega_variable(
    hum_decisiveness_audit,
    'How decisive is the hum and consensus-call machinery in contested outcomes, as distinct from chair judgment exercised off-floor?',
    'Reconstruct close decisions from meeting minutes, jabber logs, and list archives; compare hum results with the chair''s stated consensus determination and with subsequent document revisions.',
    'If the hum is largely decorative in close cases, theater_ratio overstates the ritual share of actual decision force and the binding mechanism is chair discretion — changing which safeguard reforms could matter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hum_decisiveness_audit, empirical, 'Whether the audible-consensus instruments bind or merely perform.').

omega_variable(
    volunteer_identity_lock_persistence,
    'How long does the unaffiliated core''s mission-identity fusion continue to supply unpaid expert labor after repeated adverse contested closures?',
    'Longitudinal participation data: retention curves of individual (non-corporate-affiliated) contributors following high-controversy decisions, compared with retention after ordinary decisions.',
    'If identity lock breaks, the free-labor subsidy collapses and the process becomes overtly professionalized — shifting the constraint''s persistence basis from volunteer goodwill to paid participation and likely raising effective extraction further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(volunteer_identity_lock_persistence, empirical, 'Durability of the identity-bound labor supply under legitimacy erosion.').

omega_variable(
    formalization_tradeoff,
    'Would formalizing the decision rule (weighted voting, enumerated constituencies) suppress organized capture, or destroy the informal volunteer economy that produces the review labor the process runs on?',
    'Comparative SDO analysis: participation and output trajectories of standards bodies that formalized voting versus those retaining consensus norms, controlling for domain and era.',
    'If formalization kills volunteering, the current arrangement''s extraction is partly the price of its productivity and the tangled-rope reading stabilizes; if it does not, the refusal to formalize is itself maintenance of the extraction channel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(formalization_tradeoff, conceptual, 'Whether the cure for capture is affordable within the process''s own economy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ietf_tr_t4, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(ietf_tr_t8, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(ietf_tr_t12, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(ietf_tr_t16, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(ietf_tr_t24, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(ietf_tr_t28, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 28, 0.41).
narrative_ontology:measurement(ietf_tr_t32, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 32, 0.45).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ietf_be_t4, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(ietf_be_t8, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(ietf_be_t12, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(ietf_be_t16, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ietf_be_t24, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(ietf_be_t28, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 28, 0.67).
narrative_ontology:measurement(ietf_be_t32, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 32, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ietf_su_t4, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 4, 0.3).
narrative_ontology:measurement(ietf_su_t8, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(ietf_su_t12, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(ietf_su_t16, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(ietf_su_t24, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(ietf_su_t28, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 28, 0.57).
narrative_ontology:measurement(ietf_su_t32, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 32, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__capture_substrate_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the IETF's openness commitment' conflates three structurally distinct claims about the same standing arrangement (the rough-consensus procedure). This file instantiates the legitimacy-erosion reading: the mechanism itself is contested and vulnerable to organized capture despite procedural safeguards, with high epsilon over the fixed referent (the procedure as operated) and the credibility commons as the depleted asset. The commons-stewardship reading authors low epsilon over the same referent (public-infrastructure preservation); the capture-substrate reading authors high epsilon with a structural-determinism diagnosis (resource advantage encoded as gatekeeping). Per DP-001 each reading is a separate constraint with its own epsilon, beneficiaries, and victims; the files are linked through affects_constraints, and the upstream/downstream ordering runs stewardship -> erosion -> substrate in decreasing faith in the procedure's self-correction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
