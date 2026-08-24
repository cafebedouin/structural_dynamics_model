% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__legitimacy_erosion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   The IETF's rough consensus mechanism — 'we reject kings, presidents, and
 *   voting; we believe in rough consensus and running code' — is the kernel
 *   of internet standards legitimacy. This reading
 *   (legitimacy_erosion_reading) treats the mechanism itself as the
 *   constraint under contest: a coordination device that has become
 *   vulnerable to organized capture, where well-resourced factions extract
 *   the mechanism's procedural legitimacy to ratify self-serving outcomes.
 *   The victim is not a group of people but the legitimacy commons itself —
 *   the shared epistemic resource that makes 'rough consensus' a credible
 *   signal. Sibling readings: commons_stewardship_reading sees the mechanism
 *   as successfully preserving interoperability; capture_substrate_reading
 *   sees the standards process as a coordination substrate where resource
 *   advantage becomes encoded gatekeeping. This reading focuses specifically
 *   on the mechanism's credibility as the extraction target.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.68).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.55).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough Consensus Mechanism — Legitimacy Erosion Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, '413f6461-1467-4781-87b8-8e067b0edc3a').
narrative_ontology:cs_kernel_codification('413f6461-1467-4781-87b8-8e067b0edc3a', distributed).
narrative_ontology:cs_authority_grounding('413f6461-1467-4781-87b8-8e067b0edc3a', practice).
narrative_ontology:cs_interpretation_layer_present('413f6461-1467-4781-87b8-8e067b0edc3a').
narrative_ontology:cs_reading_relation('413f6461-1467-4781-87b8-8e067b0edc3a', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('413f6461-1467-4781-87b8-8e067b0edc3a', ietf_openness_commitment__capture_substrate_reading, influences).
narrative_ontology:cs_axiom('413f6461-1467-4781-87b8-8e067b0edc3a', foundational, legitimacy_commons_is_depletable).
narrative_ontology:cs_axiom_status(legitimacy_commons_is_depletable, holdable).
narrative_ontology:cs_axiom_grounding('413f6461-1467-4781-87b8-8e067b0edc3a', legitimacy_commons_is_depletable, empirically_contingent).
narrative_ontology:cs_axiom('413f6461-1467-4781-87b8-8e067b0edc3a', foundational, procedural_safeguards_are_capture_vectors).
narrative_ontology:cs_axiom_status(procedural_safeguards_are_capture_vectors, holdable).
narrative_ontology:cs_axiom_grounding('413f6461-1467-4781-87b8-8e067b0edc3a', procedural_safeguards_are_capture_vectors, empirically_contingent).
narrative_ontology:cs_axiom('413f6461-1467-4781-87b8-8e067b0edc3a', secondary, running_code_bias_privileges_incumbents).
narrative_ontology:cs_axiom_status(running_code_bias_privileges_incumbents, holdable).
narrative_ontology:cs_axiom_grounding('413f6461-1467-4781-87b8-8e067b0edc3a', running_code_bias_privileges_incumbents, empirically_contingent).
narrative_ontology:cs_reference_frame('413f6461-1467-4781-87b8-8e067b0edc3a', rough_consensus_as_legitimacy_warrant).
narrative_ontology:cs_drift_state('413f6461-1467-4781-87b8-8e067b0edc3a', post_consolidation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('413f6461-1467-4781-87b8-8e067b0edc3a', '2026-08-15T14:32:00Z').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, large_platform_vendors).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, organized_standards_caucuses).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_protocol_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, legitimacy_commons).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, small_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, new_entrants).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, global_south_participants).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__legitimacy_erosion_reading, rough_consensus_as_legitimacy_source).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__legitimacy_erosion_reading, open_standards_require_inclusive_process).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major technology companies (cloud providers, browser vendors, CDN operators) that deploy IETF standards at planetary scale. They fund large delegations, host meetings, employ standards editors, and shape working group charters. Their scale lets them absorb participation costs that exclude smaller actors. They benefit when the consensus mechanism ratifies designs that align with their deployed infrastructure, and they can block or delay designs that threaten it. Exit is trivial — they can implement proprietary alternatives or ignore standards entirely.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, large_platform_vendors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__legitimacy_erosion_reading, large_platform_vendors, agenda_setter).

% Coordinated groups of participants (corporate-aligned, national-body-aligned, or ideological) that operate across working groups to advance shared positions. They exploit procedural familiarity, mailing list dynamics, and humming rituals to create the appearance of consensus where none exists. Their cohesion lets them outlast opposition in multi-year processes. Exit is constrained — leaving the caucus loses the coordination advantage, but the caucus itself persists.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, organized_standards_caucuses, beneficiary,
    organized, biographical, constrained, global).

% Organizations with widely deployed implementations of existing protocols. They benefit when the consensus mechanism resists changes that would require costly migration, and when new standards extend their incumbent advantage. They participate selectively — engaging only when their interests are directly threatened. Exit is mobile — they can comply, ignore, or fork — but their installed base gives them de facto veto power.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, incumbent_protocol_implementers, beneficiary,
    powerful, biographical, mobile, global).

% The shared epistemic resource that makes 'rough consensus' a credible signal of technical merit and broad acceptance. Every capture event — every humming that silences dissent, every charter that narrows scope to exclude alternatives, every appeal to 'running code' that privileges incumbents — draws down this commons. It cannot exit; it is the thing being depleted. Its degradation makes future consensus harder to achieve and easier to dismiss.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, legitimacy_commons, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ietf_openness_commitment__legitimacy_erosion_reading, legitimacy_commons).

% Startups, open-source projects, academic groups, and niche vendors that implement standards but lack the resources for sustained IETF participation. They bear the cost of standards shaped by others' priorities — unnecessary complexity, missing features, architectural decisions that favor scale they don't have. They can exit by not implementing, but then they lose interoperability; they can implement differently, but then they fragment the ecosystem. Their constrained exit makes them price-takers in the consensus economy.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, small_implementers, payer,
    moderate, biographical, constrained, global).

% Entities attempting to enter markets governed by IETF standards — new browser engines, new transport protocols, new identity systems. They face a consensus mechanism that structurally favors the already-deployed. The 'running code' requirement becomes a moat: you need consensus to get deployment, but you need deployment to prove running code. They are trapped — the only path through is the very mechanism that excludes them.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, new_entrants, payer,
    powerless, immediate, trapped, global).

% Participants from regions with limited travel budgets, time-zone disadvantages, language barriers, and fewer institutional affiliations. The IETF's meeting-centric, English-language, mailing-list culture systematically disadvantages them. Their identity_locked exit reflects that the standards *are* the global interoperability layer — they cannot build a separate internet, so they must engage with a process that structurally marginalizes them. Their absence from consensus is not voluntary; it is the extraction.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, global_south_participants, payer,
    powerless, generational, identity_locked, global).

% The Internet Engineering Steering Group and the LLC that administers the IETF. They set working group charters, appoint area directors, manage the appeals process, and control procedural interpretation. They are the formal guardians of the consensus mechanism. Their legitimacy depends on the mechanism's credibility — which creates a tension: they must defend the process against capture, but their defensive actions (stricter process, more bureaucracy) can themselves become capture vectors. They are analytically positioned but institutionally embedded.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, ietf_leadership_iesg_llc, agenda_setter,
    institutional, generational, analytical, global).

% Recognized technical authorities who participate for merit rather than institutional mandate. They provide the 'rough' in rough consensus — the technical judgment that is supposed to ground the humming. Their credibility is the mechanism's asset; their co-option is its risk. They can exit to other venues (academia, other SDOs, private practice) but their departure signals mechanism failure. Mobile exit, high signaling value.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, independent_technical_experts, observer,
    moderate, biographical, mobile, global).

% Government and intergovernmental bodies (EU, ITU, national regulators) that treat IETF standards as de facto regulatory baselines. They observe whether the consensus mechanism produces outcomes compatible with public policy — competition, security, human rights. They do not participate in humming but their shadow falls over every working group. Their analytical seat is the external discipline that the mechanism claims to not need.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__legitimacy_erosion_reading, regulatory_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rough consensus coordinates decentralized protocol development across competing implementations by providing a lightweight, implementation-biased decision procedure that avoids formal voting and its strategic distortions. It solves: how do thousands of engineers across hundreds of organizations agree on technical specifications without gridlock or capture?
% TRANSFER_FUNCTION: The mechanism transfers procedural legitimacy — the credible signal that a specification represents broad technical agreement — from the legitimacy commons to the beneficiaries of each consensus decision. Well-resourced factions extract this legitimacy to ratify designs that serve their deployed base, locking in advantages. The cost is paid by the commons (degraded credibility), small implementers (standards that don't fit), new entrants (barriers to entry), and global south participants (exclusion from the room where legitimacy is minted).
% ABSENT_VOICES: The vast majority of internet users — billions of people whose data flows through IETF protocols — have no representation. Civil society organizations focused on digital rights attend sporadically but lack sustained presence. Academic researchers who could provide independent technical assessment are structurally excluded by the 'running code' bias. Future implementers — those who don't exist yet — cannot object to decisions that will constrain them.
% DISAPPEARANCE_RATIONALE: If the rough consensus mechanism vanished overnight, the internet would not stop routing packets — but the process for evolving its protocols would collapse into either vendor-driven fragmentation (each major platform pushes proprietary extensions) or heavyweight formal standardization (ITU-style, with state voting). The specific coordination achievement — a single, globally deployed protocol suite evolved by a loose technical community — would be lost. The world would rearrange around either corporate fiefdoms or state control.
% FOUNDING_PROBLEM: The early internet needed a way to standardize protocols without the bureaucratic overhead and national-body voting of traditional SDOs (ISO, CCITT). The founding problem was: how to achieve technical coordination at speed and scale, grounded in implementation experience rather than paper specifications, while remaining open to any participant.
% FOUNDING_PROBLEM_CORROBORATION: IETF leadership and long-time participants attest the founding problem remains live — the internet still needs lightweight, implementation-driven coordination. Critics from the global south, civil society, and academic internet research attest the problem has shifted: the internet is now critical infrastructure, the stakes are political-economic not just technical, and the mechanism's openness is largely performative. The corroboration split maps exactly to the beneficiary/payer divide in this story.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__legitimacy_erosion_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__legitimacy_erosion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ietf_openness_commitment__legitimacy_erosion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__legitimacy_erosion_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.68) reflects that the mechanism now reliably produces outcomes aligned with the interests of large platform vendors and organized caucuses, not the broad technical merit it claims to measure. The 1992-2026 trajectory shows steady accumulation: early IETF had minimal extraction (small community, low stakes); commercialization brought vendor delegations; the web's rise made standards high-stakes commercial assets; mobile and cloud concentrated deployment power. Suppression (0.55) is moderate — the mechanism doesn't formally forbid participation, but structural barriers (travel, time zones, procedural complexity, 'running code' requirement) functionally suppress dissent. Theater ratio (0.42) captures the growing gap between the ritual (humming, mailing list debate, working group last calls) and the reality (pre-negotiated positions, caucus discipline, charter scope manipulation). Accessibility collapse (0.38) is not total — alternatives exist (other SDOs, proprietary protocols, forks) — but they are costly enough that most actors stay. Resistance (0.61) is significant: appeals, dissents, competing implementations, external criticism — but resistance is fragmented and the mechanism absorbs it without structural change.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (IETF leadership), the mechanism is a genuine coordination achievement under pressure — they see the procedural safeguards working, the appeals process functioning, the openness real. From the payer seats (small implementers, new entrants, global south), the same safeguards are the capture machinery — charters that narrow scope, humming that silences, 'running code' that privileges incumbents. The engine computes this divergence from the structural data: same constraint, different power/exit/spatial_scope profiles yield different effective extraction. The claimed_type (tangled_rope) reflects this authoring-seat judgment: genuine coordination function + asymmetric extraction + active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Large platform vendors and organized caucuses are structural beneficiaries (d near 0.0-0.2): they collect the legitimacy rent, control the agenda, and face trivial exit. Incumbent implementers are partial beneficiaries (d ~0.3): they gain from stability but pay compliance costs. The legitimacy commons is the ultimate payer (d = 1.0 analytically) — it is the resource being extracted. Small implementers, new entrants, and global south participants are payers with high d (0.7-0.9): they bear costs of misfit standards, barriers to entry, and structural exclusion. Their exit options (constrained, trapped, identity_locked) amplify effective extraction. IETF leadership and independent experts sit near symmetric (d ~0.4-0.5): they both maintain and are constrained by the mechanism. Regulatory observers are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (lightweight implementation-driven coordination) was live in 1992. By 2026, the problem has mutated: the internet is critical infrastructure, standards are regulatory baselines, and the mechanism's openness is exploited by actors the founders never imagined. The mandate has not atrophied — the coordination need is greater than ever — but the mechanism has not adapted to the new threat model. This is not mandatrophy (purpose gone, structure remains); it is mandate-capture (purpose persists, structure hijacked). The tangent: if the mechanism cannot distinguish legitimate coordination from organized capture, it loses the legitimacy that makes it a coordination mechanism at all. That is the extraction spiral this reading measures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_commons_measurement,
    'Can the legitimacy commons — the shared epistemic resource that makes ''rough consensus'' a credible signal — be measured independently of the consensus outcomes it legitimates?',
    'Longitudinal study of citation/appeal patterns: when a working group claims ''rough consensus,'' how often is that claim accepted without challenge vs. contested? Track the decay of ''rough consensus'' as a warrant in IETF discourse and in external references (regulatory filings, academic papers, vendor marketing).',
    'If the commons is measurably depleted, the mechanism''s extractiveness is higher than current metrics capture — each consensus decision draws down a finite credibility reservoir. If the commons is stable, the mechanism may be a sustainable tangled_rope rather than a degrading one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_commons_measurement, empirical, 'Whether the legitimacy commons can be quantified and tracked as a depletable resource.').

omega_variable(
    coordination_capture_boundary,
    'Where is the structural boundary between legitimate coordination (rough consensus working as designed) and capture (rough consensus ratifying pre-determined outcomes)?',
    'Counterfactual analysis of contested working groups: for each case where consensus was claimed but challenged, reconstruct the participation network, funding flows, and pre-meeting coordination. Identify the structural markers that distinguish genuine convergence from manufactured consent.',
    'If the boundary is porous (no reliable markers), the mechanism is structurally incapable of self-distinguishing coordination from capture — it is a snare masquerading as a rope. If the boundary is sharp, the mechanism can be repaired by strengthening the markers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_capture_boundary, conceptual, 'Whether the mechanism has an internal structural distinction between its coordination and capture modes.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the legitimacy_erosion_reading logically foreclose the commons_stewardship_reading, or do they coexist as competing framings of the same kernel?',
    'Test whether a single institutional framework could simultaneously hold: (a) rough consensus is a credible legitimacy signal that preserves interoperability (stewardship), and (b) rough consensus is a credibility reservoir being depleted by organized capture (erosion). If both can be maintained without contradiction, they coexist; if (b) entails the negation of (a), erosion forecloses stewardship.',
    'If forecloses, the kernel has a structural fracture — the commitment cannot sustain both readings. If coexists_with, the kernel supports a persistent interpretive dispute that the mechanism must mediate. This determines whether the kernel itself is stable or in crisis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Structural relationship between this reading and the commons_stewardship_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 1992, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_legitimacy_erosion_tr_t1992, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 1992, 0.05).
narrative_ontology:measurement(ietf_legitimacy_erosion_tr_t1998, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 1998, 0.08).
narrative_ontology:measurement(ietf_legitimacy_erosion_tr_t2004, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2004, 0.15).
narrative_ontology:measurement(ietf_legitimacy_erosion_tr_t2010, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(ietf_legitimacy_erosion_tr_t2016, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2016, 0.33).
narrative_ontology:measurement(ietf_legitimacy_erosion_tr_t2020, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(ietf_legitimacy_erosion_tr_t2026, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(ietf_legitimacy_erosion_be_t1992, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 1992, 0.12).
narrative_ontology:measurement(ietf_legitimacy_erosion_be_t1998, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 1998, 0.18).
narrative_ontology:measurement(ietf_legitimacy_erosion_be_t2004, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2004, 0.28).
narrative_ontology:measurement(ietf_legitimacy_erosion_be_t2010, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(ietf_legitimacy_erosion_be_t2016, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2016, 0.55).
narrative_ontology:measurement(ietf_legitimacy_erosion_be_t2020, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(ietf_legitimacy_erosion_be_t2026, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ietf_legitimacy_erosion_su_t1992, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 1992, 0.15).
narrative_ontology:measurement(ietf_legitimacy_erosion_su_t1998, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 1998, 0.22).
narrative_ontology:measurement(ietf_legitimacy_erosion_su_t2004, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2004, 0.31).
narrative_ontology:measurement(ietf_legitimacy_erosion_su_t2010, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2010, 0.42).
narrative_ontology:measurement(ietf_legitimacy_erosion_su_t2016, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2016, 0.48).
narrative_ontology:measurement(ietf_legitimacy_erosion_su_t2020, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(ietf_legitimacy_erosion_su_t2026, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__legitimacy_erosion_reading, 0.12).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__capture_substrate_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, rfc_editor_model).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, iana_stewardship_transition).

% DUAL FORMULATION NOTE:
% This reading is one of three in the ietf_openness_commitment constraint family. The commons_stewardship_reading treats the kernel as a Mountain (negligible extraction, genuine coordination). The capture_substrate_reading treats it as a Tangled Rope (coordination substrate with encoded gatekeeping). This legitimacy_erosion_reading treats the mechanism itself as a Tangled Rope whose coordination function is real but whose legitimacy signal is the extraction target. All three share the same kernel but instantiate different constraints with different ε values, stakeholder structures, and drift trajectories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ietf_openness_commitment__legitimacy_erosion_reading, institutional, 0.15).
constraint_indexing:directionality_override(ietf_openness_commitment__legitimacy_erosion_reading, organized, 0.25).
constraint_indexing:directionality_override(ietf_openness_commitment__legitimacy_erosion_reading, powerful, 0.35).
constraint_indexing:directionality_override(ietf_openness_commitment__legitimacy_erosion_reading, moderate, 0.75).
constraint_indexing:directionality_override(ietf_openness_commitment__legitimacy_erosion_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
