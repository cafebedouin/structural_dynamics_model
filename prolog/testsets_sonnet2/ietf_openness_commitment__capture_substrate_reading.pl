% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__capture_substrate_reading, []).

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
 *   constraint_id: ietf_openness_commitment__capture_substrate_reading
 *   human_readable: IETF Rough-Consensus Process as Resource-Advantage Gatekeeping Substrate
 *   domain: technology governance/internet standards/institutional economics
 *
 * SUMMARY:
 *   This story is the capture-substrate reading of the IETF openness
 *   commitment kernel: the claim that the rough-consensus, running-code
 *   process — while genuinely coordination-functional at its core — has
 *   become a substrate through which resource advantage systematically
 *   converts into encoded technical outcomes. Large platform operators do not
 *   need to break any procedural rule; sustained, salaried presence at
 *   meetings and mailing lists over years is itself the mechanism, since
 *   'rough consensus' is measured by who is durably present and vocal, not by
 *   representative weight across the implementer population. Proprietary
 *   extensions get shipped ahead of ratification, then cited as deployed
 *   reality the standard should reflect. This reading holds the ε referent
 *   fixed on the standing arrangement — the current IETF process as it
 *   actually operates, not any reformed alternative — and reports its own
 *   view of that arrangement's extraction, independent of the
 *   commons-stewardship and legitimacy-erosion siblings.
 *
 * KEY AGENTS:
 *   - large_platform_operators: structural beneficiary, sustained institutional presence converts into drafting and chairing control
 *   - well_resourced_vendor_consortiums: coalition beneficiary, pooled resources produce bloc-like influence despite formal individual participation
 *   - small_implementers: payer, arrives after substantive decisions are locked, must implement text shaped by others
 *   - independent_open_source_maintainers: payer, bears volunteer maintenance cost of complexity added to formalize dominant vendor behavior
 *   - downstream_end_users: payer, absent from the process entirely, experiences captured standards only as software behavior
 *   - ietf_leadership_and_area_directors: agenda-setter, administers the process, identity bound to its legitimacy
 *   - excluded_regional_and_language_communities: excluded, structurally unable to participate in the meeting/mailing-list cadence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.56).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.48).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Rough-Consensus Process as Resource-Advantage Gatekeeping Substrate").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology governance/internet standards/institutional economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, '1eae02f9-2ec8-46c7-b3e4-3b992c3e30d5').
narrative_ontology:cs_kernel_codification('1eae02f9-2ec8-46c7-b3e4-3b992c3e30d5', distributed).
narrative_ontology:cs_authority_grounding('1eae02f9-2ec8-46c7-b3e4-3b992c3e30d5', practice).
narrative_ontology:cs_interpretation_layer_present('1eae02f9-2ec8-46c7-b3e4-3b992c3e30d5').
narrative_ontology:cs_reading_relation('1eae02f9-2ec8-46c7-b3e4-3b992c3e30d5', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('1eae02f9-2ec8-46c7-b3e4-3b992c3e30d5', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('1eae02f9-2ec8-46c7-b3e4-3b992c3e30d5', foundational, sustained_participation_capacity_determines_de_facto_authorship).
narrative_ontology:cs_axiom_status(sustained_participation_capacity_determines_de_facto_authorship, holdable).
narrative_ontology:cs_axiom_grounding('1eae02f9-2ec8-46c7-b3e4-3b992c3e30d5', sustained_participation_capacity_determines_de_facto_authorship, empirically_contingent).
narrative_ontology:cs_axiom('1eae02f9-2ec8-46c7-b3e4-3b992c3e30d5', secondary, formal_openness_without_participation_cost_equalization_is_insufficient_for_substantive_openness).
narrative_ontology:cs_axiom_status(formal_openness_without_participation_cost_equalization_is_insufficient_for_substantive_openness, holdable).
narrative_ontology:cs_axiom_grounding('1eae02f9-2ec8-46c7-b3e4-3b992c3e30d5', formal_openness_without_participation_cost_equalization_is_insufficient_for_substantive_openness, conventional).
narrative_ontology:cs_reference_frame('1eae02f9-2ec8-46c7-b3e4-3b992c3e30d5', rough_consensus_running_code_meritocratic_ideal).
narrative_ontology:cs_drift_state('1eae02f9-2ec8-46c7-b3e4-3b992c3e30d5', contemporary_platform_dominance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1eae02f9-2ec8-46c7-b3e4-3b992c3e30d5', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, well_resourced_vendor_consortiums).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, independent_open_source_maintainers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, downstream_end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Send salaried engineers to sit in working groups for years, chair the groups that matter, author the drafts that become base texts, and run the interop test suites vendors are measured against. Ship proprietary extensions ahead of ratification, then argue rough consensus should follow deployed reality — their own deployment. Absorb the multi-year time cost of standards participation as a routine operating expense; competitors without that budget cannot even be present at the table long enough to object.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, agenda_setter).

% Pool travel budgets, dedicated standards staff, and legal review capacity across member companies to sustain presence at every meeting cycle. Trade support for extensions across working groups, effectively voting as a bloc under the appearance of independent rough consensus. Can walk from any single standard to a competing venue (a different SDO, a de facto industry cartel) if a given process stops serving them — an exit option small implementers structurally lack.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, well_resourced_vendor_consortiums, beneficiary,
    organized, generational, mobile, global).

% Cannot fund a full-time standards presence; participate episodically via mailing lists and occasional travel, arriving after major textual decisions are already locked in working-group folklore. Must implement to whatever text emerges, including extensions whose interop tests were built around the large operators' existing deployments. Nominal exit exists (don't implement the standard) but forfeits interoperability with the dominant platforms their users already expect.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_implementers, payer,
    moderate, biographical, constrained, national).

% Volunteer or grant-funded, maintain reference implementations that must track whatever drafts pass rough consensus, including last-minute extension text inserted by well-staffed participants during the final call window when volunteer reviewers have the least bandwidth. Carry the maintenance burden of complexity added primarily to formalize platform-specific behavior. Cannot decline to track the standard without fragmenting the ecosystem their users depend on.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, independent_open_source_maintainers, payer,
    powerless, biographical, trapped, global).

% Experience the standard only as whatever behavior their software exhibits; have no seat in the process and no visibility into which extensions originated as proprietary vendor features later blessed as 'open.' Bear switching costs and lock-in effects when the standardized surface quietly encodes a dominant vendor's implementation choices as the only well-supported path.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, downstream_end_users, payer,
    powerless, immediate, trapped, global).

% Administer working-group chartering, chair selection, and the rough-consensus call itself. Committed by professional and institutional identity to the legitimacy of the process they run; declaring it captured would undercut the authority they personally embody. Have formal power to intervene against dominance by any one participant but rarely exercise it against major sponsors whose staff time and test infrastructure the process has come to depend on operationally.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_leadership_and_area_directors, agenda_setter,
    institutional, generational, identity_locked, global).

% Would raise concerns about encoding assumptions (character sets, addressing conventions, deployment topologies) that fit dominant-market conditions poorly, but lack the English-fluency, travel budget, or timezone alignment to participate in the mailing-list and meeting cadence where rough consensus is actually formed.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, excluded_regional_and_language_communities, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__capture_substrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The process genuinely solves a real coordination problem: without a shared specification body, competing implementations of network protocols would fragment and interoperability across the internet would collapse into incompatible vendor silos.
% TRANSFER_FUNCTION: Moves the cost of specification labor (drafting, testing, editing across years of meeting cycles) from all implementers collectively onto whichever participants can afford sustained presence, and in exchange moves control over the specification's substantive content to those same well-resourced participants — who then externalize the complexity and adaptation cost of their preferred design onto smaller implementers and end users who had no seat in shaping it.
% ABSENT_VOICES: Small implementers arrive late to drafts already substantively settled; independent maintainers are structurally unable to attend in person; regional and language communities whose deployment conditions differ from the dominant market are effectively absent from the room where rough consensus is measured by who is present and vocal, not by representative weight.
% DISAPPEARANCE_RATIONALE: Large operators dispute that anything would change if the current process vanished — they would simply coordinate bilaterally or through a captured successor body, since their advantage is resource-based, not procedural. Small implementers and independent maintainers argue the opposite: that even an imperfect open process with public mailing lists and non-veto rough consensus is the only leverage they have against pure vendor fiat, and its disappearance would remove even the residual check that publicity and process delay currently impose.
% FOUNDING_PROBLEM: In the absence of a shared, vendor-neutral specification process, the early internet risked exactly the fragmentation into incompatible proprietary networking stacks that later characterized enterprise LAN protocols — a genuine coordination failure that rough consensus and running code were built to solve.
% FOUNDING_PROBLEM_CORROBORATION: IETF leadership and long-tenured participants attest the founding problem remains live and the process still functions as intended. Independent researchers studying standards-body capture (documented in academic STS literature on SDO participation asymmetry, and in antitrust testimony from smaller vendors in adjacent standards bodies) attest from outside the benefiting operators that participation cost has become a de facto capture mechanism even where formal rules remain unchanged — corroboration exists outside the beneficiary set, though it is contested by the operators themselves.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, contested).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__capture_substrate_reading, 0.56, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__capture_substrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__capture_substrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.56) is moderate rather than severe because the coordination function is genuine and most published standards remain broadly useful — the extraction is concentrated in specific contested extensions and API surfaces, not the entire body of specifications. Suppression (0.48) reflects that no one is formally barred from participating; the barrier is resource-based and procedural-attrition-based rather than coercive exclusion, which keeps it well below snare-level suppression. Theater ratio (0.42) captures that a meaningful share of 'open participation' — the public mailing list, the call for comments — functions increasingly as legitimating performance around decisions substantially settled in advance by well-resourced participants' private coordination. Accessibility collapse (0.5) is moderate: alternatives (competing venues, forking implementations) exist but are costly. Resistance (0.58) is elevated because small implementers, IETF process reformers, and academic critics actively contest the pattern — this is not a quiet, unresisted arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the large-operator seat, the process is functioning exactly as designed: showing up consistently and doing the drafting work IS the legitimate mechanism, and calling it capture merely describes losing an open competition for influence that anyone could enter. From the small-implementer and independent-maintainer seats, the same formal openness computes as extraction because the entry cost to meaningfully influence outcomes has become directly proportional to standing institutional resources, converting a nominally participatory process into a de facto plutocracy of attention.
 *
 * DIRECTIONALITY LOGIC:
 *   Large platform operators and vendor consortiums are declared beneficiaries because they capture control over specification content proportional to sustained investment, and can exit to alternative venues if a given SDO stops serving them (near-beneficiary d). Small implementers, independent maintainers, and end users are declared victims: they bear the downstream complexity and adaptation cost of decisions made without their effective participation, and their exit options range from constrained (small implementers, who can decline to implement at real interoperability cost) to trapped (end users and volunteer maintainers, who have no realistic alternative venue). IETF leadership sits as agenda-setter with identity-locked exit — the process's legitimacy is fused with their professional identity, making acknowledgment of capture personally costly regardless of what they privately observe.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (preventing protocol fragmentation) remains genuinely live — this is not a pure snare where the coordination story is empty cover. What has drifted is the distribution of who captures the specification-shaping value produced by that genuine coordination. Classifying this as tangled_rope rather than snare or rope preserves both facts simultaneously: real coordination value is produced AND it is asymmetrically captured through the same structure, with active enforcement (chairing decisions, agenda control, interop test design) required to sustain the capture. A pure snare classification would falsely deny the coordination function IETF still performs for the broader internet; a pure rope classification would falsely deny the resource-asymmetric capture this reading identifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_substrate_vs_commons_stewardship_framing,
    'Is the dominant structural fact about the IETF process that it successfully preserves open interoperability for all implementers (commons_stewardship_reading), or that resource asymmetry has converted its formal openness into a de facto gatekeeping mechanism for well-resourced participants (this reading)?',
    'Comparative analysis of draft authorship and extension-adoption outcomes weighted by sponsor participation intensity across a representative sample of working groups, plus interviews with small-implementer participants about perceived versus actual influence on contested extensions.',
    'If commons_stewardship evidence dominates (most standards show no correlation between sponsor investment and outcome capture), this reading''s tangled_rope classification would be overstated and the constraint would sit closer to a genuine rope; if capture-substrate evidence dominates, the classification understates extraction if anything.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_substrate_vs_commons_stewardship_framing, conceptual, 'Whether capture-substrate or commons-stewardship is the more accurate primary framing of the same standing process.').

omega_variable(
    capture_substrate_vs_legitimacy_erosion_locus,
    'Is the extraction this reading identifies already realized and ongoing (capture has happened), or is it better understood as a latent structural vulnerability that the legitimacy_erosion_reading identifies at the level of the consensus mechanism itself, not yet fully exploited?',
    'Historical case analysis of specific extensions alleged to be captured (e.g., specific transport or web protocol extensions where a single vendor''s implementation became the de facto normative reference) versus counter-cases where broad-based consensus visibly overrode dominant-vendor preferences.',
    'If realized capture is rare and isolated to a few high-profile cases, this reading''s moderate-extraction, tangled_rope classification may overstate the pattern''s generality relative to the more procedural legitimacy_erosion framing; if realized capture is systemic across working groups, this reading is the more accurate present-tense account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capture_substrate_vs_legitimacy_erosion_locus, empirical, 'Whether the capture this reading describes is a realized present pattern or a latent vulnerability better located in the legitimacy_erosion sibling.').

omega_variable(
    resource_advantage_naturalness,
    'Is unequal resource-based participation an unavoidable structural feature of any voluntary technical standards process (making the gatekeeping effect closer to inevitable friction), or is it a contingent design choice (meeting-heavy cadence, in-person weighting, lack of funded participation stipends) that could be substantially mitigated?',
    'Comparative study of standards bodies that have implemented participation-cost mitigations (funded travel for underrepresented implementers, asynchronous-first decision processes) against those that have not, measuring whether capture indicators change.',
    'If resource advantage is largely unavoidable, the tangled_rope classification is stable and reform efforts should focus on managing rather than eliminating asymmetry; if substantially mitigable, the current arrangement''s extraction is more clearly attributable to design choice rather than intrinsic process necessity, sharpening the case for reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_advantage_naturalness, conceptual, 'Whether resource-based influence asymmetry is intrinsic to voluntary standards processes or a mitigable design artifact of this specific process''s structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ietf_tr_t4, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(ietf_tr_t8, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(ietf_tr_t12, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(ietf_tr_t16, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(ietf_tr_t24, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(ietf_be_t4, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(ietf_be_t8, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(ietf_be_t12, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(ietf_be_t16, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(ietf_be_t24, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 24, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ietf_su_t4, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(ietf_su_t8, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(ietf_su_t12, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(ietf_su_t16, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(ietf_su_t24, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the ietf_openness_commitment kernel. commons_stewardship_reading authors substantially lower extraction for the same standing process, emphasizing preserved interoperability as the dominant structural fact. legitimacy_erosion_reading locates the concern one level up, in the vulnerability of rough-consensus itself to capture rather than in already-realized capture outcomes. All three share ε's referent (the current, contested IETF process) but author different ε, different beneficiary/victim structures, and different types from that shared referent, per the ε-invariance and reading-indexing principles (OQ-26).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
