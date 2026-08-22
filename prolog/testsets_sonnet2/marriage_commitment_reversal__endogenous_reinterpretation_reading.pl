% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto as Endogenous Divine Reinterpretation (Woodruff's Vision Reading)
 *   domain: religious institutional history / political theology / commitment systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel around the
 *   1890 Manifesto: the endogenous-reinterpretation reading, in which Wilford
 *   Woodruff's September 23 vision is treated, on its own terms, as a genuine
 *   internal divine revelation reinterpreting God's will under changed
 *   circumstances — not as a euphemism for external coercion (the sibling
 *   exogenous_override_reading) and not as a doctrine-practice gap left
 *   structurally unresolved (the sibling practice_doctrine_gap). Under this
 *   reading, the coordination function is real: the institution needed a
 *   mechanism to change course without conceding that revelation is
 *   reversible by outside force, and the revelatory frame supplies exactly
 *   that mechanism. The extraction is moderate rather than severe because the
 *   reading grants the revelation's sincerity; the cost it imposes falls on
 *   plural families whose status changed without their voice, and on the
 *   coherence of the doctrine itself, which must explain how an 'eternal
 *   principle' became suspendable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.52).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "1890 Manifesto as Endogenous Divine Reinterpretation (Woodruff's Vision Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious institutional history / political theology / commitment systems").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'c983f7ba-0e64-4dc6-9f10-549a0ebb628a').
narrative_ontology:cs_kernel_codification('c983f7ba-0e64-4dc6-9f10-549a0ebb628a', formalized).
narrative_ontology:cs_authority_grounding('c983f7ba-0e64-4dc6-9f10-549a0ebb628a', lineage).
narrative_ontology:cs_interpretation_layer_present('c983f7ba-0e64-4dc6-9f10-549a0ebb628a').
narrative_ontology:cs_reading_relation('c983f7ba-0e64-4dc6-9f10-549a0ebb628a', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('c983f7ba-0e64-4dc6-9f10-549a0ebb628a', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('c983f7ba-0e64-4dc6-9f10-549a0ebb628a', foundational, revelation_is_the_operative_cause_of_reversal).
narrative_ontology:cs_axiom_status(revelation_is_the_operative_cause_of_reversal, holdable).
narrative_ontology:cs_axiom_grounding('c983f7ba-0e64-4dc6-9f10-549a0ebb628a', revelation_is_the_operative_cause_of_reversal, theological).
narrative_ontology:cs_axiom('c983f7ba-0e64-4dc6-9f10-549a0ebb628a', foundational, current_prophetic_authority_supersedes_prior_canonical_command).
narrative_ontology:cs_axiom_status(current_prophetic_authority_supersedes_prior_canonical_command, holdable).
narrative_ontology:cs_axiom_grounding('c983f7ba-0e64-4dc6-9f10-549a0ebb628a', current_prophetic_authority_supersedes_prior_canonical_command, conventional).
narrative_ontology:cs_reference_frame('c983f7ba-0e64-4dc6-9f10-549a0ebb628a', section_132_eternal_principle_doctrine).
narrative_ontology:cs_drift_state('c983f7ba-0e64-4dc6-9f10-549a0ebb628a', post_manifesto_consolidation_1890_1910, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c983f7ba-0e64-4dc6-9f10-549a0ebb628a', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, wilford_woodruff_prophetic_office).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, plural_wives_and_families_post_1890).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency_of_eternal_principle_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_church_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_church_membership).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_interpretive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As church president, receives and announces the September 23, 1890 vision/revelation directing the church to cease sanctioning new plural marriages. Frames the reversal not as capitulation to federal pressure but as God's own changed instruction responding to changed circumstances (the confiscation of temple properties, the untenability of continued practice). By locating the reversal inside the revelatory chain rather than outside it, preserves the office's claim to speak current divine will and forecloses the reading that the church simply broke under external force.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, wilford_woodruff_prophetic_office, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, wilford_woodruff_prophetic_office, beneficiary).

% The broader quorum and administrative apparatus benefit from the Manifesto being received as continuing revelation rather than doctrinal retreat: it protects the church's temporal assets (soon returned), enables Utah statehood negotiations, and preserves the theological premise that current leadership's pronouncements carry the same authority as founding-era revelation like Section 132. The institution's long-run legitimacy depends on revelation being able to move without ever being described as reversible by external force.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_institutional_leadership, beneficiary,
    institutional, civilizational, arbitrage, national).

% Women and children in plural households formed under the prior sanctioned practice bear the direct cost of the reversal: loss of institutional legitimacy for their family form, legal precarity, social stigmatization, and in many cases economic abandonment as the church publicly distanced itself from ongoing plural unions. They did not receive a vision; they received an announcement that altered the moral and legal status of their existing lives without their participation in the revelatory process.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, plural_wives_and_families_post_1890, payer,
    powerless, biographical, trapped, local).

% Section 132 had characterized plural marriage as an eternal principle necessary for the highest degree of exaltation, commanded rather than merely permitted. The endogenous-revelation framing must explain how an eternal, salvific principle became temporally suspended by circumstance without conceding the principle was never truly eternal. This doctrinal object absorbs the strain of reconciling 'necessary for exaltation' with 'now discontinued.'
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency_of_eternal_principle_doctrine, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency_of_eternal_principle_doctrine).

% Receive continuity of institutional membership and a resolution of the crisis (imprisonment of leaders, disenfranchisement, property seizure) that had directly burdened ordinary members. Also inherit an unresolved doctrinal tension they are asked to accept on faith — the same revelatory chain that once commanded plural marriage now commands its cessation, with no public account of why God's timing required decades of family disruption in between.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_church_membership, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_church_membership, payer).

% Applied the Edmunds-Tucker Act and property confiscation that precipitated the crisis, but is written out of the endogenous-revelation account entirely — the Manifesto's own language attributes the change to divine instruction, not federal coercion. Their causal role is the central fact the exogenous_override_reading foregrounds and this reading structurally minimizes.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, federal_government_and_anti_polygamy_movement, excluded,
    institutional, biographical, mobile, national).

% Examine the timing correlation between federal pressure and the vision's announcement, the private diary language Woodruff used, and the subsequent Second Manifesto (1904) needed to enforce compliance among leaders who continued plural marriages after 1890 — evidence that cuts against a clean, complete, purely-revelatory account of consensus and enforcement.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, historians_and_doctrinal_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the institution a way to change a foundational practice without conceding that revelation is fallible or reversible by outside force — coordinating belief, compliance, and continued institutional legitimacy around a single authoritative announcement rather than requiring members to reconcile the change themselves.
% TRANSFER_FUNCTION: Moves the burden of doctrinal inconsistency and family disruption from the institution (which retains authority, property, and path to statehood) onto plural families (who absorb the practical and social costs) and onto the doctrine itself (which must silently accommodate a discontinued 'eternal' command).
% ABSENT_VOICES: Plural wives themselves were not participants in or addressees of the September 23 revelation process; their lived circumstances are referenced by the institution's later apologetics but they held no seat in the interpretive act that redefined their family's status. Federal actors whose coercion is causally central to the timing are absent from the revelatory text by design.
% DISAPPEARANCE_RATIONALE: If the endogenous-revelation framing were withdrawn and the reversal were instead openly narrated as a pragmatic capitulation to federal pressure (the exogenous_override_reading), the church's claim to continuous, authoritative, current revelation would be substantially weakened — Section 132's status, the office of the presidency's interpretive authority, and the theological architecture linking founding-era and modern pronouncements would all require public renegotiation.
% FOUNDING_PROBLEM: The church faced federal seizure of temple properties, disenfranchisement, and imprisonment of leadership under anti-polygamy statutes; institutional survival required ending the practice while a theology asserting the practice's eternal necessity remained canonically in force.
% FOUNDING_PROBLEM_CORROBORATION: The institution attests the founding problem was resolved by genuine continuing revelation, consistent with the doctrine of an open canon. Independent historians, citing correspondence and the timing of the vision relative to the Edmunds-Tucker enforcement wave and the need for a Second Manifesto in 1904 to compel compliance among leaders who continued the practice, attest that the announced resolution did not fully settle the underlying tension and that compliance required additional external and internal enforcement beyond the 1890 announcement itself.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.52 by the interval's end: moderate, not severe, because this reading credits the revelation as a genuine act of continuing revelation rather than treating it as pure institutional self-preservation — but the theological strain (an eternal, salvation-necessary principle discontinued by circumstance) and the burden borne by plural families keep extraction well above negligible. Theater ratio starts elevated (0.55) around the announcement itself, reflecting the public performative weight of the vision narrative at the moment of maximum institutional exposure, then eases somewhat as the framing settles into accepted doctrine, before ticking back up toward 1910 as the Second Manifesto enforcement period requires renewed public performance of doctrinal consistency. Suppression requirement is highest at the announcement (0.70, when compliance from plural households and continuing practitioners had to be actively secured) and eases as the practice recedes, though it never falls to a low value because enforcement against continuing plural marriages persisted for over a decade.
 *
 * PERSPECTIVAL GAP:
 *   From the prophetic office's seat, the September 23 vision is continuous revelation exercising exactly the interpretive authority the office has always claimed — no reversal of principle, only a change in current application. From the seat of a plural wife whose family status was upended by an announcement she had no part in producing, the same event operates as an externally imposed redefinition of her life circumstances dressed in revelatory language. The engine computes these as structurally different experiences of one arrangement; this reading does not adjudicate between them but authors the metrics as this reading's own lights see the standing arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Woodruff's prophetic office and the institutional leadership sit at the beneficiary end: the revelatory framing preserves their interpretive authority and secures the church's temporal survival, so their directionality is low (near-beneficiary). Plural wives and families sit at the target end: they bear the practical costs of a change made without their participation, so their directionality is high (near-target), amplified by trapped exit options and powerlessness. The doctrinal-consistency object is not an agent but absorbs analytical strain — modeled here as a non-agent payer to register the cost without treating a proposition as a rent-collecting actor.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents collapsing this into a pure snare by crediting the coordination function this reading actually claims: a mechanism for revising a foundational practice while preserving institutional continuity is a genuine problem the revelatory frame solves, not merely a pretext. But it also prevents treating the reversal as a clean, victimless doctrinal update — the tangled_rope classification requires and receives both a real beneficiary (institutional legitimacy) and a real victim (plural families, and the doctrine's own consistency), held together by the active enforcement the Second Manifesto period required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_sincerity_vs_pragmatic_timing,
    'Was the September 23 vision an independent revelatory event, or was its content and timing substantially shaped by the immediate threat of total temple-property confiscation and further prosecution?',
    'Comparative analysis of Woodruff''s private journal entries before and after the vision, correspondence among the Quorum of the Twelve in the weeks preceding the announcement, and the documented timeline of federal enforcement actions against church property in 1890.',
    'If the vision''s content and timing were substantially shaped by external threat, this endogenous-reinterpretation reading collapses toward the exogenous_override_reading, and the beneficiary/victim structure shifts: the extraction becomes primarily about institutional self-preservation rather than genuine doctrinal reinterpretation, likely raising authored extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_sincerity_vs_pragmatic_timing, empirical, 'Whether the revelatory framing reflects genuine independent revelation or coercion-shaped pragmatic timing.').

omega_variable(
    eternal_principle_discontinuity_explanation,
    'Does the church''s theological tradition offer an internally coherent account of how a doctrine characterized as necessary for the highest degree of exaltation (Section 132) could be legitimately suspended by later revelation, without implying the earlier revelation was mistaken or merely provisional in a way that undermines the doctrine of continuing revelation generally?',
    'Textual and doctrinal analysis of subsequent official church statements, general conference addresses, and correlated curriculum materials addressing the relationship between Section 132 and the Manifesto over the following century.',
    'A coherent internal account would lower the theological-consistency victim''s burden and support treating this as closer to rope; the absence of such an account, or reliance on studied ambiguity, would sustain or raise the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(eternal_principle_discontinuity_explanation, conceptual, 'Whether the endogenous-revelation reading can theologically reconcile eternal-principle language with later discontinuation.').

omega_variable(
    kernel_framing_selection_ambiguity,
    'Is the choice to treat the September 23 vision as the primary interpretive object (rather than, e.g., the pattern of enforcement culminating in the 1904 Second Manifesto, or the broader felt experience of plural families) itself a framing decision that privileges the institution''s own narrative over other equally defensible framings of the same kernel?',
    'Comparison of the three sibling readings'' respective ε values and beneficiary/victim structures against the historical record each foregrounds; assessment of which framing the primary sources (official church declarations vs. private correspondence vs. family histories) most directly support.',
    'If the vision-centered framing is judged to be institutionally selected rather than historically privileged, confidence in this reading''s beneficiary structure (institutional legitimacy as primary beneficiary) strengthens, since the framing choice itself would be evidence of whose interests it serves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_selection_ambiguity, conceptual, 'Whether centering the revelatory vision, rather than the enforcement pattern or lived family experience, is itself a framing choice serving institutional legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.55).
narrative_ontology:measurement(marr_tr_t1894, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1894, 0.5).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1898, 0.46).
narrative_ontology:measurement(marr_tr_t1902, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1902, 0.44).
narrative_ontology:measurement(marr_tr_t1906, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1906, 0.46).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.48).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.4).
narrative_ontology:measurement(marr_be_t1894, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1894, 0.45).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1898, 0.48).
narrative_ontology:measurement(marr_be_t1902, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1902, 0.5).
narrative_ontology:measurement(marr_be_t1906, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1906, 0.51).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(marr_su_t1894, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1894, 0.62).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1898, 0.6).
narrative_ontology:measurement(marr_su_t1902, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1902, 0.58).
narrative_ontology:measurement(marr_su_t1906, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1906, 0.58).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the marriage_commitment_reversal kernel. endogenous_reinterpretation_reading (this story) credits the September 23 vision as genuine continuing revelation and locates extraction in the burden on plural families and doctrinal coherence, producing moderate ε (0.52) under tangled_rope. exogenous_override_reading treats the same event as substantively coerced by federal action with Section 132 preserved as unretracted principle, which should produce a different ε and likely a different beneficiary emphasis (institutional survival under duress rather than interpretive continuity). practice_doctrine_gap treats the doctrine/practice relationship as an unresolved structural ambiguity rather than adjudicating cause, which should produce yet another distinct ε profile centered on the gap itself rather than on either revelation or coercion. All three share the same underlying historical kernel but are authored as separate constraints per the ε-invariance principle, linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
