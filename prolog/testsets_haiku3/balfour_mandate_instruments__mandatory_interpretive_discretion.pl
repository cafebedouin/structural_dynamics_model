% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_interpretive_discretion, []).

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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: British Mandatory Power Interpretive Discretion over Mandate Instruments
 *   domain: international_law/colonial_administration
 *
 * SUMMARY:
 *   The British Mandate (1920-1948) over Palestine was founded on three
 *   fundamentally incompatible textual commitments: the Balfour Declaration
 *   (1917) supporting a Jewish national home, the Hussein-McMahon Letters
 *   (1915-16) implying Arab self-determination, and the Mandate Charter
 *   protecting existing Arab civil/political rights. Rather than resolve this
 *   textual contradiction through binding external arbitration, the League of
 *   Nations Covenant vested interpretive authority solely in the British
 *   mandatory power. This constraint story examines the operational
 *   consequence: British interpretive discretion became the system itself.
 *   Both Arab and Jewish communities faced continuous strategic uncertainty
 *   as British policy oscillated through the White Papers (1922, 1930, 1939)
 *   and land/immigration directives. Neither community could appeal to fixed
 *   textual meaning; both were locked into path-dependent negotiation
 *   positions shifted by each new British interpretation. The beneficiary was
 *   the British authority structure, which extracted political control and
 *   divide-and-rule leverage from the discretion itself. The victims were
 *   both communities, whose exit options collapsed to: acquiesce to
 *   oscillation, mount direct resistance (triggering enforcement), or appeal
 *   to a League Mandates Commission with no enforcement power.
 *
 * KEY AGENTS:
 *   - british_mandatory_authority: Institutional agenda-setter holding unilateral interpretive authority and enforcement machinery; power derives from discretion
 *   - arab_community: Indigenous population and victims of strategic uncertainty; identity-locked and unable to appeal oscillating interpretations
 *   - jewish_community: Beneficiary-victim: benefits from Balfour text but subject to same discretionary reinterpretation; identity-locked; exit options equally constrained
 *   - league_mandates_commission: Structurally excluded; exists theatrically but holds no enforcement power; its advisory status vindicates the discretionary authority rather than constraining it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.68).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.72).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "British Mandatory Power Interpretive Discretion over Mandate Instruments").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, '2ba1cd42-c711-45f5-bbc8-87b56b044c02').
narrative_ontology:cs_kernel_codification('2ba1cd42-c711-45f5-bbc8-87b56b044c02', fixed_text).
narrative_ontology:cs_authority_grounding('2ba1cd42-c711-45f5-bbc8-87b56b044c02', extraction).
narrative_ontology:cs_interpretation_layer_present('2ba1cd42-c711-45f5-bbc8-87b56b044c02').
narrative_ontology:cs_reading_relation('2ba1cd42-c711-45f5-bbc8-87b56b044c02', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_reading_relation('2ba1cd42-c711-45f5-bbc8-87b56b044c02', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_axiom('2ba1cd42-c711-45f5-bbc8-87b56b044c02', foundational, mandatory_power_unilateral_authority).
narrative_ontology:cs_axiom_status(mandatory_power_unilateral_authority, holdable).
narrative_ontology:cs_axiom_grounding('2ba1cd42-c711-45f5-bbc8-87b56b044c02', mandatory_power_unilateral_authority, conventional).
narrative_ontology:cs_axiom('2ba1cd42-c711-45f5-bbc8-87b56b044c02', secondary, external_review_authority_foreclosed).
narrative_ontology:cs_axiom_status(external_review_authority_foreclosed, overridden).
narrative_ontology:cs_axiom_grounding('2ba1cd42-c711-45f5-bbc8-87b56b044c02', external_review_authority_foreclosed, conventional).
narrative_ontology:cs_reference_frame('2ba1cd42-c711-45f5-bbc8-87b56b044c02', unilateral_british_interpretive_authority).
narrative_ontology:cs_drift_state('2ba1cd42-c711-45f5-bbc8-87b56b044c02', post_1939_white_paper, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2ba1cd42-c711-45f5-bbc8-87b56b044c02', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_authority).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_community).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the interpretation of Mandate instruments through White Papers, administrative directives, land-transfer policy, and immigration quotas. Enforces the chosen interpretation through military deployment, legal detention, and confiscation. Can shift policy baseline at will, forcing both Arab and Jewish communities to renegotiate their positions from new starting points. The role itself — mandatory power — is the institutional identity; Britain holds arbitrage exit (relinquish the Mandate, which it eventually does in 1948).
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_authority, agenda_setter,
    institutional, generational, arbitrage, regional).

% Indigenous population of Palestine. Interprets the Mandate instruments as protecting existing Arab civil and political rights and implying Arab self-determination (the dual_obligation reading). Faces continuous strategic uncertainty as British policy oscillates: 1922 White Paper acknowledges Palestinian Arab political aspirations; 1930 Shaw Commission restricts Jewish land purchase; 1939 White Paper affirms Arab majority rights and restricts Jewish immigration. However, each policy shift is presented as British reinterpretation, not as yielding to Arab demands, maintaining British unilateral authority. Cannot exit Palestine (identity-locked territorially and constitutionally) and cannot appeal outside British authority. Mounts continuous resistance (strikes, protests, armed rebellion 1936-39) but faces military suppression. Each policy shift resets the baseline for negotiations, locking Arab leadership into path-dependent positions.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_community, payer,
    moderate, generational, identity_locked, regional).

% Interprets the Mandate instruments as authorizing a Jewish national home and facilitating Jewish immigration and settlement (the jewish_national_home_primacy reading). Benefits from the Balfour Declaration language and from British facilitation of the Jewish Agency as institutional authority parallel to Arab municipal structures. However, equally subject to British reinterpretation: the 1922 White Paper qualifies national home as cultural/religious rather than political; 1930 Shaw Commission restricts land transfer; 1939 White Paper caps immigration and reasserts Arab majority rights. Identity-locked to the Zionist project (settlement, institutional development, immigration) and to Palestine territorially. Cannot appeal outside British authority. Mounts institutional resistance through the Jewish Agency and Zionist organizations but faces enforcement when resistance moves toward armed rebellion (post-1945). Each policy reversal (especially 1939) locks Jewish leadership into path-dependent positions requiring renegotiation with a Britain that has unilateral discretion to reinterpret again.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_community, payer,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_community, beneficiary).

% Established under League Covenant Article 22 to receive annual reports from mandatory powers and advisory statements from territories. Receives petitions from Arab and Jewish delegations; hosts oral hearings; issues advisory recommendations. Has NO authority to adjudicate disputes, NO power to order policy changes, and NO enforcement mechanism. The Mandates Commission exists as the appearance of external oversight but is structurally excluded from substantive authority. Its existence vindicates British discretion (the Commission's advisory status confirms that real authority resides with the mandatory power) rather than constraining it. By 1939-48, the Commission's advisory role becomes obviously theatrical as British policy diverges from Commission recommendations and enforcement machinery dominates.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, league_permanent_mandates_commission, excluded,
    institutional, generational, trapped, global).

% Prior administrative system under which Palestine was governed (until 1917). Ottoman legal architecture included hierarchical review (provincial governors reported to Constantinople; imperial regulations set boundaries on interpretation). British Mandatory system displaces this with unilateral discretion unconstrained by external review. The contrast highlights the discontinuity in authority structure: the constraint is not a continuation of prior rule-of-law mechanisms but a concentration of authority absent in the Ottoman precedent.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, ottoman_legacy_legal_systems, observer,
    analytical, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(balfour_mandate_instruments__mandatory_interpretive_discretion, ottoman_legacy_legal_systems).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_authority).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__mandatory_interpretive_discretion, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single authoritative mechanism to manage genuinely incompatible textual commitments (Balfour Declaration supporting Jewish national home, Hussein-McMahon Letters implying Arab self-determination, Mandate instruments protecting existing Arab civil/political rights). Prevents simultaneous implementation of all three by vesting interpretive authority in a single institutional seat (British mandatory power). The coordination function solves the problem that no exegesis could reconcile the texts.
% TRANSFER_FUNCTION: Moves interpretive authority from fixed textual meaning (which both communities could appeal to) to discretionary judgment (which only the British seat controls). Transfers political leverage and strategic position between Arab and Jewish communities as British reinterpretation shifts the policy baseline. Transfers the cost of managing incompatible promises from the text (which cannot manage them) to the communities (who must renegotiate each time policy reverses). Transfers regulatory legitimacy from the League (which the Mandates Commission represents) to the mandatory power (which exercises unilateral discretion).
% ABSENT_VOICES: The League Permanent Mandates Commission would argue for binding external review, enforcement authority over the mandatory power, and fixed textual interpretation if its role were substantive rather than advisory. It is structurally excluded by the Covenant and Mandate Agreement. Ottoman administrative hierarchy (which provided review procedures under the prior regime) is absent entirely. Both Arab and Jewish communities that would challenge unilateral discretion are present but have no appeal mechanism beyond the League Commission (which is theatrical). External powers (France, Italy, potentially the US) that might have institutional interests in the Mandate's outcome are excluded from the arbitration mechanism.
% DISAPPEARANCE_RATIONALE: If British unilateral interpretive discretion disappeared — replaced by binding external arbitration, co-equal Arab-Jewish-British interpretive authority, or fixed textual exegesis enforced by the League — the entire strategic landscape would reorganize. The 1922, 1930, and 1939 White Papers would no longer be possible as unilateral reversals; instead, policy would be determined by negotiated settlement or external adjudication. The path-dependent lock-in created by successive reinterpretation would break. The British seat would lose the political leverage that discretion provides. Both Arab and Jewish communities would face a different strategic environment: one with fixed rules rather than oscillating discretion. The Mandate might have ended differently (or not at all) without the strategic uncertainty that discretion created.
% FOUNDING_PROBLEM: Three incompatible textual commitments made to different parties at different times — the Balfour Declaration (1917) to Zionists supporting a Jewish national home, the Hussein-McMahon Letters (1915-16) to Arab leadership implying self-determination, and the Mandate instruments (1920) protecting existing Arab civil and political rights — could not be exegetically reconciled. Britain needed a mechanism to implement one while managing the others, or to oscillate between them. Unilateral interpretive discretion vested in the mandatory power was presented as the solution: Britain would judge which reading applied in each circumstance.
% FOUNDING_PROBLEM_CORROBORATION: British colonial officials attested that the textual incompatibility required discretionary management — Herbert Samuel (first High Commissioner) and subsequent officials defended discretion as the only workable solution. Arab and Jewish leadership from 1920 onward disputed whether the problem was genuinely insoluble (Arabs: the texts clearly support Arab self-determination and Arab majority rights; Jews: the Balfour Declaration and national home language are primary; British: the texts are insoluble and Britain must judge). External scholars and international law analysts (not beholden to any of the parties) confirmed the textual incompatibility; they divided on whether unilateral British discretion was the appropriate mechanism (some argued it was necessary; others argued that binding external arbitration or text-bound interpretation would have been preferable). The 1939 White Paper's oscillation back toward Arab majority rights suggested even the British seat came to doubt whether discretion could achieve reconciliation.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 (interval end) because the constraint's primary output is political authority and negotiating leverage concentrated in the British seat, extracted from both victim communities' ability to rely on fixed textual meaning. Suppression is higher (0.72) because both communities face active enforcement (military deployment, administrative detention, land confiscation) when they resist oscillating policy interpretations. Theater rises from 0.25 (1920, when discretion was new and policy still settling) to 0.43 (1939, when the League Mandates Commission's advisory role became obvious theater, formal procedural hearings yielded no binding decisions, and enforcement machinery dominated the actual operation). The slight decline by 1948 reflects the constraint's terminal breakdown — the Mandate ended and competing readings could no longer be held in unilateral British interpretive suspension. The measurement grid is aligned: every metric is authored at every examined time point (1920, 1925, 1930, 1935, 1939, 1948), capturing the trajectory of the constraint's operation from establishment through collapse. The rising extractiveness and suppression reflect the British authority's increasing reliance on enforcement as the incompatible textual readings became unmistakable and both communities' resistance mounted.
 *
 * PERSPECTIVAL GAP:
 *   The British mandatory authority and the Arab/Jewish communities compute different constraint types from the same structural data. From the British institutional seat, the arrangement appears as a rope: necessary coordination solving an insoluble textual problem, protecting communities from the consequences of incompatible promises, and maintaining administrative order. From the Arab seat, the constraint appears as a snare: oscillating policy leaves no stable exit, identity-lock prevents relocation, and direct resistance triggers enforcement targeted at Arab political mobilization. From the Jewish seat, the constraint presents a partially different snare: beneficiary of Balfour language, but equally subject to British reinterpretation that can (and did in 1939) reverse gains. Both victim communities compute the same core structure (unilateral discretion, path-dependent lock-in, enforcement machinery), but diverge on whether the constraint is originally extractive or became extractive as policy shifted against their interests. The engine computes these divergent classifications from power asymmetry (institutional vs. moderate), exit options (arbitrage vs. identity-locked), and victim/beneficiary declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   British mandatory authority: d ≈ 0.0-0.1 (full beneficiary of discretion, arbitrage-level exit, institutional power). Arab community: d ≈ 0.85-0.90 (full target, identity-locked, moderate power constrained by colonial enforcement, victim declaration). Jewish community: d ≈ 0.60-0.70 (asymmetric: benefits from Balfour text but equally targeted by oscillating reinterpretation, identity-locked, moderate power, victim declaration). The directionality asymmetry between the beneficiary seat and the victim seats drives the engine's effective extraction computation: χ is amplified for trapped/identity-locked victims and damped for the institutional beneficiary with arbitrage options.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (reconciling incompatible textual commitments) remains live throughout the 1920-1948 interval, but the founding solution (unilateral British discretion as the least-bad arbiter) becomes contested by 1930 and is repudiated by 1939. The 1939 White Paper represents the founding problem's formal acknowledgment as unresolved: Britain reasserts Arab majority rights, contradicting the Balfour commitment's implementation trajectory, and confesses that the discretionary solution has not reconciled the original contradiction — it has merely deferred it through oscillation. The theater ratio's rise (0.25→0.43) captures this: as the discretion's legitimacy crumbles, enforcement becomes more theatrical (the Mandates Commission's advisory sessions multiply while actual power flows through military and administrative channels). By 1948, the constraint ends not through resolution of the underlying textual contradiction but through the collapse of the British Mandate itself — the contradiction is inherited by successor states and international forums. The classification remains snare throughout because the oscillating discretion continues to extract political authority and strategic position from both communities until the system itself dissolves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_incompatibility_genuineness,
    'Are the Balfour Declaration and the Hussein-McMahon Letters genuinely textually incompatible, or is the incompatibility a reading choice that could be reconciled by alternative exegesis?',
    'Comparative textual analysis by scholars outside the British/Arab/Jewish institutional commitments; historical evidence of negotiators'' intent; League legal analysis at the time.',
    'If incompatibility is genuine, the constraint is a necessary mechanism for managing a real problem. If it is a reading choice, the constraint is an unnecessary concentration of discretion that could have been replaced by binding external arbitration or co-equal interpretive authority. The classification depends on whether discretion is solving or creating the problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_incompatibility_genuineness, conceptual, 'Whether the founding problem (textual incompatibility) is structural or interpretive.').

omega_variable(
    discretion_as_solution_vs_discretion_as_extraction,
    'Is unilateral British interpretive discretion the least-bad solution to genuinely incompatible textual commitments, or is it a mechanism to extract political control from both communities under the guise of arbitration?',
    'Comparative institutional analysis: did other mandatory powers use similar discretionary mechanisms for their contradictory commitments? Were there alternative dispute-resolution structures available (e.g., binding League arbitration, co-equal Arab-Jewish-British commission) that were explicitly rejected by Britain? Counterfactual evidence from post-1948 resolution models.',
    'If discretion is genuinely the least-bad solution, the constraint is tangled_rope (real coordination cost, real extraction as the cost of coordination). If it is extraction dressed as solution, the constraint is snare (extraction justified by a cover story). The 1939 White Paper and 1948 Mandate collapse suggest the discretion failed as a solution, suggesting the snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_as_solution_vs_discretion_as_extraction, empirical, 'Whether the constraint solves the founding problem or merely postpones resolution while extracting authority.').

omega_variable(
    mandatory_power_legitimacy_grounding,
    'On what grounds does the League of Nations Covenant authorize the mandatory power''s unilateral interpretive discretion? Is it derived from consent of the territories (no evidence for Palestine), from expertise (Britain claimed expertise in administering diverse populations), from lineage (no prior British authority over Palestine), or from pure institutional assertion?',
    'Historical analysis of League legal doctrine; statements by British officials and League Assembly members at the time of Mandate approval; comparison to other mandates and how Britain justified discretion in those cases.',
    'If legitimacy is derived from consent or expertise, the constraint operates with some consensual foundation. If it is pure institutional assertion, the constraint is more transparently extractive. The legitimacy grounding also affects the classification: derived authority supports tangled_rope; pure assertion supports snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatory_power_legitimacy_grounding, conceptual, 'The authority structure grounding for unilateral mandatory power discretion.').

omega_variable(
    oscillation_as_deliberate_strategy_vs_policy_drift,
    'Did British policy oscillate between the 1922, 1930, and 1939 White Papers because of genuine changes in circumstance and British judgment, or because oscillation itself was a deliberate divide-and-rule strategy to prevent either community from organizing effective resistance?',
    'Colonial administrative records; British Cabinet papers (declassified in the UK); testimony from British officials involved in policy formation; comparative analysis of policy coherence with other British colonial administrations.',
    'If oscillation is circumstantial drift, the extraction is incidental to the discretion mechanism. If oscillation is deliberate strategy, the constraint is transparently snare: policy reversals are the primary tool for maintaining control. The rising theater ratio and sustained suppression suggest deliberate oscillation rather than drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oscillation_as_deliberate_strategy_vs_policy_drift, empirical, 'Whether policy oscillation is a side effect of discretion or a deliberate strategy for divide-and-rule.').

omega_variable(
    league_mandates_commission_authority_foreclosure,
    'Did the League Covenant''s Article 22 and the Mandate Agreement genuinely foreclose binding external review (making the Mandates Commission advisory-only), or was Britain''s rejection of binding review a political choice rather than a legal necessity?',
    'Textual analysis of Article 22 and the Mandate Agreement; League legal opinion at the time; evidence of whether Britain advocated for or against binding external review in League discussions; comparison to alternative Mandate architectures that were considered but rejected.',
    'If binding review was genuinely foreclosed by the Covenant text, the constraint is structurally necessary. If it was Britain''s political choice, the discretion is not a least-bad solution but a chosen concentration of authority. The classification would shift toward clearer snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(league_mandates_commission_authority_foreclosure, conceptual, 'Whether unilateral discretion is a legal necessity or a political choice made at the League founding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1920, 0.25).
narrative_ontology:measurement(balf_tr_t1925, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1925, 0.3).
narrative_ontology:measurement(balf_tr_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1930, 0.36).
narrative_ontology:measurement(balf_tr_t1935, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1935, 0.4).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1939, 0.43).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1948, 0.41).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1920, 0.52).
narrative_ontology:measurement(balf_be_t1925, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1925, 0.58).
narrative_ontology:measurement(balf_be_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1930, 0.64).
narrative_ontology:measurement(balf_be_t1935, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1935, 0.67).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1939, 0.71).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1948, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement(balf_su_t1925, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1925, 0.64).
narrative_ontology:measurement(balf_su_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1930, 0.68).
narrative_ontology:measurement(balf_su_t1935, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1935, 0.71).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1939, 0.76).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1948, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.22).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__jewish_national_home_primacy).

% DUAL FORMULATION NOTE:
% The balfour_mandate_instruments kernel decomposes into three distinct constraint stories, each instantiating a different reading of the same textual commitments. mandatory_interpretive_discretion is the institutional architecture reading: it examines the system by which interpretive authority is vested and deployed. dual_obligation_indigenous_rights and jewish_national_home_primacy are substantive-interpretation readings: they examine what the Mandate instruments are read to require. All three are live simultaneously 1920-1948 as competing institutional and legal frames. This constraint's epsilon (0.68, snare) reflects the extraction inherent in unilateral discretion; the sibling constraints will show different epsilon values reflecting their respective interpretive claims (dual_obligation likely lower epsilon reflecting coordination/protection logic; jewish_national_home_primacy likely moderate-to-high epsilon reflecting transformation/settlement extraction). The three stories are linked: the discretion mechanism (this story) enables either sibling reading to be implemented, reversed, or oscillated without external constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
