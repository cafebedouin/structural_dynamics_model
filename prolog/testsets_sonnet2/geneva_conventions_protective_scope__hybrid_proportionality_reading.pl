% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Conflict-Classification-Dependent Geneva Protective Scope (Hybrid Proportionality Reading)
 *   domain: international_humanitarian_law/legal_theory/armed_conflict
 *
 * SUMMARY:
 *   International humanitarian law does not apply a single protective
 *   standard to all armed conflict; instead it graduates protection by
 *   conflict classification, with Additional Protocol I governing
 *   international armed conflicts and Common Article 3 plus Additional
 *   Protocol II governing non-international ones, and proportionality
 *   analysis determining whether specific military actions are lawful within
 *   whichever framework applies. This structure was built to make the
 *   Conventions ratifiable across a diversity of state interests while
 *   extending some protection to internal conflicts previously outside treaty
 *   scope entirely. In practice, however, the classification threshold has
 *   become the primary contested terrain: which framework applies is
 *   frequently determined unilaterally by the more powerful party to a
 *   conflict, with limited real-time contestability, meaning legal ambiguity
 *   around classification functions as interpretive latitude for that party
 *   while leaving non-state fighters, civilians in contested zones, and
 *   ambiguously-classified detainees without a stable, predictable protective
 *   floor.
 *
 * KEY AGENTS:
 *   - classifying_state_militaries: agenda_setter/beneficiary (institutional/arbitrage) — determine conflict classification and proportionality application
 *   - legal_advisory_corps: beneficiary (organized/mobile) — professional apparatus built around interpreting the classification/proportionality framework
 *   - non_state_armed_group_fighters: payer (powerless/trapped) — protected status entirely dependent on classification they cannot contest
 *   - civilians_in_contested_classification_zones: payer (powerless/trapped) — bear unpredictable proportionality outcomes
 *   - detainees_of_ambiguous_status: payer (powerless/trapped) — legal status determined unilaterally by detaining power
 *   - icrc_and_monitoring_bodies: excluded/observer (organized/constrained) — document but cannot bind classification determinations
 *   - international_criminal_tribunals: observer (institutional/analytical) — clarify doctrine after the fact, without real-time protective effect
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.58).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.51).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Conflict-Classification-Dependent Geneva Protective Scope (Hybrid Proportionality Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "international_humanitarian_law/legal_theory/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'cac2173f-ffc2-4d9c-ae22-ca9c1543b3cd').
narrative_ontology:cs_kernel_codification('cac2173f-ffc2-4d9c-ae22-ca9c1543b3cd', fixed_text).
narrative_ontology:cs_authority_grounding('cac2173f-ffc2-4d9c-ae22-ca9c1543b3cd', distributed).
narrative_ontology:cs_reading_relation('cac2173f-ffc2-4d9c-ae22-ca9c1543b3cd', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('cac2173f-ffc2-4d9c-ae22-ca9c1543b3cd', geneva_conventions_protective_scope__universal_rights_reading, influences).
narrative_ontology:cs_axiom('cac2173f-ffc2-4d9c-ae22-ca9c1543b3cd', foundational, protection_scales_with_conflict_character).
narrative_ontology:cs_axiom_status(protection_scales_with_conflict_character, holdable).
narrative_ontology:cs_axiom_grounding('cac2173f-ffc2-4d9c-ae22-ca9c1543b3cd', protection_scales_with_conflict_character, conventional).
narrative_ontology:cs_axiom('cac2173f-ffc2-4d9c-ae22-ca9c1543b3cd', secondary, proportionality_analysis_resolves_boundary_cases).
narrative_ontology:cs_axiom_status(proportionality_analysis_resolves_boundary_cases, holdable).
narrative_ontology:cs_axiom_grounding('cac2173f-ffc2-4d9c-ae22-ca9c1543b3cd', proportionality_analysis_resolves_boundary_cases, instrumental).
narrative_ontology:cs_reference_frame('cac2173f-ffc2-4d9c-ae22-ca9c1543b3cd', id_1977_additional_protocols_graduated_framework).
narrative_ontology:cs_drift_state('cac2173f-ffc2-4d9c-ae22-ca9c1543b3cd', post_global_war_on_terror_classification_disputes, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cac2173f-ffc2-4d9c-ae22-ca9c1543b3cd', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, classifying_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, legal_advisory_corps).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_group_fighters).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_contested_classification_zones).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, detainees_of_ambiguous_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determine, through their own targeting and legal advisory processes, whether a given conflict is classified as international armed conflict (triggering full AP I protections) or non-international (triggering the thinner Common Article 3/AP II floor), and apply proportionality analysis to specific strikes and detention decisions. The classification determination is made internally, under operational pressure, with limited external contestability in real time. Where classification is ambiguous, the state benefits from the interpretive latitude this ambiguity affords.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, classifying_state_militaries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, classifying_state_militaries, beneficiary).

% Military and government legal advisors build careers and institutional authority around interpreting the classification and proportionality framework. Their expertise is valuable precisely because the standards are contestable; they operate within, and benefit from, the interpretive space the hybrid structure creates.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, legal_advisory_corps, beneficiary,
    organized, biographical, mobile, national).

% Their protected status depends entirely on how the conflict is classified by the more powerful party, a determination they cannot contest in real time or through any accessible tribunal. If the conflict is deemed non-international, they receive only the Common Article 3 floor rather than full combatant/POW protections, and are often prosecuted domestically for the mere act of fighting. They bear the classification decision without a voice in making it.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_group_fighters, payer,
    powerless, immediate, trapped, regional).

% Live in areas where the conflict's classification is disputed or shifts (e.g., transnational operations against non-state actors, occupation-adjacent hostilities). Whether a strike is judged lawful under proportionality analysis depends on which framework applies and how proportionality is calculated by the party conducting the strike, leaving civilians unable to predict or rely on a stable protective standard.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_contested_classification_zones, payer,
    powerless, immediate, trapped, regional).

% Held in detention where their classification as POW, protected civilian, or unprivileged detainee determines interrogation limits, release timelines, and legal process access. The classifying power's own legal apparatus makes this determination, often without independent tribunal review, and the detainee has no mechanism to compel reclassification.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, detainees_of_ambiguous_status, payer,
    powerless, biographical, trapped, regional).

% The International Committee of the Red Cross and similar monitoring bodies advocate for protective interpretations and document violations, but have no binding authority to override a state's classification determination or proportionality calculus. They can name the ambiguity publicly but cannot resolve it; access to detainees and conflict zones is itself contingent on the classifying party's cooperation.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, icrc_and_monitoring_bodies, excluded,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, icrc_and_monitoring_bodies, observer).

% Adjudicate individual criminal responsibility after the fact, sometimes issuing rulings that clarify classification standards (e.g., the Tadić 'overall control' test). Their rulings shape doctrine going forward but almost never provide real-time protection to those caught in a live classification dispute, and enforcement of their judgments depends on state cooperation.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_criminal_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__hybrid_proportionality_reading, classifying_state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__hybrid_proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a graduated legal framework so that combatants, civilians, and detaining powers know which body of humanitarian law rules apply given the character of the conflict, avoiding a single rigid standard that would either over-extend full POW protections to every skirmish or under-protect civilians in major interstate war.
% TRANSFER_FUNCTION: Moves interpretive discretion — and with it, the practical burden of proof about which protections apply — from the party bearing the risk of misclassification (fighters, civilians, detainees) to the party making the classification determination (the state conducting operations), which is typically the stronger military actor.
% ABSENT_VOICES: Non-state armed group fighters and civilians in contested zones have no seat in the classification process and no real-time forum to contest a state's determination; the ICRC and human rights monitors raise these voices but hold no binding authority, so the absence persists structurally even where documented.
% DISAPPEARANCE_RATIONALE: If the conflict-type-dependent structure disappeared and a single uniform protective standard applied regardless of classification, entire categories of current legal argument (contesting classification to avoid AP I obligations, or to deny POW status) would vanish, targeting and detention practices would need to be reformed to a single floor, and the extensive legal-advisory apparatus built around classification disputes would lose its primary function.
% FOUNDING_PROBLEM: The 1949 Conventions and 1977 Protocols were built to extend meaningful protection across a widening range of conflict types — including colonial wars of national liberation and internal armed conflicts — without simply imposing full interstate-war rules on every internal disturbance, which states would have refused to ratify.
% FOUNDING_PROBLEM_CORROBORATION: States and their legal advisors attest the graduated structure remains necessary to preserve ratification and operational workability. The ICRC, international criminal tribunals, and independent IHL scholars (outside any state's benefiting interest) attest that in practice the classification threshold has become the primary site of protection-avoidance litigation, with the Tadić and subsequent jurisprudence explicitly developed to close gaps states were exploiting — corroboration from outside the benefiting parties that the founding coordination problem has been partially supplanted by a classification-avoidance function.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the classification/proportionality structure genuinely coordinates a graduated protective regime but has become, in practice, a lever the stronger party in a conflict uses to select the applicable framework and proportionality baseline to its own advantage — the coordination function and the extraction function ride the same legal mechanism. Suppression (0.51) is moderate: there is no single coercive gatekeeper, but the absence of a real-time, binding, neutral classification tribunal functions as structural suppression of contestation by weaker parties. Theater ratio (0.42) captures a meaningful and rising share of legal-advisory activity oriented toward defensible classification arguments rather than toward actually determining and applying the correct protective standard — a documented trend as targeting operations and detention practices have scaled since the interval's start. Accessibility collapse (0.47) is moderate-low: alternative universal-floor or state-centric framings remain live in legal discourse (this is precisely why they are separate sibling readings), so the hybrid reading has not fully foreclosed contestation, but working alternatives for an affected individual in a live conflict are functionally inaccessible in real time. Resistance (0.63) is substantial, driven by ICRC advocacy, tribunal jurisprudence (Tadić and successors), and academic IHL critique actively contesting unilateral classification.
 *
 * PERSPECTIVAL GAP:
 *   From the classifying state's seat, proportionality analysis is a good-faith legal discipline applied under operational uncertainty. From the seat of a non-state fighter or civilian in a contested zone, the identical structure operates as an unreviewable unilateral determination of their protective status. The engine computes these as structurally different seat experiences from the same authored data — the divergence is the object of analysis, not an inconsistency to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Classifying state militaries sit near the full-beneficiary end: they make the classification call, apply proportionality analysis to their own operations, and retain arbitrage-grade exit (they can select the more favorable framework where classification is genuinely contestable). Legal advisory corps benefit structurally from the interpretive complexity itself — their institutional value rises with ambiguity. Non-state fighters, civilians in contested zones, and ambiguously-classified detainees are pushed toward the full-target end: trapped exit options, immediate-to-biographical time horizons, and no capacity to contest the classification determination that fixes their protective status. The ICRC and tribunals occupy an observer/excluded hybrid — analytically positioned to see the full structure but structurally excluded from binding it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — extending meaningful protection to conflicts the rigid interstate-only Geneva framework would have left uncovered — remains partially live (internal and hybrid conflicts are real and growing in relative frequency), which prevents a clean mandatrophy verdict. But the corroborated tribunal record (Tadić et al.) shows the classification threshold has also become a site actively exploited for protection-avoidance, a function the founding coordination problem never intended. Classifying the arrangement as tangled_rope rather than snare or mountain reflects this: a genuine coordination function (graduated protection matched to conflict character) persists alongside a substantial, actively enforced extraction dynamic (unilateral classification as interpretive leverage) — collapsing it to pure extraction would erase the real protective work the graduated structure still does in genuinely ambiguous, good-faith classification cases; treating it as a settled coordination mechanism would erase the corroborated exploitation pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_good_faith_vs_strategic,
    'In contested classification cases, is the classifying state''s determination a genuine good-faith application of Article 4/common Article 3 criteria, or a strategic selection of whichever framework minimizes its own obligations?',
    'Comparative case analysis of classification determinations against independent tribunal reclassifications after the fact (e.g., ICTY overall-control jurisprudence); a pattern of post-hoc tribunal correction concentrated against one party''s initial determinations would support the strategic-selection reading.',
    'If predominantly strategic, effective extraction is higher than the base metric suggests and the hybrid reading''s coordination claim weakens toward snare; if predominantly good-faith under genuine uncertainty, the tangled_rope classification with a real coordination component is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_good_faith_vs_strategic, empirical, 'Whether classification determinations reflect genuine legal uncertainty or strategic obligation-avoidance.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the hybrid proportionality reading itself the most accurate description of how IHL actually operates, or does it understate the degree to which practice has converged toward either the state-centric reading (in practice, if not in doctrine) or the universal-rights reading (in aspiration, via IHRL overlay)?',
    'Systematic review of state practice and opinio juris across multiple contemporary non-international and hybrid conflicts to determine whether the graduated hybrid framework, the state-centric floor, or the IHRL-supplemented universal floor best predicts actual protective outcomes.',
    'If practice has converged toward the state-centric reading, this story''s ε understates extraction (the hybrid framing would itself be providing legitimating cover for an effectively state-centric outcome); if toward the universal-rights reading, ε overstates it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the hybrid reading is the best description of practice, or itself functions as a mediating fiction between the two more extreme siblings.').

omega_variable(
    proportionality_calculus_transparency,
    'How much of the proportionality analysis underlying specific targeting and detention decisions is ever subject to independent, non-self-interested review?',
    'Track the rate at which proportionality determinations in contested strikes are referred to, and overturned or upheld by, independent investigative or judicial bodies versus resolved solely through internal military legal review.',
    'A low independent-review rate would support treating the proportionality mechanism as substantially self-certifying, raising the effective suppression score; a high rate would support the coordination framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_calculus_transparency, empirical, 'The degree to which proportionality determinations are independently reviewable versus self-certified.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gene_tr_t8, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(gene_tr_t16, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(gene_tr_t24, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(gene_tr_t32, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(gene_tr_t40, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gene_be_t8, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(gene_be_t16, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(gene_be_t24, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(gene_be_t32, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(gene_be_t40, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gene_su_t8, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 8, 0.43).
narrative_ontology:measurement(gene_su_t16, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(gene_su_t24, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(gene_su_t32, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(gene_su_t40, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 40, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__universal_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the geneva_conventions_protective_scope kernel. The state_centric_reading treats Article 4 combatant-status criteria as a hard gate excluding unprivileged belligerents from treaty protection entirely (lower coordination claim, higher declared extraction toward excluded fighters). The universal_rights_reading treats Common Article 3 plus IHRL as establishing a universal floor regardless of classification (higher declared coordination, lower declared extraction, since classification disputes are structurally dissolved rather than adjudicated). This hybrid_proportionality_reading occupies the middle: it authors moderate ε reflecting a real graduated-protection coordination function alongside a substantial, corroborated extraction dynamic where classification ambiguity is exploited by the stronger party. The three stories share no beneficiary/victim arrays and are not averaged; each has its own stable ε per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
