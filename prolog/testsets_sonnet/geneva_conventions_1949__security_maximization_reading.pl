% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Security-Maximization Reading of the Geneva Conventions (Operational Necessity Suspension Doctrine)
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the security-maximization reading of the Geneva
 *   Conventions kernel: the claim that Convention protections are peacetime
 *   aspirations properly suspended when operational necessity demands it in
 *   asymmetric or irregular conflict. Under this reading, the 'unlawful
 *   combatant' category expands to strip POW status and habeas access,
 *   civilian immunity degrades through 'human shields' doctrine and expanded
 *   collateral-damage tolerance, detention becomes indefinite and judicially
 *   unreviewable, and coercive interrogation is redefined below the torture
 *   threshold. This is not the same constraint as the
 *   humanitarian_ceiling_reading (which treats the same textual kernel as
 *   establishing an absolute floor regardless of reciprocity) or the
 *   conditional_reciprocity_reading (which ties protection levels to
 *   adversary compliance) — those are separate constraint stories with their
 *   own ε values, victim sets, and classification outcomes, linked here only
 *   through the shared kernel_id and network edges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.81).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.88).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Security-Maximization Reading of the Geneva Conventions (Operational Necessity Suspension Doctrine)").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, 'ae31b887-7517-44bf-9d4c-5c735e013513').
narrative_ontology:cs_kernel_codification('ae31b887-7517-44bf-9d4c-5c735e013513', fixed_text).
narrative_ontology:cs_authority_grounding('ae31b887-7517-44bf-9d4c-5c735e013513', extraction).
narrative_ontology:cs_interpretation_layer_present('ae31b887-7517-44bf-9d4c-5c735e013513').
narrative_ontology:cs_reading_relation('ae31b887-7517-44bf-9d4c-5c735e013513', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('ae31b887-7517-44bf-9d4c-5c735e013513', geneva_conventions_1949__conditional_reciprocity_reading, influences).
narrative_ontology:cs_axiom('ae31b887-7517-44bf-9d4c-5c735e013513', foundational, operational_necessity_overrides_textual_protection).
narrative_ontology:cs_axiom_status(operational_necessity_overrides_textual_protection, holdable).
narrative_ontology:cs_axiom_grounding('ae31b887-7517-44bf-9d4c-5c735e013513', operational_necessity_overrides_textual_protection, instrumental).
narrative_ontology:cs_axiom('ae31b887-7517-44bf-9d4c-5c735e013513', foundational, irregular_combatant_status_forfeits_pow_protection).
narrative_ontology:cs_axiom_status(irregular_combatant_status_forfeits_pow_protection, holdable).
narrative_ontology:cs_axiom_grounding('ae31b887-7517-44bf-9d4c-5c735e013513', irregular_combatant_status_forfeits_pow_protection, conventional).
narrative_ontology:cs_reference_frame('ae31b887-7517-44bf-9d4c-5c735e013513', state_to_state_uniformed_combatant_framework).
narrative_ontology:cs_drift_state('ae31b887-7517-44bf-9d4c-5c735e013513', post_2001_asymmetric_conflict_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ae31b887-7517-44bf-9d4c-5c735e013513', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, executive_security_apparatus).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, detaining_state_military_command).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, interrogation_and_intelligence_services).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, unlawful_combatant_designated_detainees).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, captured_irregular_fighters).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, operational_necessity_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, asymmetric_threat_justification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines who counts as an 'unlawful combatant,' issues the legal memoranda that authorize suspension of POW protections, habeas corpus, and interrogation limits, and controls the classification apparatus that decides which detainees receive which protections. Bears no direct cost from the suspension and gains expanded discretionary power over detention and interrogation policy.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, executive_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% Operates detention facilities and battlefield targeting under the relaxed civilian-immunity and combatant-status rules this reading authorizes. Gains operational flexibility (proportionality thresholds loosened, 'human shields' doctrine shifting responsibility for collateral harm onto the adversary) and reduced legal exposure for battlefield conduct.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detaining_state_military_command, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__security_maximization_reading, detaining_state_military_command, agenda_setter).

% Conducts coercive interrogation reclassified as non-torture under this reading's operational-necessity threshold. Collects intelligence value directly from techniques that would be prohibited under the humanitarian-ceiling reading; institutionally insulated from prosecution by the same classification apparatus that authorizes the technique.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, interrogation_and_intelligence_services, beneficiary,
    institutional, biographical, arbitrage, national).

% Classified outside POW status by unilateral executive determination, with no judicial review of the classification itself. Held indefinitely without trial, subject to interrogation techniques the detaining power defines as lawful. No standing to contest status determination from inside detention; habeas corpus access is precisely what this reading suspends.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, unlawful_combatant_designated_detainees, payer,
    powerless, biographical, trapped, global).

% Bear collateral harm from strikes justified under loosened proportionality standards and from the 'human shields' doctrine, which reallocates responsibility for civilian casualties to the irregular force operating nearby rather than to the targeting decision itself. Have no legal recourse within the framework this reading establishes; their immunity is the specific protection this reading treats as negotiable under operational necessity.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, regional).

% Denied combatant privilege because they fight without the uniform/command-structure markers this reading uses to define lawful combatancy, and simultaneously denied protected-civilian status because they took up arms. Fall into a designed gap between the two Convention categories with no protection floor in either.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, captured_irregular_fighters, payer,
    powerless, biographical, trapped, national).

% ICRC and UN human rights mechanisms attempt to monitor detention and targeting conduct but are denied access to classification decisions, detention facilities, or interrogation records under national-security exemptions this reading treats as sovereign prerogative. Their objections are noted in reports but carry no binding force against the classifying state.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_humanitarian_law_monitoring_bodies, excluded,
    organized, generational, constrained, global).

% Periodically review individual habeas petitions or war-crimes referrals, producing rulings that sometimes constrain the security-maximization reading's operation (as in landmark detainee-rights cases) but do not eliminate the classificatory discretion at its core. Sits outside the extraction relationship, adjudicating specific instances rather than the framework itself.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, domestic_and_international_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__security_maximization_reading, diffuse).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__security_maximization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a state's military and intelligence apparatus around a single operational threshold — necessity — replacing the more constraining, harder-to-apply Convention categories with a discretionary standard the executive itself sets and revises, enabling faster and more unified security decision-making across services.
% TRANSFER_FUNCTION: Moves legal protection, procedural due process, and physical security away from detainees and conflict-zone civilians and toward the detaining state's security apparatus, which gains intelligence yield, operational flexibility, and reduced legal exposure in exchange.
% ABSENT_VOICES: Detainees themselves have no voice in their own status determination — the classification is made unilaterally and is precisely what forecloses their access to any forum where they could object. Conflict-zone civilians are represented, if at all, only through NGO and monitoring-body reporting that the framework treats as advisory rather than binding.
% DISAPPEARANCE_RATIONALE: If this reading's operational-necessity suspension authority vanished overnight, detaining states would be bound by the full POW and civilian-protection regime: habeas review would reattach to all detainees, coercive interrogation techniques currently authorized would become prosecutable, and targeting decisions would face the stricter proportionality calculus the humanitarian-ceiling reading applies. Entire detention architectures (indefinite holding facilities, military-commission-only review) exist only because this reading currently licenses them.
% FOUNDING_PROBLEM: Twentieth-century irregular and asymmetric conflicts (insurgency, terrorism, non-state armed groups operating outside traditional uniformed-army structures) appeared to expose a gap in a Convention framework built around state-to-state warfare between uniformed, command-structured forces — the founding claim is that rigid application of POW-centric rules to actors who do not reciprocate those norms leaves states unable to protect their populations from irregular threats.
% FOUNDING_PROBLEM_CORROBORATION: The executive security apparatus and military command attest the operational gap remains live and cite ongoing irregular-conflict casualties as evidence. Independent legal scholars, the ICRC, and several domestic and international courts (including rulings establishing habeas rights for detainees) attest that the Conventions already contain sufficient categories (protected persons, common Article 3, customary IHL minimums) to cover irregular combatants without wholesale suspension — meaning the 'gap' this reading invokes is substantially a constructed one used to license discretion rather than a genuine doctrinal void. No source entirely outside the security-apparatus and its allied legal offices affirms that suspension, rather than interpretation within existing categories, was structurally necessary.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply in the early years of the interval (0.55 to 0.76) as classification and detention infrastructure is built, then plateaus near 0.80-0.81 as the doctrine matures into settled practice rather than emergency improvisation — a rent-seeking layering pattern, not a one-time crisis response. Theater ratio climbs in parallel (0.18 to 0.42) as formal review boards, military commissions, and periodic-review processes proliferate around detention decisions that remain substantively unreviewable in outcome — procedural performance surrounding a discretionary core. Suppression is authored high and rising (0.62 to 0.88) because the doctrine's persistence depends on active denial of habeas access, monitoring-body access, and judicial review, not on voluntary participant acceptance.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this reading appears as necessary operational flexibility responsive to a genuine asymmetric threat — a coordination function unifying security policy under a single workable standard. From the payer seats (detainees, civilians, captured fighters), the identical structure operates as unreviewable extraction of physical security and legal protection with no exit and no forum. The engine computes both seats from the same structural data; the divergence between them is the specific thing a tangled_rope classification is built to register — genuine coordination function (unified security policy) coexisting with asymmetric extraction (protection stripped from a structurally powerless population) enforced by active suppression of judicial and humanitarian review.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive security apparatus, military command, and intelligence services sit at the beneficiary end: they set the classification criteria, collect the operational and intelligence benefits, and bear no structural cost from the suspension of protections. Detainees, captured irregular fighters, and conflict-zone civilians sit at the full-target end: trapped exit, no standing to contest classification, and the direct bearers of detention, interrogation, and collateral-harm costs. Monitoring bodies are excluded rather than coordinated — their exclusion from classification and facility access is a structural feature this reading requires to function, not an oversight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as contested rather than dead precisely to avoid two symmetric mislabeling errors: treating the doctrine as pure legitimate coordination (ignoring that courts and independent legal scholars attest existing Convention categories already cover irregular combatants, making the 'gap' partly constructed) and treating it as pure inertial extraction with no coordination content (ignoring that asymmetric conflict does present genuine classification difficulties the original state-to-state framework did not anticipate). The tangled_rope classification — rather than snare — reflects that a real coordination problem exists alongside the asymmetric extraction; collapsing to snare would deny the doctrine's genuine (if contested) operational rationale, while collapsing to rope would launder the suppression and victim set out of the analysis entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_gap_authenticity,
    'Is the ''gap'' in Convention coverage for irregular combatants that this reading invokes a genuine doctrinal void, or a constructed justification for discretionary suspension where existing categories (common Article 3, customary IHL) already provide coverage?',
    'Comparative doctrinal analysis of pre-suspension-era case law and ICRC commentary on common Article 3''s applicability to non-state armed groups, cross-checked against the specific protections this reading suspends.',
    'If the gap is substantially constructed, the coordination-function claim underlying the tangled_rope classification weakens significantly and the structure moves closer to a snare wearing coordination language as cover; if the gap is genuine, the coordination function is more robust and the extraction is better understood as the cost of resolving a real doctrinal problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_gap_authenticity, conceptual, 'Whether the founding doctrinal gap this reading claims to fill is real or constructed.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that all three readings (security_maximization, humanitarian_ceiling, conditional_reciprocity) derive from the same 1949 text and its 1977 Protocols, what determines which reading a given state or tribunal adopts at a given moment — legal doctrine, political power, or something else?',
    'Track which reading prevails across different institutional venues (domestic courts vs. executive legal offices vs. international tribunals) over the interval and correlate with the relative power of the parties before each venue.',
    'If institutional power (not legal reasoning) predicts which reading prevails, this reading''s persistence is better explained by the beneficiary''s control of the classifying apparatus than by doctrinal merit — reinforcing the tangled_rope reading over any rope reading of this same story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, empirical, 'What structurally determines reading-selection across venues for the shared kernel.').

omega_variable(
    coercive_interrogation_reclassification_stability,
    'Is the line this reading draws between ''coercive interrogation'' and torture a stable legal category, or does it shift opportunistically with political pressure and litigation exposure?',
    'Longitudinal tracking of which specific techniques cross from authorized to prohibited across the interval, correlated with litigation outcomes and public disclosure events.',
    'A category that shifts primarily in response to exposure risk (rather than stable ethical or medical criteria) indicates the classification function is itself extraction-serving rather than a good-faith operational necessity determination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercive_interrogation_reclassification_stability, empirical, 'Whether the torture/non-torture line is principled or exposure-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(gene_tr_t2004, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2004, 0.28).
narrative_ontology:measurement(gene_tr_t2008, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2008, 0.36).
narrative_ontology:measurement(gene_tr_t2012, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2012, 0.4).
narrative_ontology:measurement(gene_tr_t2016, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2016, 0.41).
narrative_ontology:measurement(gene_tr_t2020, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__security_maximization_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(gene_be_t2004, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2004, 0.68).
narrative_ontology:measurement(gene_be_t2008, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2008, 0.76).
narrative_ontology:measurement(gene_be_t2012, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2012, 0.79).
narrative_ontology:measurement(gene_be_t2016, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2016, 0.8).
narrative_ontology:measurement(gene_be_t2020, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2020, 0.81).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2001, 0.62).
narrative_ontology:measurement(gene_su_t2004, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2004, 0.74).
narrative_ontology:measurement(gene_su_t2008, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2008, 0.82).
narrative_ontology:measurement(gene_su_t2012, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2012, 0.85).
narrative_ontology:measurement(gene_su_t2016, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2016, 0.87).
narrative_ontology:measurement(gene_su_t2020, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2020, 0.88).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, conditional_reciprocity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the geneva_conventions_1949 kernel. security_maximization_reading claims the widest suspension scope and the highest extraction/suppression profile of the three; humanitarian_ceiling_reading claims the opposite pole (absolute minimums, minimal legitimate extraction); conditional_reciprocity_reading occupies a conditional middle position tying protection to adversary compliance. Each reading carries its own ε, beneficiary/victim set, and computed classification — they are linked via network edges, not merged into one constraint with a measurement parameter, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
