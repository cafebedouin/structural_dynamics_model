% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Geneva Conventions 1949 — Security Maximization Reading (Necessity-Suspended Protections)
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   The security-maximization reading of the 1949 Geneva Conventions,
 *   institutionalized most prominently in the post-2001 counterterrorism
 *   framework, converts a mutual-restraint treaty regime into a one-way
 *   license: protections become suspendable at the state's discretion, a
 *   newly expanded unlawful-combatant category removes detainees from the
 *   protection system along with their access to courts, civilian immunity
 *   narrows through human-shields framing and widened collateral-damage
 *   acceptance, and interrogation limits are redrawn by definitional fiat.
 *   The epsilon referent is the standing arrangement under contest — the
 *   conventions regime as it actually operates under this reading — assessed
 *   from this reading's own seat: the reading does not dispute that liberties
 *   and immunities are taken from the people in its custody and its target
 *   areas; it disputes that taking them is wrongful. Epsilon measures the
 *   taking, not its justification. Claim/metric independence is preserved:
 *   claimed_type snare is my structural judgment about what this arrangement
 *   is; the metrics describe how it operates.
 *
 * KEY AGENTS:
 *   - state_security_apparatus: agenda-setting beneficiary (institutional/arbitrage) — authors the reading, collects its gains, controls its interpretations
 *   - field_command_operators: dual-positioned operator (powerful/constrained) — gains operational latitude, bears reciprocity and legal-exposure costs
 *   - unlawful_combatant_detainees: primary target (powerless/trapped) — liberty and legal personhood taken indefinitely
 *   - civilian_populations_conflict_zones: primary target (powerless/trapped) — immunity degraded where the widened rules operate
 *   - coercive_interrogation_subjects: primary target (powerless/trapped) — bodily integrity taken under redefined limits
 *   - icrc_humanitarian_monitors: excluded monitor (organized/constrained) — formal mandate, managed access, no enforcement hook
 *   - domestic_habeas_courts: observer/excluded adjudicator (institutional/constrained) — jurisdiction worked around, partially reasserted
 *   - human_rights_treaty_bodies: excluded voice (organized/mobile) — documentation without enforcement
 *   - ihl_scholarly_community: analytical observer (analytical/analytical) — sees the full structure from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.83).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.75).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.83).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, snare).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Geneva Conventions 1949 — Security Maximization Reading (Necessity-Suspended Protections)").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, 'd5fb929c-67c2-4859-89a8-1e0c285fcec2').
narrative_ontology:cs_kernel_codification('d5fb929c-67c2-4859-89a8-1e0c285fcec2', fixed_text).
narrative_ontology:cs_authority_grounding('d5fb929c-67c2-4859-89a8-1e0c285fcec2', extraction).
narrative_ontology:cs_interpretation_layer_present('d5fb929c-67c2-4859-89a8-1e0c285fcec2').
narrative_ontology:cs_reading_relation('d5fb929c-67c2-4859-89a8-1e0c285fcec2', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('d5fb929c-67c2-4859-89a8-1e0c285fcec2', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('d5fb929c-67c2-4859-89a8-1e0c285fcec2', foundational, operational_necessity_trumps_treaty_protections).
narrative_ontology:cs_axiom_status(operational_necessity_trumps_treaty_protections, holdable).
narrative_ontology:cs_axiom_grounding('d5fb929c-67c2-4859-89a8-1e0c285fcec2', operational_necessity_trumps_treaty_protections, instrumental).
narrative_ontology:cs_axiom('d5fb929c-67c2-4859-89a8-1e0c285fcec2', secondary, geneva_protections_presuppose_interstate_warfare).
narrative_ontology:cs_axiom_status(geneva_protections_presuppose_interstate_warfare, holdable).
narrative_ontology:cs_axiom_grounding('d5fb929c-67c2-4859-89a8-1e0c285fcec2', geneva_protections_presuppose_interstate_warfare, conventional).
narrative_ontology:cs_reference_frame('d5fb929c-67c2-4859-89a8-1e0c285fcec2', conventions_as_peacetime_aspiration).
narrative_ontology:cs_drift_state('d5fb929c-67c2-4859-89a8-1e0c285fcec2', post_september_11_architecture, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d5fb929c-67c2-4859-89a8-1e0c285fcec2', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, state_security_apparatus).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, field_command_operators).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, unlawful_combatant_detainees).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilian_populations_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, coercive_interrogation_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, field_command_operators).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, operational_necessity_supremacy).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, unlawful_combatant_classification).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, collateral_damage_proportionality_deference).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the detention, interrogation, and targeting policies that give the reading operational form: defines who counts as an unlawful combatant, operates detention facilities outside ordinary court review, and runs interrogation programs under legally redefined limits. Collects the operational gains — intelligence product, detention authority, targeting latitude — and controls the legal interpretations that keep those gains available. Can revise or abandon the reading at will; nothing external compels its continuation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, state_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% Carry out detentions, raids, and interrogations under the widened rules. Gain speed and latitude that full convention processing would slow, but bear the reciprocal side of the bargain: adversaries invoke the same looseness against captured personnel, and personal legal exposure resurfaces whenever courts or successor administrations revisit the authorizing memos. Bound to the framework by career and command structure; individual exit is not a real option.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, field_command_operators, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__security_maximization_reading, field_command_operators, payer).

% Held for years without prisoner-of-war status, without charges, and — for long stretches — without access to any court that could test the legality of their detention. Classification as unlawful combatants places them outside the detention and trial protections the conventions extend to lawful fighters. Exit means release, transfer, or a court order; none is within their own power.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, unlawful_combatant_detainees, payer,
    powerless, biographical, trapped, regional).

% Live where the widened targeting rules operate. Civilian immunity narrows in practice: presence near fighters is recast as shielding, and acceptable collateral damage widens as proportionality judgments move into classified targeting processes. They cannot leave the areas where the rules are applied and hold no seat in the processes that draw the lines.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilian_populations_conflict_zones, payer,
    powerless, biographical, trapped, regional).

% Undergo interrogation under techniques authorized by redefining what counts as torture — stress positions, prolonged isolation, simulated drowning, temperature extremes — each certified in advance as falling short of the legal line. Their bodies are the site where the redefinition is tested. No procedural avenue exists to contest the authorization before it is applied.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, coercive_interrogation_subjects, payer,
    powerless, immediate, trapped, local).

% Hold a formal mandate under the conventions to visit and register detainees and to monitor compliance. Under this reading, access is deferred, conditioned, or confined to facilities the detaining power selects, and confidential findings carry no enforcement hook. They continue to document and protest from outside the decisions that set the rules.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, icrc_humanitarian_monitors, excluded,
    organized, generational, constrained, global).

% Are the institutions whose jurisdiction the reading works to exclude: legislation strips habeas for designated detainees, and classified-evidence doctrines limit what review survives. Parts of the judiciary have pushed back — restoring some review and striking detention schemes — but the docket they receive is bounded by what the political branches concede. They adjudicate the framework's edges while being structurally kept off its core.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, domestic_habeas_courts, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__security_maximization_reading, domestic_habeas_courts, excluded).

% United Nations bodies, special rapporteurs, and non-governmental organizations document departures from convention minimums and publish findings. They hold no enforcement power over the states adopting the reading, are denied access to facilities, and are answered with sovereignty objections. Their reports shape allied opinion and future accountability but not current operating rules.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, human_rights_treaty_bodies, excluded,
    organized, generational, mobile, global).

% Tracks the reading's doctrinal moves — the combatant-status categories, the necessity claims, the interrogation-limit redefinitions — and maps them against the treaty text and customary law. Their position is analytical distance: they can name the whole structure, publish it, and teach it, with influence that runs on timescales longer than the operations they describe.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, ihl_scholarly_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__security_maximization_reading, state_security_apparatus).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__security_maximization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Internally, the reading gives the security bureaucracy a shared legal-operational frame that lets agencies act in concert across theaters without case-by-case convention adjudication. Externally, it preserves a thin nominal-adherence surface — formal continued party status, selective monitoring access, periodic compliance reporting — sufficient to keep allied cooperation and treaty membership intact.
% TRANSFER_FUNCTION: Moves liberty (indefinite detention without charge or habeas), bodily integrity (coercive interrogation under redefined limits), and legal immunity from prosecution from detainees, interrogation subjects, and civilians in conflict zones to the state as intelligence product, detention authority, and targeting latitude.
% ABSENT_VOICES: The detainees and civilians whose status the reading decides are never in the room; the ICRC holds a formal visiting mandate but is managed toward selected facilities; treaty bodies and special rapporteurs receive answers, not seats. The reading was authored in classified executive legal memoranda where the only voices belonged to the departments that benefit from it.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would reopen habeas for the detained population, halt interrogation programs operating under the redefined limits, restore convention processing for future captures, and force targeting procedures back inside narrower proportionality review. The detention and interrogation architecture would have to rebuild itself around restored constraints, and allied arrangements premised on the current frame would require renegotiation.
% FOUNDING_PROBLEM: A treaty regime drafted around wars between uniformed state armies confronted adversaries organized as transnational irregular networks that wear no uniforms, hold no territory, and claim no convention obligations — leaving capturing states with categories (prisoner of war, civilian) that appeared to fit neither the fighters nor the threat.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: ICRC commentaries and successive United Nations reporting acknowledge the fit problem between the 1949 categories and irregular conflict; military historians and counterinsurgency practitioners unaffiliated with the security departments attest the operational dilemma is real. What those same sources dispute is the remedy — they attest the founding problem is live while documenting that the suspension architecture consistently exceeds any demonstrated necessity.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.83, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.83 at interval end) because the arrangement takes liberty, bodily integrity, and legal status from identifiable classes of people and delivers the proceeds — intelligence, detention authority, targeting latitude — to a single concentrated seat. Suppression is high (0.75) because persistence depends on actively holding courts out of detention review, restricting monitor access, and maintaining the classification boundaries against challenge; it is a raw structural property, unscaled by power or scope. Theater is just above half (0.52): a large and growing share of activity is legal-performative — memoranda redefining terms, compliance narration, renamed programs — layered over grimly functional detention and interrogation operations. The measurement series run on one shared time grid (points 0–24, seven points, all three metrics at every point). The suppression_requirement series is authored deliberately: enforcement machinery builds steeply through the first third of the interval (court-stripping, secret facilities, classification regimes), peaks, then decays modestly as courts reassert review and some programs close — a net-hardened but partially relaxed enforcement picture, which is why the scalar sits below the series peak.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat should compute this arrangement as prudential legal interpretation preserving state capacity — from inside the security apparatus, each suspension is a reasoned response to a documented threat, and the conventions remain formally honored. The detainee, civilian, and interrogation-subject seats experience the same structure as unreviewable force: no court, no status, no notice. The commander seat splits down the middle — beneficiary of the latitude, bearer of the blowback. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: the security apparatus derives d near 0.0 (collects everything, bears almost nothing, holds arbitrage-grade control over the framework's own interpretation). Field commanders derive low d from their beneficiary role, but the derivation undershoots their true position — they bear reciprocity risk against their own captured personnel and recurring personal legal exposure — hence the explicit override setting powerful-seat d to 0.35. The three victim groups derive d near the full-target end: powerless, trapped, identity of the arrangement defined by what is taken from them. Monitors, courts, treaty bodies, and scholars occupy observational or excluded seats whose directionalities the engine computes from their constrained relationships to the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification guards against a double mislabeling. First, it prevents the genuine coordination function of the conventions themselves — a real mutual-restraint rope — from being tarred with the extraction measured here: the family decomposition isolates the reading's epsilon from the kernel's. Second, it prevents the reading's coordination story ('the conventions still bind us, flexibly applied') from laundering the suspension architecture as benign flexibility: the story is cover, persistence depends on coercion and exit-suppression, and the victims are named. On the R5 interview, the founding problem is live and independently corroborated, so no dead-mandate zombie flag fires — but the theater_ratio trajectory (0.28 to 0.52) records the arrangement's center of gravity migrating from adaptive response toward self-maintaining legal performance, the signature of a mandate drifting toward the maintenance of its maintainer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is one reading of the geneva_conventions_1949 kernel. How much of the measured structure is the kernel''s and how much is this reading''s?',
    'Compare against the sibling stories: the humanitarian_ceiling_reading authors a small victim set and sharply lower epsilon over the same treaty text; the conditional_reciprocity_reading makes epsilon contingent on adversary conduct. Divergence across the family locates what each reading adds.',
    'If the sibling readings compute as ropes over the same text, the extraction measured here belongs to the reading, not the kernel — the conventions are a coordination instrument whose degradation is a choice, not a property of humanitarian law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-relative versus kernel-intrinsic structure: the disagreement between readings is located in the conditionality of obligation (unconditional floor vs reciprocity-triggered vs necessity-deferred).').

omega_variable(
    necessity_empirical_status,
    'Is ''operational necessity'' a demonstrable causal driver of security outcomes, or an unfalsifiable warrant that absorbs whatever practice the security apparatus prefers?',
    'Comparative outcome studies of states facing comparable asymmetric threats that adopted versus rejected the reading, matched for threat level and capability; intelligence-value audits of products obtained specifically through techniques the conventions prohibit.',
    'If necessity claims fail empirical testing, the reading''s foundational axiom loses its instrumental grounding and the arrangement stands exposed as preference-backed suspension — deepening the snare classification and undermining the enforcement rationale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_empirical_status, empirical, 'Whether the necessity warrant is empirically load-bearing or unfalsifiable cover.').

omega_variable(
    unlawful_combatant_boundary_drift,
    'Where does the unlawful-combatant category stabilize — does it expand to absorb anyone armed outside uniformed forces, including civilians who resist occupation, or does it hold at a narrow core?',
    'Track category membership criteria across administrations, theaters, and successive legal memoranda; compare detention populations against the category''s stated definition.',
    'Boundary expansion converts the residual convention constraints into fully suspended ones and deepens the extraction; stabilization leaves a thin tangled-rope residue where some protections survive for some classes of person.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unlawful_combatant_boundary_drift, empirical, 'Expansion dynamics of the category that removes persons from the protection system.').

omega_variable(
    reciprocity_contagion,
    'Does one powerful state''s adoption of this reading degrade the kernel globally — via adversarial imitation, allied normalization, and justification-transfer into the conditional_reciprocity_reading''s trigger findings?',
    'Longitudinal tracking of convention-compliance language in other states'' doctrine following the reading''s institutionalization; citation analysis showing the reading''s vocabulary appearing in other states'' necessity claims.',
    'If contagion is real, this reading exerts structural downstream pressure on the entire kernel family and the measured extraction understates the system-level cost; if contained, the damage remains jurisdiction-local.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_contagion, empirical, 'Whether the reading''s adoption propagates through the constraint network.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__security_maximization_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(gene_tr_t4, geneva_conventions_1949__security_maximization_reading, theater_ratio, 4, 0.34).
narrative_ontology:measurement(gene_tr_t8, geneva_conventions_1949__security_maximization_reading, theater_ratio, 8, 0.41).
narrative_ontology:measurement(gene_tr_t12, geneva_conventions_1949__security_maximization_reading, theater_ratio, 12, 0.46).
narrative_ontology:measurement(gene_tr_t16, geneva_conventions_1949__security_maximization_reading, theater_ratio, 16, 0.49).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_1949__security_maximization_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement(gene_tr_t24, geneva_conventions_1949__security_maximization_reading, theater_ratio, 24, 0.52).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(gene_be_t4, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 4, 0.64).
narrative_ontology:measurement(gene_be_t8, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 8, 0.7).
narrative_ontology:measurement(gene_be_t12, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 12, 0.74).
narrative_ontology:measurement(gene_be_t16, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(gene_be_t24, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 24, 0.83).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gene_su_t4, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 4, 0.7).
narrative_ontology:measurement(gene_su_t8, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 8, 0.78).
narrative_ontology:measurement(gene_su_t12, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(gene_su_t16, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 16, 0.79).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(gene_su_t24, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 24, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, conditional_reciprocity_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the geneva_conventions_1949 kernel: one fixed treaty text, three structurally distinct constraints. The humanitarian_ceiling_reading yields low epsilon with victims only at the margins; the conditional_reciprocity_reading makes epsilon contingent on adversary conduct; this security_maximization_reading yields high epsilon with a fixed victim set (detainees, civilians, interrogation subjects) and a concentrated beneficiary (the security apparatus). Downstream pressure runs from this reading's justificatory vocabulary into the reciprocity reading's non-compliance findings; the ceiling reading stands upstream as the interpretive baseline this reading derogates from. Linked per the epsilon-invariance rule — the colloquial label 'the Geneva Conventions' covers all three, but each carries its own epsilon, victims, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__security_maximization_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
