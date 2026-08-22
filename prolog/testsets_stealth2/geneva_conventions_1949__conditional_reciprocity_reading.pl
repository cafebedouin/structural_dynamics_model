% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions as Conditional Reciprocity Restraints
 *   domain: international_humanitarian_law/law_of_armed_conflict/political_philosophy
 *
 * SUMMARY:
 *   Under the conditional reciprocity reading, the 1949 Geneva Conventions
 *   operate as mutual restraints between belligerents whose full application
 *   is contingent: a fighter receives combatant-level protection by
 *   satisfying Article 4 (organized command, distinctive sign, arms carried
 *   openly, conduct in accordance with the law), and a belligerent's
 *   protections are sustained by the expectation that the adversary
 *   reciprocates. Civilian immunity is preserved but narrowed wherever
 *   proportionality calculations judge military advantage sufficient. The
 *   practical effect in asymmetric conflict is a two-tier structure: regular
 *   state forces enjoy the full protective regime while detained irregulars
 *   are classified outside it and civilians near hostilities bear the residue
 *   of proportionality judgments. This file is ONE READING of the
 *   geneva_conventions_1949 kernel (fixed text, four Conventions); the
 *   sibling readings — humanitarian_ceiling (absolute minimums regardless of
 *   compliance) and security_maximization (necessity-based suspension) — are
 *   separate constraints with their own epsilon values and victim sets,
 *   linked through network.affects_constraints. Per the epsilon-referent
 *   rule, the extractiveness authored here measures the standing
 *   conditional-reciprocity arrangement as this reading itself assesses it:
 *   the reading regards conditionality as legitimate earned forfeiture, so
 *   the authored value counts the arrangement's real channelled costs
 *   (detention without combatant status, proportionality-borne civilian harm,
 *   hostage-taking of one side's protections by the other's breaches) without
 *   counting the degradation the reading deems deserved as unjust extraction.
 *   KEY AGENTS (by structural relationship): - regular_state_militaries:
 *   primary beneficiary and administering interpreter
 *   (institutional/constrained) — collects reciprocity protection for their
 *   own captured personnel and holds classification and proportionality
 *   authority - smaller_contracting_states: secondary beneficiaries
 *   (moderate/constrained) — rely on the reciprocity shield with little
 *   interpretive weight - irregular_armed_groups: primary target
 *   organizations (moderate/trapped) — their members fall outside Article 4
 *   by mode of fighting - captured_irregular_detainees: sharpest individual
 *   target (powerless/trapped) — custody without combatant status -
 *   civilians_in_asymmetric_conflict_zones: dual-positioned
 *   (powerless/trapped) — residual immunity narrowed by proportionality
 *   balancing - icrc_and_protecting_powers: embedded monitor
 *   (organized/identity_locked) — documents deviation, compels nothing -
 *   human_rights_advocacy_organizations: excluded voice (organized/mobile) —
 *   contests narrowing classifications from outside the decision rooms -
 *   international_criminal_tribunals: retrospective adjudicator
 *   (institutional/analytical) — episodic enforcement feeding back into
 *   manuals
 *
 * KEY AGENTS:
 *   - regular_state_militaries: primary beneficiary and administering interpreter (institutional/constrained)
 *   - smaller_contracting_states: secondary beneficiaries relying on the reciprocity shield (moderate/constrained)
 *   - irregular_armed_groups: primary target organizations whose mode of fighting excludes their members from Article 4 (moderate/trapped)
 *   - captured_irregular_detainees: individual bearers of the degradation (powerless/trapped)
 *   - civilians_in_asymmetric_conflict_zones: dual-positioned recipients of narrowed immunity (powerless/trapped)
 *   - icrc_and_protecting_powers: neutral monitor locked into its intermediary role (organized/identity_locked)
 *   - human_rights_advocacy_organizations: excluded contestants of the narrowing readings (organized/mobile)
 *   - international_criminal_tribunals: retrospective adjudicators with episodic reach (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.62).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.55).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions as Conditional Reciprocity Restraints").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "international_humanitarian_law/law_of_armed_conflict/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, 'cddafe20-9918-47f9-bf30-f0299feb8bc8').
narrative_ontology:cs_kernel_codification('cddafe20-9918-47f9-bf30-f0299feb8bc8', fixed_text).
narrative_ontology:cs_authority_grounding('cddafe20-9918-47f9-bf30-f0299feb8bc8', lineage).
narrative_ontology:cs_interpretation_layer_present('cddafe20-9918-47f9-bf30-f0299feb8bc8').
narrative_ontology:cs_reading_relation('cddafe20-9918-47f9-bf30-f0299feb8bc8', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('cddafe20-9918-47f9-bf30-f0299feb8bc8', geneva_conventions_1949__security_maximization_reading, influences).
narrative_ontology:cs_axiom('cddafe20-9918-47f9-bf30-f0299feb8bc8', foundational, combatant_status_is_conditionally_earned).
narrative_ontology:cs_axiom_status(combatant_status_is_conditionally_earned, holdable).
narrative_ontology:cs_axiom_grounding('cddafe20-9918-47f9-bf30-f0299feb8bc8', combatant_status_is_conditionally_earned, conventional).
narrative_ontology:cs_axiom('cddafe20-9918-47f9-bf30-f0299feb8bc8', foundational, reciprocity_is_the_compliance_engine).
narrative_ontology:cs_axiom_status(reciprocity_is_the_compliance_engine, holdable).
narrative_ontology:cs_axiom_grounding('cddafe20-9918-47f9-bf30-f0299feb8bc8', reciprocity_is_the_compliance_engine, instrumental).
narrative_ontology:cs_axiom('cddafe20-9918-47f9-bf30-f0299feb8bc8', secondary, civilian_immunity_yields_to_proportionality).
narrative_ontology:cs_axiom_status(civilian_immunity_yields_to_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('cddafe20-9918-47f9-bf30-f0299feb8bc8', civilian_immunity_yields_to_proportionality, conventional).
narrative_ontology:cs_reference_frame('cddafe20-9918-47f9-bf30-f0299feb8bc8', reciprocal_restraint_between_regular_armies).
narrative_ontology:cs_drift_state('cddafe20-9918-47f9-bf30-f0299feb8bc8', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cddafe20-9918-47f9-bf30-f0299feb8bc8', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, regular_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, smaller_contracting_states).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, captured_irregular_detainees).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_asymmetric_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_asymmetric_conflict_zones).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, article_4_lawful_combatancy_criteria).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, reciprocal_enforcement_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Field disciplined armies under the four Conventions; their captured personnel receive registered, visited, and repatriated prisoner treatment whenever the adversary reciprocates. They hold the working authority to classify opposing forces, run proportionality assessments before strikes, and determine which detainees fall outside the Article 4 criteria, and they carry disproportionate interpretive weight in diplomatic conferences and military manuals. Formal denunciation of the treaties is legally available but reputationally and practically costly, so exit is effectively closed.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, regular_state_militaries, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, regular_state_militaries, agenda_setter).

% Maintain small regular forces whose principal wartime protection is the conventions' reciprocity shield; they ratified early, comply visibly, and contribute almost nothing to interpretation. When a stronger adversary invokes conditionality against them or mistreats their prisoners, their recourse is limited to ICRC publicity and diplomatic protest.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, smaller_contracting_states, beneficiary,
    moderate, generational, constrained, national).

% Fight insurgencies and civil wars without fixed insignia, centralized command, or openly carried arms; under the conditional reading their members fall outside the Article 4 criteria, exposing captured fighters to detention and prosecution rather than prisoner-of-war status. Organizing into visible compliance offers a path toward protections but also makes the force easier to target; leaving the conflict is not available to the organization as such.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_armed_groups, payer,
    moderate, biographical, trapped, regional).

% Individuals in custody classified as having fought outside the lawful-combatant criteria. They fall outside the full detention regime: no combatant immunity, exposure to interrogation and to domestic criminal prosecution, release governed by administrative and sentencing timelines rather than armistice repatriation. They have no procedural seat at which their classification is contested.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, captured_irregular_detainees, payer,
    powerless, immediate, trapped, regional).

% Receive the Fourth Convention's baseline immunity and the Common Article 3 minimums, which remain theirs irrespective of how nearby fighters dress or organize. In areas of insurgent activity their protection is mediated by proportionality calculations that tolerate incidental harm where military advantage is judged sufficient; they cannot leave the battlespace and have no voice in the balancing that prices their proximity.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_asymmetric_conflict_zones, beneficiary,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_asymmetric_conflict_zones, payer).

% Visit places of detention, register prisoners, transmit family messages, and promote dissemination of the law. Access to detainees depends on strict neutrality, so the intermediary role cannot be set aside without destroying the very access that gives it value; the institution documents deviations and negotiates improvements but cannot compel a classification reversal or a repatriation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, icrc_and_protecting_powers, observer,
    organized, generational, identity_locked, global).

% Campaign against narrowing classifications and diminished detention standards, publishing documentation and litigating in domestic courts. They hold no seat in treaty diplomacy and none in the military classification decisions that determine who receives protections; their influence runs through publicity and litigation rather than through the rooms where the determinations are made.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, human_rights_advocacy_organizations, excluded,
    organized, generational, mobile, global).

% Adjudicate grave breaches and war-crimes charges after the fact, under mandates limited by state consent and prosecutorial selectivity. Their jurisprudence feeds back into military manuals and doctrine, but enforcement lands unevenly across violators, and they play no role in the forward-looking classification decisions that allocate protections during hostilities.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_criminal_tribunals, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__conditional_reciprocity_reading, regular_state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__conditional_reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of wartime restraint between armed forces that cannot verify intentions in advance: standardized treatment of prisoners, wounded, and civilians lowers the cost of compliance for each side by making restraint reciprocal and predictable, and gives each side a concrete stake (its own captured personnel) in honoring the rules.
% TRANSFER_FUNCTION: Moves protection and legal status: full detention-and-trial safeguards flow to fighters who satisfy the lawful-combatant criteria and to belligerents whose adversaries comply; classification discretion and proportionality judgment flow to state militaries; the costs of degraded protection — detention without combatant status, incidental civilian harm — flow to irregular fighters and to civilians proximate to hostilities.
% ABSENT_VOICES: The people whose protections are conditioned away are absent from every decision room: detainees cannot contest their classification, civilians have no seat in the proportionality balancing that prices their exposure, and irregular groups were scarcely represented in the diplomatic conferences that defined lawful combatancy. Affected populations speak only through advocacy organizations, which hold no vote in treaty interpretation or military classification.
% DISAPPEARANCE_RATIONALE: If the conditional reciprocity framework vanished overnight, the machinery organized around it — prisoner registration and exchange, camp inspection, protecting-power assignments, military-law training pipelines, tribunal jurisdiction definitions — would all require reconstruction, and the conduct of war would reorganize around naked reciprocity-of-fear or open assertions of necessity. Every named seat's arrangements depend on the framework existing in something like its current form, including the seats that suffer under it.
% FOUNDING_PROBLEM: Industrial-scale war between regular armies produced mass captivity, abandoned wounded, and reprisal spirals; the 1949 Conventions were built to stabilize mutual restraint between state militaries after the Second World War's captivity atrocities, using reciprocity as the compliance mechanism available in a world with no central enforcer of battlefield law.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's reality is corroborated from outside the benefiting parties: ICRC historical documentation of wartime captivity and reprisal cycles, and independent military-historical scholarship, attest both the original problem and the design logic that answered it. Those same external sources now dispute whether the founding problem remains the live one — contemporary armed conflicts are predominantly asymmetric, and humanitarian-law scholarship outside state delegations argues the reciprocity mechanism fits that setting worst. No attestation that the arrangement still fits its problem comes from outside state militaries and their legal advisers; the dispute is genuine and unresolved.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.62 because the arrangement genuinely constrains state violence in symmetric war (registered POW treatment, camp inspection, repatriation machinery all function) while channeling substantial costs onto irregulars and civilians in asymmetric conflict, where the reading's conditionality does most of its work. Suppression is 0.55 and is authored as a raw structural property — it is deliberately NOT scaled by power or scope; the engine owns any scaling of extractiveness only. Suppression here is institutional rather than physical: treaty ratification locks parties in, reputational and prosecutorial exposure penalizes open repudiation, and the classification apparatus resists reinterpretation, while formal alternatives (the Additional Protocol I model) remain legally available to non-parties and advocates. Accessibility collapse is 0.45 because alternatives have not collapsed — the ceiling reading persists in the practice of AP I parties and in advocacy; the reciprocity frame is entrenched but not totalizing. Resistance is 0.6: humanitarian organizations, several treaty parties, and parts of the academy actively contest the narrowing classifications. Theater is 0.45: camp visits, registration, and dissemination are functional, but a growing share of legal activity consists of memos, reviews, and characterizations that ratify predetermined classifications. The temporal series runs on one shared eight-point grid (every tracked metric authored at every point, per the alignment rule). The extractiveness steps are event-driven rather than cyclical: the 1968 elevation tracks the Vietnam-era use of irregularity doctrine to deny status; the 1977 dip tracks the disciplining pressure of the Additional Protocols among ratifiers; the 2001 step tracks the unlawful-combatant classification practice; the post-2010 plateau reflects normalization of classification control alongside maturing enforcement machinery. Coalition prospects for the powerless seats are poor: detainees and civilians are scattered across discrete conflicts with no shared organizational surface, which is why their low power does not convert into bargaining leverage despite their numbers.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the regular_state_militaries seat the arrangement is an earned order: restraint is incentive-compatible, one's own captured soldiers are protected, and denial of status to those who fight out of uniform is the system working as designed. From the captured_irregular_detainee and civilian seats the same structure operates as a licensing architecture: their protections are the adjustable variable in someone else's compliance ledger and someone else's targeting arithmetic. Smaller contracting states experience a third position: a shield whose value depends entirely on great-power self-restraint they cannot enforce. The ICRC seat carries an identity-lock dynamic of the institutional kind — strict neutrality is not a strategy it selects but constitutive of what it is; abandoning the intermediary role to protest classifications would destroy the access that makes the role possible, so the organization absorbs documented deviation rather than exiting. If that identity frame broke — if the ICRC ever publicly refused access terms — the monitoring pillar of enforcement would weaken faster than any tribunal replacement could form. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to real structural receipt: regular_state_militaries collect both the reciprocity protection of their own personnel and the classification/proportionality authority the reading reserves to disciplined forces, so their derived directionality sits near the beneficiary pole despite their constrained exit (ratification and reputation bind them). Smaller_contracting_states derive low-to-moderate directionality — genuine shield, no interpretive rent. The victim declarations drive the target poles: irregular_armed_groups and captured_irregular_detainees sit near full-target, their exit trapped by the battlespace itself. Civilians_in_asymmetric_conflict_zones are dual-positioned (beneficiary of residual immunity, bearer of proportionality's residue) and are carried in the victims array because in the asymmetric settings where this reading binds, the net flow runs against them; their situation text records the residual benefit so the dual position is visible. The ICRC sits near symmetric — it serves the system and is harmed by its failures in equal measure — and the tribunals occupy the analytical seat. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — industrial interstate war producing mass captivity, abandoned wounded, and reprisal spirals between regular armies — has receded as the dominant conflict form, while the arrangement persists and has been repurposed for asymmetric conflicts its reciprocity engine fits poorly. This is a transformation, not a death: the mandate is contested rather than dead, which is why founding_problem_status is authored 'contested' rather than 'dead'. The classification guards against mislabeling in both directions. Calling the arrangement pure extraction ignores the demonstrated coordination function — POW exchange machinery, camp inspection, and the Common Article 3 floor have repeatedly reduced suffering even between adversaries who otherwise breached. Calling it pure coordination ignores the conditionality's asymmetric incidence: the criteria for full protection are written in the idiom of regular armies, the discretion to classify belongs to the party with the disciplined force, and the costs of degradation land on actors with no seat in the drafting or classifying rooms. The R5 mismatch consumer should note the authored combination — founding_problem_status 'contested' with disappearance_verdict 'world_rearranges' — which flags the arrangement as persisting past its design environment without asserting that its function has wholly atrophied; the theater ratio's rise toward 0.45 is the observable symptom of that persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_disagreement_structure,
    'This story instantiates one reading (conditional_reciprocity) of the geneva_conventions_1949 kernel: which structural element separates it from its siblings — is protection conditional on adversary compliance and on the fighter satisfying Article 4 criteria, or does some protection attach unconditionally?',
    'Doctrinal convergence in state practice, tribunal jurisprudence, and treaty diplomacy — specifically whether Common Article 3-type minimums come to be treated as unconditional floors or as themselves reciprocal.',
    'Adoption of the humanitarian_ceiling sibling restores protections to irregulars and narrows the proportionality license, lowering epsilon substantially; adoption of the security_maximization sibling generalizes suspension beyond adversary conduct, raising epsilon and shifting the arrangement toward purely enforced extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_disagreement_structure, conceptual, 'Kernel-reading contest: the disagreement is located in conditionality of attachment and survival of any floor under total non-compliance.').

omega_variable(
    reciprocity_necessity_vs_construction,
    'Is conditioning protection on reciprocity a structural necessity of humanitarian law in a world without a central enforcer, or a constructed design choice that concentrates advantage on states fielding regular armies?',
    'Comparative compliance outcomes between reciprocity-coupled regimes and decoupled regimes (Additional Protocol I parties, which severed civilian protection from reciprocity) across matched conflicts.',
    'If decoupled regimes sustain comparable restraint, conditionality is constructed preference and the arrangement''s costs to irregulars and civilians are less defensible as coordination overhead; if decoupled regimes erode, conditionality is load-bearing and part of the measured cost is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_necessity_vs_construction, empirical, 'Whether the conditionality feature is necessary machinery or advantaged construction.').

omega_variable(
    classification_discretion_abuse,
    'What share of protection-degradation decisions reflects verified adversary non-compliance with Article 4 criteria versus discretionary classification serving operational convenience?',
    'Systematic audit of detention-classification records against independent assessments of whether the detained force met the organized-command, insignia, and open-arms-bearing criteria at capture.',
    'A high discretionary share raises effective extraction above the authored base value and supports drift of the arrangement toward pure extraction riding on a coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_discretion_abuse, empirical, 'How much degradation is earned forfeiture versus discretionary labeling.').

omega_variable(
    proportionality_good_faith,
    'Are proportionality assessments governing civilian immunity genuine ex ante balancings, or post-hoc justifications of already-planned operations?',
    'Comparison of pre-strike collateral-damage estimates with independent casualty accounting and post-strike review findings across a sample of operations.',
    'A post-hoc pattern attributes more of the civilian toll to the reading''s license structure rather than to adversary conduct, raising the theater ratio and shifting extraction attribution toward the classifying militaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_good_faith, empirical, 'Whether the proportionality mechanism balances or rationalizes.').

omega_variable(
    unconditional_floor_survival,
    'Does any protection floor — the Common Article 3 minimums — survive total adversary non-compliance, thereby bounding the degradation this reading licenses?',
    'Textual and application-history analysis of Common Article 3 in conflicts in which every party breached; examination of whether any party has treated CA3 obligations as discharged by adversary conduct.',
    'If the floor is unconditional, the reading''s degradation license is capped and its extraction is bounded; if conditionality reaches the floor, the reading converges structurally toward the security_maximization sibling and the victim set expands to all persons in non-compliant conflicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unconditional_floor_survival, conceptual, 'Existence and reach of an unconditional floor inside the conditional structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.2).
narrative_ontology:measurement(gene_tr_t1958, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1958, 0.22).
narrative_ontology:measurement(gene_tr_t1968, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1968, 0.3).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1977, 0.33).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(gene_tr_t2025, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.42).
narrative_ontology:measurement(gene_be_t1958, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1958, 0.46).
narrative_ontology:measurement(gene_be_t1968, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1968, 0.55).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1977, 0.52).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2001, 0.62).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(gene_be_t2025, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.35).
narrative_ontology:measurement(gene_su_t1958, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1958, 0.38).
narrative_ontology:measurement(gene_su_t1968, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1968, 0.45).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1977, 0.48).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2010, 0.57).
narrative_ontology:measurement(gene_su_t2025, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Geneva Conventions'. The single natural-language concept covers three structurally distinct claims with materially different epsilon values: the conditional_reciprocity reading (this file, epsilon ~0.62 — conditionality licenses selective degradation concentrated on irregulars and civilians); the humanitarian_ceiling reading (lower epsilon — absolute floors sever protection from adversary conduct, shrinking the victim set); and the security_maximization reading (higher epsilon — necessity-based suspension generalizes degradation beyond adversary conduct). The fixed 1949 text is the upstream common ground all three readings cite; this reading is midstream in causal influence — its classification categories (lawful/unlawful combatant) supply the legal raw material the security_maximization sibling extends, which is why the reading_relations edge to that sibling is 'influences'. Each story carries its own beneficiaries, victims, and metrics; none hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
