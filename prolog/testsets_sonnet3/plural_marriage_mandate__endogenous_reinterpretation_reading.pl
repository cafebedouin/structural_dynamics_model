% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto as Legitimate Prophetic Reinterpretation Suspending Plural Marriage
 *   domain: religious/institutional/political theology
 *
 * SUMMARY:
 *   This story instantiates the endogenous reinterpretation reading of the
 *   plural marriage mandate kernel: the 1890 Manifesto is treated, by its own
 *   internal logic, as a genuine act of continuing revelation in which God
 *   temporally suspended (rather than repudiated) the practice of plural
 *   marriage to preserve the church's capacity to perform temple ordinances
 *   and carry out its missionary mission. Under this reading the coordination
 *   function is real — the church needed a single authoritative resolution to
 *   an internally destabilizing and externally besieged practice — and the
 *   beneficiary set (mainstream membership, temple-worthy members, the
 *   missionary program, and the institution itself) gains continuity, legal
 *   peace, and expanded reach. The victim set consists of those who held the
 *   pre-1890 revelation as permanently binding: fundamentalist dissenters who
 *   were excommunicated for continuing the practice, and plural wives already
 *   in existing marriages whose family structures lost religious legitimacy
 *   overnight. This reading's ε is authored from the standing arrangement as
 *   the reading's own adherents understand it: moderate extraction, because
 *   even on its own terms the reinterpretation required active, ongoing
 *   enforcement (excommunication, denial of temple recommends) against those
 *   who read the prior revelation as still binding — a genuine coordination
 *   function does not by itself eliminate the asymmetric cost borne by
 *   dissenters and existing plural families.
 *
 * KEY AGENTS:
 *   - church_institution: agenda_setter/beneficiary (institutional/analytical) — issues and administers the reinterpretation
 *   - mainstream_membership: beneficiary (moderate/constrained) — retains standing, relieved of legal burden
 *   - fundamentalist_dissenters: payer (powerless/trapped) — excommunicated for maintaining prior revelation
 *   - plural_wives_of_1890: payer (powerless/trapped) — lose religious legitimacy for existing marriages
 *   - federal_government: excluded (institutional/arbitrage) — coercive context excluded from this reading's own causal account
 *   - church_historians: observer (analytical/analytical) — assess the endogenous vs exogenous evidentiary record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.38).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.55).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "1890 Manifesto as Legitimate Prophetic Reinterpretation Suspending Plural Marriage").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious/institutional/political theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, '77bb743c-ac23-4f00-9926-42e809ebfc60').
narrative_ontology:cs_kernel_codification('77bb743c-ac23-4f00-9926-42e809ebfc60', formalized).
narrative_ontology:cs_authority_grounding('77bb743c-ac23-4f00-9926-42e809ebfc60', lineage).
narrative_ontology:cs_interpretation_layer_present('77bb743c-ac23-4f00-9926-42e809ebfc60').
narrative_ontology:cs_reading_relation('77bb743c-ac23-4f00-9926-42e809ebfc60', plural_marriage_mandate__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('77bb743c-ac23-4f00-9926-42e809ebfc60', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('77bb743c-ac23-4f00-9926-42e809ebfc60', foundational, prophetic_authority_supremacy).
narrative_ontology:cs_axiom_status(prophetic_authority_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('77bb743c-ac23-4f00-9926-42e809ebfc60', prophetic_authority_supremacy, deontological).
narrative_ontology:cs_axiom('77bb743c-ac23-4f00-9926-42e809ebfc60', foundational, continuing_revelation_can_suspend_prior_mandate).
narrative_ontology:cs_axiom_status(continuing_revelation_can_suspend_prior_mandate, holdable).
narrative_ontology:cs_axiom_grounding('77bb743c-ac23-4f00-9926-42e809ebfc60', continuing_revelation_can_suspend_prior_mandate, theological).
narrative_ontology:cs_reference_frame('77bb743c-ac23-4f00-9926-42e809ebfc60', eternal_plural_marriage_mandate_1843_revelation).
narrative_ontology:cs_drift_state('77bb743c-ac23-4f00-9926-42e809ebfc60', manifesto_1890_announcement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('77bb743c-ac23-4f00-9926-42e809ebfc60', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_membership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, missionary_program).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, temple_worthy_members).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissenters).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_wives_of_1890).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__endogenous_reinterpretation_reading, prophetic_authority_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The church presidency issues the Manifesto as a revealed directive, then administers its consequences: statehood is secured, temple operations continue, missionary work expands, and federal seizure of church assets ends. The institution frames the change as continuing revelation rather than concession, and subsequently enforces compliance through excommunication of members who continue plural marriage.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution, beneficiary).

% The majority of members accept the Manifesto as authoritative revelation, retain temple access and social standing, and are relieved of the increasingly costly social and legal burden of plural marriage. Their acceptance is shaped by trust in prophetic authority and by the practical benefits of ending federal antagonism.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_membership, beneficiary,
    moderate, generational, constrained, national).

% Global missionary expansion, previously hampered by plural marriage's reputational cost, becomes viable at scale after the Manifesto; this is a structural beneficiary condition rather than an agent with its own choices.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, missionary_program, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(plural_marriage_mandate__endogenous_reinterpretation_reading, missionary_program).

% Members and splinter communities who maintain that the original revelation on plural marriage remains binding are excommunicated, stripped of temple privileges, and cast out of the institutional body they regard as the sole legitimate priesthood authority. Their exit options are effectively nonexistent within the tradition they were raised in — leaving means abandoning the entire framework of salvation they were taught, not merely a practice.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissenters, payer,
    powerless, generational, trapped, regional).

% Women already in plural marriages at the time of the Manifesto face sudden loss of religious legitimacy for their existing family structures, ambiguous legal and social status, and in many cases economic precarity as households are pressured to reorganize. They did not choose the timing of the reinterpretation and bear its costs directly in their domestic lives.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_wives_of_1890, payer,
    powerless, biographical, trapped, local).

% Applied escheatment, disenfranchisement, and imprisonment pressure that precipitated the timing of the Manifesto, but plays no role inside the church's own account of the revelation — the reading under analysis treats the change as internally generated, so the federal government's coercive role is structurally external to this constraint's stated mechanism even though it shaped the historical moment.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, biographical, arbitrage, national).

% Assess archival records, contemporaneous correspondence, and the timing correlation between federal pressure and the revelation's announcement, without institutional stake in either the endogenous or exogenous account being correct.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, church_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative resolution to an internally destabilizing practice, allowing the institution's membership, leadership, and external relations to reorganize around one settled directive instead of fracturing over contested continuation of plural marriage.
% TRANSFER_FUNCTION: Moves institutional continuity, temple access, and missionary viability toward the mainstream body and its leadership, while moving religious legitimacy, family stability, and community standing away from those who maintain the pre-1890 practice as binding.
% ABSENT_VOICES: Plural wives whose marriages predated the Manifesto had no formal voice in the revelatory process announced by an all-male priesthood hierarchy; fundamentalist adherents who trace authority through the same revelatory chain are excluded from the conversation that redefines that chain's current output.
% DISAPPEARANCE_RATIONALE: If the Manifesto's authority were withdrawn, the church would face renewed legal jeopardy, loss of the institutional settlement with the federal government, and a doctrinal crisis reopening the question of whether continuing revelation can suspend a divinely mandated practice — mainstream membership, temple operations, and missionary access would all be destabilized, and fundamentalist splinter groups would gain a claim to primary legitimacy.
% FOUNDING_PROBLEM: Escalating federal prosecution, disenfranchisement, and asset seizure under anti-polygamy legislation threatened the church's institutional survival, its temple ordinances, and Utah's path to statehood.
% FOUNDING_PROBLEM_CORROBORATION: Historians outside the church's institutional apparatus (including LDS-affiliated but professionally independent historians publishing in peer-reviewed venues) corroborate that the specific federal legal crisis that precipitated the Manifesto ended with statehood and amnesty; the church's own continuing invocation of the revelation as doctrinally settled is self-asserted from within the benefiting institution and is not independently corroborated as a theological necessity distinct from the resolved legal crisis.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).
:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.20 pre-crisis to 0.38 by the mid-1930s as the enforcement apparatus (excommunication proceedings, denial of temple ordinances to continuing polygamists) matures and stabilizes into routine institutional practice. Suppression peaks around 1904 (the Second Manifesto and Reed Smoot hearings, which forced more aggressive enforcement against continuing practitioners) before settling to a still-substantial plateau — this reading requires ongoing active enforcement to hold, which is structurally inconsistent with treating the reinterpretation as a costless doctrinal clarification. Theater ratio is modest and rises slightly as some enforcement activity becomes more procedural/performative (formal disciplinary councils) rather than purely functional.
 *
 * DIRECTIONALITY LOGIC:
 *   The church institution and mainstream membership sit near the beneficiary end: they retain or gain temple access, legal peace, and missionary reach, and their exit options are either analytical (the institution sets the terms) or constrained-but-net-positive (members trade a costly practice for continued belonging). Fundamentalist dissenters and plural wives of 1890 sit near the full-target end: they bear the cost of a directive whose timing and content they did not set, and their exit options are trapped — leaving means abandoning the entire salvific framework they were raised inside, not simply changing institutional affiliation. This is a structurally high-cost exit, which is why their directionality is not merely 'symmetric disagreement' but genuine target-side extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal legal existential threat to the institution) is genuinely dead by any external corroboration — statehood was achieved, asset seizure ended, prosecution ceased. Yet the church's continued framing of the Manifesto as revealed doctrinal truth (rather than resolved crisis management) persists indefinitely, which is exactly the founding-problem-status mismatch the R5 interview is designed to surface: status=dead paired with an institution that treats the arrangement as still doctrinally necessary is a candidate for zombie/capture flagging even under this reading's own generous framing. This reading resists full mandatrophy characterization only because the coordination function it claims (preserving temple access and missionary viability) remains genuinely operative today, not merely inertial — that is what keeps this a rope-claim rather than a piton-claim, and is precisely the tension this story is authored to expose rather than resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_authenticity_vs_coercion_timing,
    'Was the 1890 revelation a genuine independent act of divine communication, or was its timing and content substantially determined by federal legal and financial coercion such that the ''revelation'' framing is itself constructed post hoc to legitimate a forced concession?',
    'This is not resolvable by historical evidence alone — the correlation between federal pressure (Edmunds-Tucker Act enforcement, imminent disincorporation, escheatment proceedings) and the Manifesto''s timing is well documented and undisputed; what remains contested is the theological interpretation of that correlation, which is a matter of religious epistemology rather than empirical fact.',
    'If the coercion account is accepted as primary, this constraint''s claimed_type (rope, genuine coordination via legitimate revelation) would not survive — the same structural facts would instead support the exogenous_override_reading or institutional_pragmatism_reading, both authored as separate constraints. This omega marks the exact fork point between the three kernel readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_authenticity_vs_coercion_timing, conceptual, 'Whether the endogenous reinterpretation account is theologically primary or a post hoc legitimation of externally coerced change — the central fork between the three kernel readings.').

omega_variable(
    victim_scope_of_reinterpretation,
    'Does the ''moderate constraint shift'' framing (practice suspended, doctrine retained) understate the cost actually borne by fundamentalist dissenters and pre-1890 plural wives, given that their entire salvific framework was invalidated in practice even while affirmed in theory?',
    'Comparative analysis of excommunication records, testimony from descendants of pre-1890 plural families, and fundamentalist community accounts of the schism''s severity versus the mainstream church''s own characterization of the change as a minor practical adjustment.',
    'If the victim cost is understated, the authored extractiveness (0.38) is too low relative to the lived experience of the payer stakeholders, and the classification could shift toward tangled_rope if the extraction is judged asymmetric enough relative to the coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_scope_of_reinterpretation, empirical, 'Whether this reading''s moderate-extraction framing understates the severity of costs borne by fundamentalist dissenters and existing plural wives.').

omega_variable(
    continuing_revelation_falsifiability,
    'Is the claim that God can temporally suspend a previously eternal-and-binding commandment falsifiable or evaluable by any evidence external to the tradition''s own authority structure, or is it definitionally insulated from disconfirmation?',
    'None available in principle — this is a first-order theological claim about prophetic authority, not an empirical hypothesis; corroboration can only ever come from within traditions that already accept the premise of ongoing revelation.',
    'If the claim is definitionally unfalsifiable, this reading''s coordination-function framing rests on an axiom (prophetic_authority_supremacy) that cannot itself be independently corroborated, which is directly relevant to the founding_problem_corroboration finding that no truly outside-the-benefiting-parties attestation exists for the doctrinal necessity claim, only for the resolved legal crisis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuing_revelation_falsifiability, conceptual, 'Whether the theological claim underlying this reading is falsifiable by any evidence outside the tradition''s own authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 1862, 1935).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1862, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1862, 0.05).
narrative_ontology:measurement(plur_tr_t1882, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1882, 0.1).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.15).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.22).
narrative_ontology:measurement(plur_tr_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(plur_tr_t1935, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1935, 0.2).

% Extraction over time
narrative_ontology:measurement(plur_be_t1862, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1862, 0.2).
narrative_ontology:measurement(plur_be_t1882, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1882, 0.28).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.32).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.36).
narrative_ontology:measurement(plur_be_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.38).
narrative_ontology:measurement(plur_be_t1935, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1935, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1862, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1862, 0.15).
narrative_ontology:measurement(plur_su_t1882, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1882, 0.35).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.5).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.65).
narrative_ontology:measurement(plur_su_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement(plur_su_t1935, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1935, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.1).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the plural_marriage_mandate kernel. The endogenous_reinterpretation_reading (this file) authors the arrangement as genuine continuing revelation with a real coordination function (rope-leaning, ε=0.38). The exogenous_override_reading authors the same historical event as federal coercion with no legitimate reinterpretation occurring (expected higher ε, snare-leaning, foreclosed by this reading's core premise since both cannot be true within one theological framework). The institutional_pragmatism_reading authors the revelation narrative itself as a legitimating cover story for survival-driven capitulation (expected tangled_rope, coexisting with this reading as a live position held by historians and critics who do not need to foreclose the church's own internal theological account to hold their structural-pragmatism account). All three share the same historical event and beneficiary/victim populations but diverge sharply in ε and claimed_type because they diverge on what actually happened at the level of causal and theological mechanism — per the ε-invariance principle, this is three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
