% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto as Legitimate Prophetic Reinterpretation Suspending Plural Marriage
 *   domain: religious institutional history / commitment systems / political theology
 *
 * SUMMARY:
 *   This story instantiates the endogenous-reinterpretation reading of the
 *   plural marriage mandate kernel: the 1890 Manifesto is treated, from
 *   within the church's own theological framework, as an authentic act of
 *   continuing revelation — God temporarily suspending a temporal practice
 *   (plural marriage) to preserve an eternal institutional mission (temple
 *   ordinances, missionary work, and the church's continued existence). This
 *   is a rope-type coordination reading: the church membership coordinates
 *   around a new, singular prophetic directive that resolves an existential
 *   institutional crisis, and — by this reading's own lights — most
 *   participants are net beneficiaries of the resolution. The victims are not
 *   the general membership but the minority who hold that the original 1843
 *   revelation was never actually rescinded by God and who are excommunicated
 *   for continued practice. Sibling readings (exogenous_override_reading,
 *   institutional_pragmatism_reading) treat the same historical episode very
 *   differently — the federal-coercion reading denies any legitimate
 *   doctrinal content to the change, and the institutional-pragmatism reading
 *   treats the revelation claim itself as a legitimating narrative laid over
 *   a survival-driven capitulation. Per the ε-invariance principle, those are
 *   different constraints with different ε values and are NOT folded into
 *   this file; this file authors only the endogenous reading's own internally
 *   consistent structure.
 *
 * KEY AGENTS:
 *   - church_institution: agenda_setter/beneficiary (institutional/analytical) — issues and administers the reinterpretation
 *   - mainstream_church_membership: beneficiary (moderate/constrained) — retains standing and worship access
 *   - fundamentalist_dissenters: payer (powerless/trapped) — excommunicated for maintaining the original reading
 *   - church_historians_and_theologians: observer (analytical/analytical) — assesses corroboration independent of institutional interest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.42).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.55).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "1890 Manifesto as Legitimate Prophetic Reinterpretation Suspending Plural Marriage").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious institutional history / commitment systems / political theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, '5f8fba28-12c5-4581-b7e2-e1e815eb7335').
narrative_ontology:cs_kernel_codification('5f8fba28-12c5-4581-b7e2-e1e815eb7335', formalized).
narrative_ontology:cs_authority_grounding('5f8fba28-12c5-4581-b7e2-e1e815eb7335', lineage).
narrative_ontology:cs_interpretation_layer_present('5f8fba28-12c5-4581-b7e2-e1e815eb7335').
narrative_ontology:cs_reading_relation('5f8fba28-12c5-4581-b7e2-e1e815eb7335', plural_marriage_mandate__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('5f8fba28-12c5-4581-b7e2-e1e815eb7335', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('5f8fba28-12c5-4581-b7e2-e1e815eb7335', foundational, prophetic_authority_can_licitly_suspend_temporal_practice).
narrative_ontology:cs_axiom_status(prophetic_authority_can_licitly_suspend_temporal_practice, holdable).
narrative_ontology:cs_axiom_grounding('5f8fba28-12c5-4581-b7e2-e1e815eb7335', prophetic_authority_can_licitly_suspend_temporal_practice, theological).
narrative_ontology:cs_axiom('5f8fba28-12c5-4581-b7e2-e1e815eb7335', foundational, continuing_revelation_operates_independent_of_external_coercion).
narrative_ontology:cs_axiom_status(continuing_revelation_operates_independent_of_external_coercion, holdable).
narrative_ontology:cs_axiom_grounding('5f8fba28-12c5-4581-b7e2-e1e815eb7335', continuing_revelation_operates_independent_of_external_coercion, theological).
narrative_ontology:cs_reference_frame('5f8fba28-12c5-4581-b7e2-e1e815eb7335', id_1843_plural_marriage_revelation_as_binding_eternal_commandment).
narrative_ontology:cs_drift_state('5f8fba28-12c5-4581-b7e2-e1e815eb7335', post_manifesto_settled_doctrine, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5f8fba28-12c5-4581-b7e2-e1e815eb7335', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_church_membership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, returning_missionaries).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, temple_going_members).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissenters).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, excommunicated_plural_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The church president issues the Manifesto as a received revelation, announcing intent to submit to federal law and counsel members to refrain from plural marriage. The institution administers this directive through excommunication proceedings against those who continue the practice, and its survival — statehood eligibility, restored property, temple operation, missionary access — depends on the directive being received as genuine prophetic guidance rather than mere capitulation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution, beneficiary).

% Members who accept the Manifesto retain temple access, continue in good standing, and are relieved of legal jeopardy and social stigma tied to plural marriage. They receive the revelation narrative as authoritative and reorganize family and community life around monogamous norms sanctioned by continuing revelation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, mainstream_church_membership, beneficiary,
    moderate, generational, constrained, national).

% Missionary work and public proselytizing had been severely hampered by the polygamy controversy; the Manifesto removes a major barrier to respectable public engagement, allowing missionaries to operate with reduced hostility and the church to expand its reach.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, returning_missionaries, beneficiary,
    moderate, generational, mobile, global).

% Temple ordinances, central to salvific practice, had been threatened by federal seizure of temple property during the anti-polygamy campaigns. Compliance with the Manifesto restores and secures temple access for members whose eternal ordinances depend on institutional continuity.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, temple_going_members, beneficiary,
    moderate, generational, constrained, national).

% Members who hold that the original revelation commanding plural marriage was never rescinded by God, only suspended by man, face excommunication for continuing the practice they believe remains divinely mandated. They lose institutional standing, family and social networks built around church membership, and access to ordinances, while asserting continued fidelity to what they read as the unaltered original commandment.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_dissenters, payer,
    powerless, generational, trapped, regional).

% Existing plural families at the time of the Manifesto and afterward face a choice between dissolving marriages they consider sacred and binding, or being formally cut off from the institution; many splinter into small, isolated communities bearing social and legal marginalization for maintaining what this reading treats as an obsolete practice.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, excommunicated_plural_families, payer,
    powerless, biographical, trapped, local).

% Applied escheatment, disenfranchisement, and imprisonment pressure that provided the occasion for the revelation, but within this reading's own framing the federal government is not a party to the theological content of the disclosure — its coercive role is acknowledged as circumstance, not as the operative cause, which is precisely the point of contest with the exogenous-override reading.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, biographical, analytical, national).

% Study the Manifesto's textual history, subsequent Second Manifesto (1904), and the persistence of plural marriage in some quarters after 1890, assessing whether the revelatory claim is corroborated independently of the church's own institutional interest in the claim being true.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, church_historians_and_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__endogenous_reinterpretation_reading, church_institution).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the church membership with a single, authoritative resolution to an otherwise fracturing crisis: a single prophetic voice declares the new direction, allowing members to coordinate around continued institutional participation, temple access, and legal normalization without each family independently negotiating a private accommodation with federal authority.
% TRANSFER_FUNCTION: Moves institutional survival capital (property, statehood standing, missionary legitimacy) from the pre-1890 confrontational posture to a stabilized post-1890 institution; moves social and religious standing away from those who maintain the original commandment and toward those who accept the new directive as binding revelation.
% ABSENT_VOICES: Fundamentalist dissenters who came to be excommunicated were, at the moment of the Manifesto's promulgation, largely inside the institution and not yet organized as a distinct voice; their later communities were not consulted in the revelation's formulation and had no institutional channel to contest the reinterpretation once church governance treated the question as settled.
% DISAPPEARANCE_RATIONALE: If this reading's authority collapsed — i.e., if the Manifesto were institutionally reclassified as non-revelatory political expedience rather than genuine prophetic disclosure — the church's claim to continuing revelation as a governing mechanism would be undermined, temple-worthiness standards tied to the post-1890 marriage norm would lose their theological grounding, and the excommunication of twentieth-century fundamentalist groups would lose its doctrinal justification, inviting reconsideration of their status.
% FOUNDING_PROBLEM: The church faced federal disincorporation, mass disenfranchisement of practicing members, imprisonment of church leadership, and seizure of temple property under the Edmunds-Tucker Act; plural marriage practice was making continued institutional existence and temple operation untenable.
% FOUNDING_PROBLEM_CORROBORATION: The federal legal threat that occasioned the Manifesto no longer exists — the Edmunds-Tucker Act's practical force ended with statehood and restored church property decades ago. Independent historians (not church officials) corroborate that the immediate legal crisis is resolved; however, whether the underlying claim was ever a genuine revelation (as opposed to a legitimating narrative for a coerced decision) is attested only from within church leadership and its official historical department — no source outside the institution corroborates the specifically revelatory character of the disclosure, only the historical fact of the legal pressure that preceded it.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate 0.42 because, within this reading's own frame, the arrangement is substantially a genuine coordination solution — the church institution does not extract wealth or labor from the general membership through the Manifesto; the cost falls narrowly on the dissenting minority. Suppression is authored higher (0.55) because maintaining the reinterpretation as authoritative requires ongoing enforcement — excommunication proceedings, doctrinal discipline, and periodic reaffirmation (the 1904 Second Manifesto) against those who continued practicing. Theater ratio starts elevated in 1890 (0.4) reflecting the genuinely contested, high-stakes nature of the initial announcement amid ongoing legal risk, then declines through the mid-20th century as the arrangement stabilizes into settled doctrine, before ticking back up slightly (0.27-0.28) in the late 20th/early 21st century as the church expends institutional effort distinguishing itself publicly from fundamentalist splinter groups that share its scriptural canon.
 *
 * PERSPECTIVAL GAP:
 *   From the church institution's seat, this is coordination around new authoritative guidance that most participants benefit from — a rope. From the fundamentalist dissenters' seat, the same structural mechanism (excommunication for adherence to what they hold as the original unmodified commandment) reads as enforced extraction of institutional standing for refusing a change they consider illegitimate. The engine computes these divergently from the same structural data; this reading does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The church institution sits at the beneficiary end: it authored the directive, administers its enforcement, and its survival directly depends on the reinterpretation being accepted. Mainstream membership, temple-going members, and missionaries are beneficiaries with more diffuse but real gains — continued good standing, restored temple access, reduced social stigma. Fundamentalist dissenters and excommunicated plural families sit at the target end: trapped exit options (leaving means abandoning family, community, and — by their own belief — salvific ordinances validated only within the institution they are being expelled from), bearing the concentrated cost of the reinterpretation's enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal legal existential threat to the institution) is dead by any external measure — the Edmunds-Tucker Act's coercive apparatus dissolved with statehood over a century ago. Yet the doctrinal architecture built to solve that problem (continuing revelation as the mechanism, monogamy as the resulting norm, excommunication as the enforcement tool) persists and is still actively applied against fundamentalist groups today. This is a mismatch worth flagging: founding_problem_status is dead while enforcement (suppression_requirement) remains substantial and even ticks upward late in the series. Under this reading, however, the mismatch is not read as capture, because the reading holds that the revelation vindicated an eternal principle (continuing revelation itself, prophetic authority) rather than merely solving the 1890 crisis — so the persistence of enforcement is read internally as fidelity to ongoing prophetic guidance, not inertia. That internal defense is exactly the structural feature the sibling institutional-pragmatism reading contests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelatory_authenticity_vs_political_expedience,
    'Was the 1890 Manifesto a genuine, independently-verifiable revelatory disclosure, or a retrospectively-theologized description of a decision made primarily under legal and political duress?',
    'Comparison of church leadership''s private correspondence and journals from the period immediately preceding the Manifesto against the public revelatory framing issued afterward; assessment of whether the decision-making process described internally resembles other episodes the tradition itself labels non-revelatory administrative decisions.',
    'If the private record shows the decision framed primarily in legal-strategic terms with the revelatory language added or emphasized only in public promulgation, this reading''s core premise (legitimate prophetic reinterpretation) weakens substantially, and the constraint''s actual operation would be better modeled by the institutional_pragmatism_reading sibling file.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelatory_authenticity_vs_political_expedience, conceptual, 'Whether the revelation claim is independently corroborated or is a legitimating narrative applied after the fact.').

omega_variable(
    beneficiary_structure_and_natural_law_framing,
    'Does treating the 1890 reinterpretation as legitimate continuing revelation (rather than contested doctrinal change) obscure the concentrated institutional benefit the church itself derived from the change?',
    'Trace the specific institutional assets (temple property, statehood eligibility, missionary access) recovered or secured in the years immediately following the Manifesto and compare their value to the cost borne by excommunicated plural families.',
    'A clear asymmetry where the church institution recovers major concrete assets while individual families lose standing and community without compensation would support reading the coordination story as substantially serving concentrated institutional interest even within this reading''s own frame — sharpening rather than resolving the tension between the rope claim and the extraction that lands on dissenters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_and_natural_law_framing, empirical, 'Whether declared institutional beneficiaries capture disproportionate value relative to the coordination benefit distributed to ordinary members.').

omega_variable(
    framing_choice_kernel_vs_legitimacy_layer,
    'Should this constraint be framed around the Manifesto as an institutional event (the obvious framing) or around the legitimacy claim (continuing revelation as a governing doctrine) that the Manifesto is cited as evidence for?',
    'Assess whether removing the specific 1890 episode while retaining the doctrine of continuing revelation would still generate the same beneficiary/victim structure — if so, the legitimacy-claim framing is doing more structural work than the event framing.',
    'If the legitimacy-claim framing is adopted instead, the relevant cs_pattern analysis would center on continuing revelation as an open-ended interpretive license rather than on this specific historical exercise of it, potentially changing the kernel_codification classification from a single fixed event to an ongoing distributed practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_choice_kernel_vs_legitimacy_layer, conceptual, 'Two coherent framings (single historical event vs. ongoing doctrinal license) were available; this story adopts the event framing per the kernel''s stated scope, guided by the manifest''s declared kernel_id being the specific 1890 mandate rather than the general revelation doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 1890, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.35).
narrative_ontology:measurement(plur_tr_t1935, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1935, 0.3).
narrative_ontology:measurement(plur_tr_t1960, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(plur_tr_t1990, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1990, 0.27).
narrative_ontology:measurement(plur_tr_t2020, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 2020, 0.28).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.5).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.48).
narrative_ontology:measurement(plur_be_t1935, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1935, 0.44).
narrative_ontology:measurement(plur_be_t1960, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(plur_be_t1990, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1990, 0.43).
narrative_ontology:measurement(plur_be_t2020, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 2020, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.65).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.7).
narrative_ontology:measurement(plur_su_t1935, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1935, 0.6).
narrative_ontology:measurement(plur_su_t1960, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1960, 0.5).
narrative_ontology:measurement(plur_su_t1990, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(plur_su_t2020, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This is one of three constraint files decomposing the natural-language label 'the 1890 Manifesto.' The exogenous_override_reading denies legitimate doctrinal content to the change (higher ε, treats the church institution's compliance narrative itself as part of the extraction); the institutional_pragmatism_reading treats the revelation claim as a legitimating overlay on a coercion-driven capitulation (highest ε among the three, with the beneficiary set narrowed to institutional leadership specifically rather than the broader membership). This file (endogenous_reinterpretation_reading) authors the lowest ε of the three because, by this reading's own internal logic, most of the membership are genuine beneficiaries of a real coordination solution, and the victim set is narrowly the fundamentalist minority. All three share the same historical kernel (the 1890 Manifesto text and event) but are linked, not merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
