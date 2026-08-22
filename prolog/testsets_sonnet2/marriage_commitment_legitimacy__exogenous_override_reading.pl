% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: 1890 Manifesto as Federal-Coerced Doctrinal Suspension (Exogenous Override Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the exogenous_override_reading of the
 *   marriage_commitment_legitimacy kernel: the 1890 Manifesto ending official
 *   Church sanction of plural marriage is read here as a coerced capitulation
 *   to federal legal and economic siege (Edmunds-Tucker Act asset seizure,
 *   disenfranchisement, imprisonment of leaders), not as a product of genuine
 *   prophetic revelation. Under this reading the underlying theological
 *   doctrine (celestial/plural marriage as an eternal principle, Section 132)
 *   remains formally canonized and unrepudiated; only the practice is
 *   suspended, and it is suspended because continuing it had become
 *   materially impossible for the institution to survive, not because God
 *   commanded a reversal. The extraction runs from the federal government
 *   (which extracts institutional compliance in exchange for restoring legal
 *   personhood, seized property, and the path to statehood) through a
 *   compliant Church hierarchy that absorbs the settlement and passes its
 *   costs downward onto plural families forced to dissolve existing
 *   households and onto the broader membership asked to accept a
 *   coercion-driven policy shift inside a revelation-framed narrative. Rising
 *   theater_ratio over the interval reflects the increasing institutional
 *   need to perform the revelation-framing publicly (the Woodruff testimony
 *   narrative, the 1904 Second Manifesto reaffirmation) as the coercive
 *   origin becomes both more historically documented and more institutionally
 *   inconvenient to acknowledge.
 *
 * KEY AGENTS:
 *   - federal_government: institutional/arbitrage — extracts compliance via legal siege, benefits from ending polygamy as a political problem
 *   - church_hierarchy_post_manifesto: institutional/constrained — issues the Manifesto to preserve corporate survival, absorbs and redistributes coercive pressure
 *   - lds_polygamous_families: powerless/trapped — bear the material and relational cost of forced dissolution
 *   - lds_general_membership: moderate/constrained — bear the cognitive/spiritual cost of the coercion-vs-revelation gap
 *   - fundamentalist_offshoot_communities: powerless/trapped — bear ongoing legal and social cost of rejecting the settlement
 *   - historians_and_legal_scholars: analytical/analytical — reconstruct the record cited by all three kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.81).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.87).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "1890 Manifesto as Federal-Coerced Doctrinal Suspension (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, '836e017c-e28d-4d90-8b99-c820efec80aa').
narrative_ontology:cs_kernel_codification('836e017c-e28d-4d90-8b99-c820efec80aa', formalized).
narrative_ontology:cs_authority_grounding('836e017c-e28d-4d90-8b99-c820efec80aa', lineage).
narrative_ontology:cs_interpretation_layer_present('836e017c-e28d-4d90-8b99-c820efec80aa').
narrative_ontology:cs_reading_relation('836e017c-e28d-4d90-8b99-c820efec80aa', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('836e017c-e28d-4d90-8b99-c820efec80aa', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('836e017c-e28d-4d90-8b99-c820efec80aa', foundational, practice_suspension_under_duress_does_not_alter_doctrine).
narrative_ontology:cs_axiom_status(practice_suspension_under_duress_does_not_alter_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('836e017c-e28d-4d90-8b99-c820efec80aa', practice_suspension_under_duress_does_not_alter_doctrine, empirically_contingent).
narrative_ontology:cs_axiom('836e017c-e28d-4d90-8b99-c820efec80aa', secondary, civil_coercion_cannot_constitute_legitimate_revelatory_cause).
narrative_ontology:cs_axiom_status(civil_coercion_cannot_constitute_legitimate_revelatory_cause, holdable).
narrative_ontology:cs_axiom_grounding('836e017c-e28d-4d90-8b99-c820efec80aa', civil_coercion_cannot_constitute_legitimate_revelatory_cause, deontological).
narrative_ontology:cs_reference_frame('836e017c-e28d-4d90-8b99-c820efec80aa', plural_marriage_as_eternal_saving_principle).
narrative_ontology:cs_drift_state('836e017c-e28d-4d90-8b99-c820efec80aa', post_edmunds_tucker_enforcement, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('836e017c-e28d-4d90-8b99-c820efec80aa', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, church_hierarchy_post_manifesto).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_polygamous_families).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_general_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, fundamentalist_offshoot_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Passed the Edmunds-Tucker Act and disincorporated the Church, seized assets, disenfranchised polygamists, and imprisoned Church leaders until the practice of plural marriage was renounced. Extracts institutional compliance and territorial statehood conditions in exchange for restoring seized property and political rights. Sets the terms under which the Church may operate as a legal entity at all.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter).

% Issues the Manifesto under this reading as capitulation rather than revelation, preserving corporate survival, restored assets, and the path to Utah statehood. Retains theological doctrine on the books (Section 132 remains canonized) while suspending its practice, and subsequently must manage a membership that reads the suspension as abandonment under duress rather than divine command.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, church_hierarchy_post_manifesto, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, church_hierarchy_post_manifesto, agenda_setter).

% Existing plural families are told to cease cohabitation or face excommunication and prosecution, forcing choices between economic ruin (dissolving already-formed households), criminal prosecution, or covert continuation. Many had entered these marriages under prior prophetic sanction and now bear the cost of a reversal they did not choose and cannot appeal, since the same institution that sanctioned the marriages now disavows the practice under external pressure.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_polygamous_families, payer,
    powerless, biographical, trapped, regional).

% Asked to accept that a practice taught as an eternal, saving principle was suspended not by clear revelation but by legal siege, while institutional messaging frames it as prophetic guidance. Bears the cognitive and spiritual cost of reconciling material coercion with a spiritual-authority narrative; exit means leaving the faith community and social network built around it.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_general_membership, payer,
    moderate, biographical, constrained, national).

% Break from the mainstream church specifically because they read the Manifesto as capitulation rather than revelation, continuing plural marriage in defiance of both federal law and institutional excommunication. Bear ongoing legal jeopardy, social isolation, and loss of institutional legitimacy as the direct cost of taking this reading of the Manifesto to its logical conclusion.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, fundamentalist_offshoot_communities, payer,
    powerless, generational, trapped, regional).

% Examine court records, Edmunds-Tucker enforcement data, and Church correspondence from the period to assess whether the Manifesto's language and timing better fit a coerced-capitulation account or a genuine-revelation account. Their reconstructions are cited by all three kernel readings as corroborating evidence.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, historians_and_legal_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, there is no genuine coordination function internal to the constraint itself — the Manifesto coordinates nothing new; it is the price the federal government demanded and the Church paid to end asset seizure and disenfranchisement. The only 'coordination' is the transactional exchange of practice-suspension for legal restoration.
% TRANSFER_FUNCTION: Moves institutional survival, restored corporate assets, and a path to statehood from a compliant Church leadership's concession to the federal government; simultaneously moves the material and spiritual costs of that concession onto plural families and the broader membership who must now live inside a doctrine officially retained but functionally forbidden.
% ABSENT_VOICES: Plural wives and children of dissolved households are almost entirely absent from the documentary record of the negotiation itself — the Manifesto was drafted and issued by an all-male hierarchy in direct correspondence with federal officials and courts, with no formal channel for affected family members to object to the terms of their own family's dissolution.
% DISAPPEARANCE_RATIONALE: If this reading's account of coercion were fully vindicated and acted on institutionally (i.e., if the Church formally repudiated the Manifesto as coerced rather than revealed), the entire doctrinal architecture built on the revelation-framing — from the 1904 Second Manifesto to modern excommunication policy toward polygamous offshoots — would require renegotiation; fundamentalist groups' claim to doctrinal continuity would gain institutional standing they currently lack.
% FOUNDING_PROBLEM: Federal prosecution, asset seizure, and disenfranchisement under the Edmunds-Tucker Act had made continued practice of plural marriage incompatible with the Church's corporate and political survival; the Manifesto existed to end that siege.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the Church (citing federal court records, the Reynolds v. United States line of cases, and Edmunds-Tucker enforcement statistics) attest that the coercive pressure motivating the 1890 announcement was real and has long since ended with statehood and full legal normalization; the Church's own modern teaching materials, by contrast, characterize the Manifesto as revelation rather than settlement, so no corroboration for the 'coercion, not revelation' reading comes from inside the institution that issued it.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (peaking at 0.81 in 1890, the year of the Manifesto itself) because under this reading the entire arrangement is a transactional settlement: the federal government extracts institutional compliance as the price of restoring corporate legal standing, and that extraction is passed down onto families who did not consent to the terms. Suppression is authored even higher (0.90 at 1890) because the mechanism securing compliance was direct legal coercion — imprisonment, disincorporation, disenfranchisement — not persuasion or genuine collective agreement. Theater ratio rises steadily across the interval (0.20 to 0.62) as the institutional narrative increasingly needs to perform 'this was revelation, not surrender' publicly even as the historical record of coercion becomes harder to obscure; the 1904 Second Manifesto marks a local peak in performative reaffirmation. accessibility_collapse is authored moderate (0.40) rather than high because, unlike a mountain, real alternatives existed and were exercised — fundamentalist communities continued the practice underground, showing the suppression did not fully collapse the alternative, only pushed it outside the sanctioned institution. resistance is authored moderate (0.55) reflecting the sustained fundamentalist schism as ongoing active resistance to the settlement.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's seat, this looks like successful law enforcement resolving a settled legal violation — arguably closer to a rope from its own vantage (a coordination problem, bigamy/polygamy enforcement, solved). From the Church hierarchy's seat under this reading, it is coerced survival-mode capitulation dressed as continuity. From the plural families' and fundamentalist communities' seats, it is a tangled rope shading into snare: a coordination story (institutional survival) sits on top of asymmetric extraction they alone pay for. The engine should compute these seats differently from the same structural data — that divergence is exactly the point of authoring this as the exogenous_override_reading rather than resolving it in the prose.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government sits at the beneficiary end: institutional, arbitrage exit, and it collects the compliance it sought without bearing any of the downstream cost. The post-Manifesto Church hierarchy is a secondary beneficiary — it survives as a legal entity and gains a path to statehood — but its exit options are only constrained, not arbitrage, since it remains bound by the settlement terms and cannot renegotiate them unilaterally. Plural families and fundamentalist offshoots sit at the target end: trapped exit, powerless, bearing costs (dissolved households, prosecution, excommunication) they did not choose and could not appeal within the institution that had previously sanctioned their marriages. General membership sits in an intermediate position — moderate power, constrained exit — bearing primarily the cognitive/spiritual cost of a coercion-framed reversal delivered as revelation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (federal legal siege threatening Church survival) as dead — resolved by 1896 statehood and full legal normalization — while the doctrinal suspension persists institutionally to this day, reaffirmed rather than revisited. That status=dead paired with disappearance_verdict=world_rearranges is the mandatrophy signal this reading is built to surface: the coercive condition that justified the suspension no longer exists, yet the suspension (and its doctrinal architecture) remains load-bearing for the institution's modern identity and legal standing, which is precisely why revisiting the coercion-vs-revelation question still rearranges present-day institutional and fundamentalist-schism arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_revelation_historical_fact,
    'Was the 1890 Manifesto issued because Wilford Woodruff received genuine divine revelation commanding the change, or because the federal legal and economic siege left the Church no institutionally survivable alternative — with the revelation-language functioning as post-hoc theological framing of a forced decision?',
    'Comparative analysis of the timing and content of Woodruff''s private journal entries against the escalation timeline of Edmunds-Tucker enforcement; examination of whether the Manifesto''s language and the subsequent Second Manifesto (1904) more closely track legal necessity or independent theological development. This is likely irresolvable to consensus given the evidentiary limits of introspective revelatory claims, but the correlation between enforcement escalation and the timing of the announcement is itself strong circumstantial evidence for this reading.',
    'If the coercion account is correct, the constraint is best modeled as extraction (federal government compelling institutional compliance, costs passed to families) rather than as coordination toward a genuinely embraced doctrinal shift — supporting the tangled_rope/snare-leaning classification authored here. If the revelation account is correct, the constraint would instead resemble a scaffold or rope from the institution''s own framework, which is the sibling endogenous_reinterpretation_reading''s structural claim, not this one''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_revelation_historical_fact, conceptual, 'Whether the Manifesto''s causal origin is best characterized as coercion or as genuine revelation — the central contested fact this kernel exists to represent as separate readings rather than resolve.').

omega_variable(
    doctrine_retained_vs_repudiated,
    'Does the Church''s continued canonization of Doctrine and Covenants Section 132 (celestial/plural marriage as eternal principle) after 1890 confirm that only practice, not doctrine, was suspended — or has subsequent institutional teaching effectively repudiated the doctrine itself, making the ''doctrine unchanged'' premise of this reading historically outdated?',
    'Textual analysis of official Church teaching materials, general conference addresses, and temple ceremony language across the twentieth and twenty-first centuries to trace whether plural marriage as celestial doctrine has been actively reinterpreted, quietly abandoned, or genuinely preserved unchanged pending a future practice.',
    'If doctrine has been effectively repudiated over time, this reading''s premise (theological doctrine unchanged, only practice suspended) becomes increasingly counterfactual for later periods, and the constraint''s classification should drift toward reflecting doctrinal change rather than pure practice-suspension as the interval extends past 1904.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_retained_vs_repudiated, empirical, 'Whether formal doctrinal retention has persisted or eroded in the century since the Manifesto, which bears on how long this reading''s core premise remains descriptively accurate.').

omega_variable(
    beneficiary_status_of_church_hierarchy,
    'Is the post-Manifesto Church hierarchy better modeled as a coerced party absorbing extraction on behalf of its membership, or as a co-beneficiary that gained institutional survival, wealth, and legitimacy from the settlement and therefore shares directionality closer to the federal government than to the membership it represents?',
    'Comparison of institutional asset trajectories, membership growth, and political standing pre- and post-1890 against counterfactual trajectories under continued federal prosecution.',
    'If the hierarchy is better modeled as a co-beneficiary, its directionality (currently d near the beneficiary end but with constrained exit) should be understood as reflecting a genuine institutional interest in the settlement, not merely a passthrough absorbing federal pressure — sharpening rather than softening the tangled_rope reading, since a real beneficiary alongside a real victim class is exactly what the tangled_rope gate requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_status_of_church_hierarchy, conceptual, 'Whether the Church hierarchy''s post-Manifesto position is coerced-intermediary or genuine co-beneficiary — bears on directionality assignment, not on the coercion-vs-revelation question itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 1885, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1885, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1885, 0.2).
narrative_ontology:measurement(marr_tr_t1887, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1887, 0.3).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1890, 0.45).
narrative_ontology:measurement(marr_tr_t1893, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1893, 0.52).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1896, 0.58).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1900, 0.6).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1904, 0.62).

% Extraction over time
narrative_ontology:measurement(marr_be_t1885, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1885, 0.55).
narrative_ontology:measurement(marr_be_t1887, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1887, 0.68).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1890, 0.81).
narrative_ontology:measurement(marr_be_t1893, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1893, 0.79).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1896, 0.74).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1900, 0.7).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1904, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1885, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1885, 0.75).
narrative_ontology:measurement(marr_su_t1887, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1887, 0.85).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1890, 0.9).
narrative_ontology:measurement(marr_su_t1893, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1893, 0.88).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1896, 0.82).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1904, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the marriage_commitment_legitimacy kernel (the 1890 Manifesto). endogenous_reinterpretation_reading treats the Manifesto as genuine revelation (low ε, coordination-dominant, likely rope/scaffold-leaning); hybrid_pragmatic_reading treats it as strategic adaptation preserving core commitments via scope ambiguity (moderate ε, mixed coordination/extraction); this story (exogenous_override_reading) treats it as coerced capitulation with doctrine unrepudiated (high ε, extraction-dominant, tangled_rope/snare-leaning). Each reading authors its own stable ε rather than averaging across the contest; they are linked here via affects_constraints because each reading's institutional legitimacy claims create downstream pressure on how the others are received by different constituencies (fundamentalist schismatics draw legitimacy specifically from this reading; mainstream Church teaching draws legitimacy from the endogenous reading; historians often gravitate toward the hybrid reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
