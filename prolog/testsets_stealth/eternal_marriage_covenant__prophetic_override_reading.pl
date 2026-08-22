% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Prophetic Override: Continuing Revelation Supersedes the Plural-Marriage Mandate (1890 Reading)
 *   domain: religious law/political theology/commitment system dynamics
 *
 * SUMMARY:
 *   This story instantiates the prophetic_override_reading of the
 *   eternal_marriage_covenant kernel: the arrangement by which the living
 *   prophet's new revelation supersedes a prior commandment when
 *   circumstances require, as exercised in the 1890 Manifesto under federal
 *   destruction pressure. The epsilon referent is that standing override
 *   arrangement as this reading sees it — not the
 *   temporal_accommodation_reading's law-obedience frame (where nothing is
 *   superseded) and not the immutable_commandment_reading's unrescindable
 *   mandate (where the override is apostasy); those are sibling constraints
 *   in the same family, linked via network.affects_constraints. Claim/metric
 *   independence: the constraint is CLAIMED as tangled_rope — a genuine
 *   survival-coordination function plus concentrated costs borne through the
 *   same structure, actively enforced — while the metrics are authored
 *   descriptively from the historical operation. The engine computes per-seat
 *   classifications from the structural data; where a seat's computed type
 *   diverges from this claim, that divergence is the measurement the corpus
 *   exists to take.
 *
 * KEY AGENTS:
 *   - church_leadership: agenda-setter and primary beneficiary (institutional/identity_locked) — administers the override, collects the survival dividend and authority centralization
 *   - general_membership: net beneficiary bearing real costs (organized/constrained) — gains legal peace, pays in doctrinal reversal
 *   - plural_families: primary target (moderate/trapped) — bear the concentrated costs of the reversal
 *   - fundamentalist_schismatics: secondary target and excluded voice (powerless/trapped) — expelled for holding the prior mandate
 *   - federal_authorities: external counterparty (institutional/mobile) — the pressure that activates the mechanism; collects compliance
 *   - historians_of_the_period: analytical observer (analytical/analytical) — sees the full structure across the pressure timeline, covert window, and enforcement record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.68).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.7).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Prophetic Override: Continuing Revelation Supersedes the Plural-Marriage Mandate (1890 Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious law/political theology/commitment system dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, 'c115afc9-8f63-4903-9675-a4390c5ca8cd').
narrative_ontology:cs_kernel_codification('c115afc9-8f63-4903-9675-a4390c5ca8cd', fixed_text).
narrative_ontology:cs_authority_grounding('c115afc9-8f63-4903-9675-a4390c5ca8cd', lineage).
narrative_ontology:cs_interpretation_layer_present('c115afc9-8f63-4903-9675-a4390c5ca8cd').
narrative_ontology:cs_reading_relation('c115afc9-8f63-4903-9675-a4390c5ca8cd', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('c115afc9-8f63-4903-9675-a4390c5ca8cd', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('c115afc9-8f63-4903-9675-a4390c5ca8cd', foundational, living_prophet_supersedes_prior_revelation).
narrative_ontology:cs_axiom_status(living_prophet_supersedes_prior_revelation, holdable).
narrative_ontology:cs_axiom_grounding('c115afc9-8f63-4903-9675-a4390c5ca8cd', living_prophet_supersedes_prior_revelation, theological).
narrative_ontology:cs_axiom('c115afc9-8f63-4903-9675-a4390c5ca8cd', secondary, institutional_continuity_outriggers_practice_mandate).
narrative_ontology:cs_axiom_status(institutional_continuity_outriggers_practice_mandate, holdable).
narrative_ontology:cs_axiom_grounding('c115afc9-8f63-4903-9675-a4390c5ca8cd', institutional_continuity_outriggers_practice_mandate, instrumental).
narrative_ontology:cs_reference_frame('c115afc9-8f63-4903-9675-a4390c5ca8cd', living_oracle_open_canon).
narrative_ontology:cs_drift_state('c115afc9-8f63-4903-9675-a4390c5ca8cd', post_second_manifesto_settlement, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('c115afc9-8f63-4903-9675-a4390c5ca8cd', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, general_membership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, plural_families).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_schismatics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, federal_authorities).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, general_membership).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__prophetic_override_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__prophetic_override_reading, prophetic_authority_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The First Presidency and Quorum of the Twelve announce, administer, and enforce the override. The 1890 declaration issued over the president's signature; the quorums sustained it; stake disciplinary councils carried it out. They collect the survival dividend — institutional continuity, the property settlement, the path to statehood — and each exercise of the mechanism re-centers final doctrinal authority in the living presidency. Leaving the mechanism would mean renouncing the living-oracle authority that constitutes their office; the presidency cannot repudiate continuing revelation without dissolving the ground of its own claim to lead.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_leadership, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, church_leadership, beneficiary).

% The rank and file gain the end of raids, arrests, and disenfranchisement; the church's legal standing and Utah's statehood restore ordinary civic life. They pay in doctrinal reversal: a principle taught as required for exaltation is rescinded by the same authority that commanded it, and testimony must be reorganized around the about-face. Exit means leaving the community, the temple covenants, and the social world — possible, and some did it, but at the cost of everything their religious life was built on.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, general_membership, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, general_membership, payer).

% Existing plural wives and their children bear the reversal's concentrated costs. New plural marriages are barred, foreclosing the marriage prospects the doctrine had promised their children; husbands come under pressure to designate one household; some families split by colonization to Mexico and Canada to keep the practice; and the stigma of the life they lived becomes the thing the church publicly renounces. Leaving the church abandons the covenants their marriages were formed inside; staying means living as historical exceptions in a community that has moved on. From where they stand there is no clean way out.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, plural_families, payer,
    moderate, biographical, trapped, regional).

% Members who hold the pre-1890 commandment as eternally binding refuse the reversal. They face temple recommend withdrawal, then excommunication, systematic from the mid-1900s onward. They form separate communities and carry the practice forward at legal and social peril. Both directions are closed: accepting the reversal means abandoning what they hold to be eternal law; refusing means losing membership, family, and community. Their communities become the visible demonstration of what refusal costs.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_schismatics, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_schismatics, excluded).

% Congress, the federal courts, and territorial administration drove the collision: the Edmunds-Tucker Act disincorporated the church, moved to seize its property, and stripped polygamists of the vote. They are not participants in the church's doctrinal framework — their demands cannot be answered inside it, which is what makes the override the only available move. They collect the compliance they demanded without operating any part of the mechanism, and they can escalate or relent independently of anything the church does internally.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_authorities, excluded,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, federal_authorities, beneficiary).

% Documentary scholars working from church archives, the Smoot hearings record, and fundamentalist counter-archives see the full structure: the pressure timeline from the Morrill Act through Edmunds-Tucker, the gap between public declaration and covert continuation from 1890 to 1904, and the enforcement record after the Second Manifesto. They collect nothing and bear nothing; theirs is the view the participants could not have.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, historians_of_the_period, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__prophetic_override_reading, church_leadership).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__prophetic_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative channel for revising settled commandment when it collides with external enforcement: one prophet speaks, the body sustains, and the institution pivots coherently instead of fragmenting into private decisions about which revelation binds. It resolved the church-state crisis of 1890 while preserving a single chain of authority and the community's continuity.
% TRANSFER_FUNCTION: Moves the cost of the pivot onto the plural families (their marriages repositioned as historical exceptions) and the principled dissenters (their standing revoked); moves doctrinal authority upward, since each exercise re-centers final interpretive authority in the living presidency; moves legal normality to the general body.
% ABSENT_VOICES: The plural wives themselves — the people whose marriages the declaration repositioned — had no seat in the decision; it was announced to them. The holders of the immutable reading, including men who had gone to prison and families in exile, were heard only as discipline cases, never as parties. The general membership sustained the declaration in conference after it was decided — ratification, not deliberation. And the dead revelators whose revelations were superseded cannot object: the override's authority rests on voices that cannot answer back.
% DISAPPEARANCE_RATIONALE: If the override mechanism vanished overnight — if no new revelation could supersede prior revelation — the 1890 pivot could not have been authored within the framework at all. The church would have faced schism between law-keepers and commandment-keepers, or continued illegal practice into institutional destruction. The modern church's monogamous norm, its legal standing, its centralized authority, and the 1978 priesthood extension all depend on this mechanism having been available.
% FOUNDING_PROBLEM: How can a community whose authority rests on an open canon — revelation that can continue — handle the collision between a prior commandment and changed circumstances without either freezing revelation (losing the authority claim) or dissolving into private interpretation? Concretely in 1890: the church faced disincorporation, property seizure, imprisonment of its members, and disenfranchisement for practicing what a prior revelation commanded.
% FOUNDING_PROBLEM_CORROBORATION: Historians outside the benefiting parties attest the survival-pressure context: the federal enforcement timeline, the documented gap between Woodruff's public theophany framing and private statements describing the declaration as the only way to save the church, and the covert-continuation window exposed in the Smoot hearings record. The fundamentalist schism's very existence is standing counter-testimony that contemporaries understood the override as survival-driven rather than doctrinally necessary. No benefiting party's self-attestation is relied on.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high but bounded: the reversal's costs concentrate on plural families and on dissenters expelled for holding the prior mandate, while the mechanism simultaneously performed a real survival function for the institution and the general body — the signature of a structure that coordinates and extracts through the same machinery. Suppression (0.70) reflects the enforcement machinery the reversal required: recommend interviews, stake discipline, excommunication of post-Manifesto practitioners after 1904; suppression is authored as the raw structural force and is not scaled by power or scope in this story. Theater (0.30) captures the framing gap: the declaration was presented as unprompted revelation while private records describe it as the only way to save the church, and public compliance outran practice during the 1890-1904 covert-continuation window; the annual conference re-reading maintains a performance floor after enforcement made compliance real. Accessibility collapse (0.60): within the institutional framework, alternatives to accepting the reversal collapse to schism-at-high-cost; the schisms that did occur show alternatives were never fully closed. Resistance (0.65): the Smoot hearings exposed the covert window, plural households resisted consolidation pressure for a decade, and the fundamentalist schism is standing resistance. The measurement series run on one shared grid (T=0,10,14,20,30,40 mapping 1890-1930) so every metric is authored at every point. The suppression series is authored because enforcement build-up is the dynamic this story traces: machinery that starts light (the announcement's own authority), hardens through the Second Manifesto and the expulsions, and plateaus once the dissenters are outside the boundary.
 *
 * PERSPECTIVAL GAP:
 *   The leadership seat and the payer seats should compute differently. From the presidency, the mechanism is the coordination structure that saved the church: one authoritative pivot instead of fragmentation, legal normality restored, the community continuous. From the plural-family seat the same structure is abandonment: the marriages they sacrificed for repositioned as exceptions, their children's promised prospects foreclosed. From the schismatic seat it is expulsion machinery: the mechanism's boundary work made visible in their excommunication. The general membership sits between — genuine relief from raids and imprisonment, purchased with a doctrinal about-face their testimony must absorb. The engine computes these per-seat classifications from power, exit, and role data; the divergence between the leadership's coordination experience and the payers' abandonment experience is the measurement this story exists to take. Note also the coalition that did not form: plural families wanted accommodation while dissenters wanted the mandate — their interests diverged, so the two least-powerful seats never combined their leverage.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: church_leadership (collects the survival dividend and authority centralization) and general_membership (legal peace, institutional continuity) derive low d. Victim declarations: plural_families (bear the concentrated costs, trapped) and fundamentalist_schismatics (expelled, trapped) derive high d. One override: general_membership is listed among beneficiaries, so the derivation would place it near the beneficiary end (~0.15), but structurally the seat is near-symmetric — it gains survival and legal normality while absorbing a doctrinal reversal that strains testimony and reorganizes family expectations — so an override sets the organized seat's d to 0.40. church_leadership needs no override: its beneficiary declaration already places it at the subsidy end, which is where it belongs. federal_authorities holds no structural declaration and takes the canonical fallback for its power atom, which is defensible — as the external counterparty it is neither subsidized by nor a target of the mechanism itself.
 *
 * MANDATROPHY ANALYSIS:
 *   Two founding problems must be kept distinct. The 1890 exercise's founding problem — institutional survival under federal destruction — is dead: statehood in 1896 and the Smoot-era settlement closed it. The mechanism's founding problem — how an open-canon authority revises settled commandment when circumstances collide with it — is live, and the 1978 exercise under a different pressure demonstrates recurrence. This story therefore authors founding_problem_status: live at the mechanism level; the mismatch consumer should find no zombie flag (live status, world_rearranges verdict). The classification prevents mislabeling in both directions: calling the mechanism a pure rope would erase the plural families whose sacrifices were nullified and the dissenters expelled; calling it a snare would erase the genuine survival function that made the pivot necessary at all. The tangled_rope claim holds both facts in one structure: the same machinery that coordinated the pivot concentrated its costs on those with the least exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'What would each sibling reading of the eternal_marriage_covenant kernel change structurally if adopted in place of this prophetic_override_reading?',
    'Comparative doctrinal analysis across the three readings'' own texts and the schisms'' counter-archives: locate the disagreement in the status each reading assigns the 1890 declaration — superseding revelation (this reading), suspension-without-renunciation (temporal_accommodation_reading), illegitimate capitulation (immutable_commandment_reading).',
    'Under the immutable reading, this arrangement is illegitimate and the operative constraint becomes an unrescindable mandate binding members to an illegal practice — a far harsher structure with the members themselves as targets. Under the accommodation reading, nothing is superseded: the operative constraint is law-obedience, the doctrinal-reversal costs disappear, and this override mechanism''s extractiveness drops toward the coordination floor. Each resolution produces a different constraint with a different victim set; the disagreement''s location is the declaration''s status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'How sibling kernel readings would restructure this constraint''s beneficiaries, victims, and type.').

omega_variable(
    override_revelation_genuineness,
    'Was the 1890 exercise a genuine revelatory event, or political necessity framed as revelation — and does the answer change what this constraint is?',
    'The theophany''s content is private and unresolvable in principle; partially resolvable through the exercise pattern (both canonical exercises, 1890 and 1978, followed acute external pressure) and the documented gap between the president''s public theophany framing and private statements describing the declaration as the only way to save the church.',
    'If framing-dominant, the mechanism functions as authority-laundering of political necessity — the structure shifts toward pure enforcement of leadership decision with revelation language as cover. If revelation-dominant, the coordination function is what this reading claims and the concentrated costs are the price of an authoritative pivot. The character of the leadership seat''s gains turns on this question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(override_revelation_genuineness, empirical, 'Genuineness of the revelatory framing of the override''s founding exercise.').

omega_variable(
    pressure_precondition_on_exercise,
    'Is the override exercisable only under survival-level external pressure, or is it a standing governance authority available for proactive doctrinal revision?',
    'Counterfactual analysis of near-exercises: doctrinal questions the institution declined to resolve by override absent pressure, set against the documented pressure contexts of both canonical exercises.',
    'If pressure-preconditioned, the mechanism is a crisis valve rather than a standing norm: its exercises are transitional events rather than routine governance, the standing-authority claim weakens, and the constraint''s character between exercises approaches dormancy. If available proactively, it is a live governance instrument and the reference frame''s implied authority is intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pressure_precondition_on_exercise, empirical, 'Whether exercise of the override requires survival pressure or is available as routine authority.').

omega_variable(
    internalized_authority_suppression,
    'Is member compliance with the doctrinal reversal structural (disciplinary machinery, recommend pressure, social cost) or internalized (testimony identity makes questioning the override equivalent to unraveling prophetic authority as such)?',
    'Post-exit trajectory of those who left over the reversal: if identity conflict persists after the disciplinary machinery no longer applies to them, the internalized component is substantial.',
    'If internalized, effective suppression exceeds the structural measure and travels with members across the schism boundary; the identity lock on the leadership seat (the office constituted by the authority it exercises) has a mirror in the membership seat, and the schism''s persistence across generations is partly explained by it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_authority_suppression, empirical, 'Structural versus internalized suppression of dissent from the doctrinal reversal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(eter_tr_t0, observed).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(eter_tr_t10, observed).
narrative_ontology:measurement(eter_tr_t14, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 14, 0.34).
narrative_ontology:measurement_basis(eter_tr_t14, observed).
narrative_ontology:measurement(eter_tr_t20, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(eter_tr_t20, observed).
narrative_ontology:measurement(eter_tr_t30, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(eter_tr_t30, observed).
narrative_ontology:measurement(eter_tr_t40, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(eter_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(eter_be_t0, observed).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(eter_be_t10, observed).
narrative_ontology:measurement(eter_be_t14, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 14, 0.64).
narrative_ontology:measurement_basis(eter_be_t14, observed).
narrative_ontology:measurement(eter_be_t20, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(eter_be_t20, observed).
narrative_ontology:measurement(eter_be_t30, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(eter_be_t30, observed).
narrative_ontology:measurement(eter_be_t40, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(eter_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(eter_su_t0, observed).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(eter_su_t10, observed).
narrative_ontology:measurement(eter_su_t14, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 14, 0.65).
narrative_ontology:measurement_basis(eter_su_t14, observed).
narrative_ontology:measurement(eter_su_t20, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(eter_su_t20, observed).
narrative_ontology:measurement(eter_su_t30, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(eter_su_t30, observed).
narrative_ontology:measurement(eter_su_t40, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(eter_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the eternal_marriage_covenant kernel decomposes into three readings because the natural-language label 'the Manifesto's relationship to D&C 132' covers three structurally distinct claims with different epsilon. The immutable reading (upstream in doctrinal time — the pre-1890 settled text) authors the mandate as unrescindable; this override reading authors the 1890 exercise as valid supersession (epsilon ~0.68: concentrated costs on plural families and dissenters inside a real survival function); the temporal_accommodation reading authors a suspension with no doctrinal reversal (the lowest epsilon of the three). The upstream immutable claim is what the override supersedes and what the accommodation preserves — each sibling is a different constraint with a different victim set, not one constraint viewed from different angles. Within this reading's framework the immutable reading is foreclosed (the override's validity negates the mandate's immutability), while the accommodation reading coexists with this one in post-Manifesto discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__prophetic_override_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
