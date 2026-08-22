% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Magisterial Authority: Composite Overdetermination Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962-1965) convened to modernize the Catholic Church's
 *   engagement with the contemporary world. This reading holds that the
 *   Council achieved its political supermajority by encoding BOTH continuity
 *   and rupture ecclesiology into ambiguous compromise formulations. Texts
 *   simultaneously authorize conservative and progressive interpretations; no
 *   single reading can claim the texts as univocal support. The constraint
 *   operates through hermeneutical authority: whoever controls the
 *   interpretation of what the Council 'really meant' controls the
 *   magisterium. The 10-12% conciliar minority votes (bishops voting against
 *   key decrees) signal that theological incompatibility was embedded in
 *   final texts, not resolved by them. Implementation divergence across
 *   dioceses is not accidental drift but structural consequence of
 *   overdetermined texts.
 *
 * KEY AGENTS:
 *   - Episcopal progressives: shaped conciliar agendas and committee language; benefit from readings authorizing change
 *   - Institutional continuity advocates: preserved formal doctrinal non-rupture language; benefit from readings that frame Council as development not revolution
 *   - Theological traditionalists: locked into identity identification with pre-conciliar doctrine; experience permanent hermeneutical limbo as texts authorize both preservation and erosion of their positions
 *   - Pre-conciliar doctrine defenders: institutional movements built on explicit pre-conciliar teaching; face cost of texts that neither endorse nor condemn their theological ground
 *   - Episcopal conservatives: voted against Council but bound by its authority; excluded from post-conciliar hermeneutical conversation despite representing the theological objections texts encode but do not resolve
 *   - Post-conciliar implementation bodies: hold de facto hermeneutical authority through regulatory interpretation of ambiguous texts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.68).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Magisterial Authority: Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, '8c166018-4d47-471d-9e6a-bae9f2c2ba5b').
narrative_ontology:cs_kernel_codification('8c166018-4d47-471d-9e6a-bae9f2c2ba5b', fixed_text).
narrative_ontology:cs_authority_grounding('8c166018-4d47-471d-9e6a-bae9f2c2ba5b', lineage).
narrative_ontology:cs_interpretation_layer_present('8c166018-4d47-471d-9e6a-bae9f2c2ba5b').
narrative_ontology:cs_reading_relation('8c166018-4d47-471d-9e6a-bae9f2c2ba5b', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c166018-4d47-471d-9e6a-bae9f2c2ba5b', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('8c166018-4d47-471d-9e6a-bae9f2c2ba5b', foundational, conciliar_texts_intentionally_overdetermined).
narrative_ontology:cs_axiom_status(conciliar_texts_intentionally_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('8c166018-4d47-471d-9e6a-bae9f2c2ba5b', conciliar_texts_intentionally_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('8c166018-4d47-471d-9e6a-bae9f2c2ba5b', foundational, hermeneutical_control_constitutes_magisterial_authority).
narrative_ontology:cs_axiom_status(hermeneutical_control_constitutes_magisterial_authority, holdable).
narrative_ontology:cs_axiom_grounding('8c166018-4d47-471d-9e6a-bae9f2c2ba5b', hermeneutical_control_constitutes_magisterial_authority, deontological).
narrative_ontology:cs_reference_frame('8c166018-4d47-471d-9e6a-bae9f2c2ba5b', unbroken_papal_magisterial_authority_through_textual_interpretation).
narrative_ontology:cs_drift_state('8c166018-4d47-471d-9e6a-bae9f2c2ba5b', contemporary_post_conciliar_implementation_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c166018-4d47-471d-9e6a-bae9f2c2ba5b', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, episcopal_progressives).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, institutional_continuity_advocates).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, theological_traditionalists).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, pre_conciliar_doctrine_defenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, lay_faithful).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, lay_faithful).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, magisterial_authority_resides_in_episcopal_college).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__composite_overdetermination_reading, pastoral_aggiornamento_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops seeking modernization of Church governance and teaching, who shaped Council texts through committee work and voting blocs. Benefit from the ambiguous formulations that legitimate progressive interpretations of conciliar intent while preventing explicit rupture language that would trigger schism. Read the texts as opening space for continuous reinterpretation; control hermeneutical authority through post-conciliar commissions and magisterial precedent-setting.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, episcopal_progressives, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, episcopal_progressives, beneficiary).

% Cardinals and theologians committed to preserving formal doctrine continuity and papal supremacy. Benefit from the texts' dual encoding: their provisions can be read as non-rupturing organic development, protecting institutional authority from the appearance of doctrinal reversal while accommodating pastoral reform. Their interpretive authority rests on controlling the reading of 'development' vs. 'rupture' language.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, institutional_continuity_advocates, beneficiary,
    institutional, generational, constrained, global).

% Priests, theologians, and lay faithful committed to pre-conciliar doctrinal positions (Latin liturgy, scholastic theology, strict papal centralism, defined natural-law sexual ethics). Bear the cost of conflicting magisterial signals: conciliar texts simultaneously validate their theological commitments (via continuity language) and authorize their erosion (via pastoral opening language). Exit would require severing professional/spiritual identity; many remain institutionally while experiencing internal doctrinal alienation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, theological_traditionalists, payer,
    moderate, biographical, identity_locked, global).

% Movements and institutional structures built on pre-conciliar ecclesiology (traditionalist seminaries, Latin Mass communities, Thomistic academies). Face the cost of structural ambiguity: their theological ground is neither explicitly upheld nor explicitly condemned, leaving them in permanent interpretive limbo. Options are limited to negotiating with bishops over implementation or accepting institutional marginalization; leaving the Church entirely forfeits the religious identity that constitutes their mission.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, pre_conciliar_doctrine_defenders, payer,
    moderate, generational, trapped, global).

% Bishops who voted against key conciliar texts (10-12% of the Council on major votes) but remained bound by conciliar authority. Their objections were recorded but overridden; they are structurally excluded from the subsequent hermeneutical conversation because conciliar majority legitimacy forecloses their dissent as formally refuted. They cannot openly teach against Council texts without schismatic appearance, even when texts contain language they explicitly rejected.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, episcopal_conservatives, excluded,
    institutional, generational, constrained, global).

% Papal commissions, liturgical committees, and episcopal conferences tasked with implementing conciliar decrees. Their interpretive choices became the binding application of texts that encoded both continuity and rupture readings. They hold de facto hermeneutical authority over what the Council 'really meant' through regulatory choice; their decisions shape how the ambiguous texts become concrete discipline.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, post_conciliar_implementation_bodies, agenda_setter,
    institutional, biographical, constrained, global).

% Ordinary believers experiencing conflicting catechesis, liturgical discontinuity, and moral teaching instability as different parishes and dioceses implement the Council differently. Some benefit from reformed liturgy and opening to modern life; others experience loss of continuity and doctrinal clarity. No exit exists without severing religious belonging; navigating the inconsistency becomes part of lived faith.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, lay_faithful, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, lay_faithful, beneficiary).

% Eastern Orthodox and Protestant theologians observing from outside the Catholic communion. Analyze the conciliar texts and their aftermath as evidence about whether Vatican II represents continuity or rupture, using the interpretation divergence as data about the Council's true hermeneutical intent. Their testimony is external to the Catholic institutional frame but carries weight in ecumenical dialogue and historical assessment.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, orthodox_christian_critics, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Council coordinated a supermajority episcopal consensus on aggiornamento by drafting texts that could be read as either organic development (continuity reading) or fundamental reorientation (rupture reading), thereby satisfying both progressive and conservative bishops and preventing immediate schism. The coordination problem solved: how to authorize substantive pastoral and theological change without triggering the appearance of doctrinal reversal that would delegitimize papal and conciliar authority.
% TRANSFER_FUNCTION: Transfers hermeneutical authority from explicitly defined doctrine to interpretive bodies (post-conciliar commissions, bishops implementing texts, theologians reading them). Moves the locus of magisterial control from doctrinal substance to the power to declare what texts 'really mean.' Extracts from traditionalist stake-holders (who must accept non-rupture language that authorizes their erosion) to progressive beneficiaries (who gain authorization for change while maintaining institutional legitimacy).
% ABSENT_VOICES: Bishops who voted against the Council (10-12% minority on major votes) are formally excluded: their dissent was overridden by majority magisterial authority, and the texts themselves do not acknowledge their theological objections. Traditionalist movements that would argue for explicit pre-conciliar doctrine endorsement have no seat at implementation — their concerns are absorbed into the ambiguous language rather than addressed. Lay testimony about lived experience of implementation divergence does not shape hermeneutical authority.
% DISAPPEARANCE_RATIONALE: If the conciliar constraint vanished — if Vatican II's texts were declared null and the Church reverted to pre-conciliar magisterial form — the entire post-conciliar institutional and pastoral structure would require reorganization. Liturgy would revert, episcopal authority structures would centralize, theological schools would realign, ecumenical relationships would sever. The alternative world (pre-conciliar restoration) is structurally imaginable but would require undoing fifty years of institutional embedding.
% FOUNDING_PROBLEM: The Council was convoked to address perceived institutional rigidity and increasing distance between Catholic teaching and contemporary intellectual and pastoral reality. The founding problem: the Church's magisterium appeared isolated from modern science, philosophy, and social conditions; the faithful experienced increasing alienation from preconciliar discipline and doctrine; ecumenical dialogue was blocked by pre-conciliar positions presented as unchangeable.
% FOUNDING_PROBLEM_CORROBORATION: Pope John XXIII and the progressive bishops who shaped the agenda attest the founding problem was institutional rigidity. Conservative bishops attest the founding problem was theological confusion and loss of doctrinal clarity. Post-conciliar decades show mixed evidence: alienation of many traditionalists (problem not solved) alongside genuine reconnection of others to institutional belonging (problem partly solved). No corroboration from outside the institutional parties exists that distinguishes whether modernization of the Church's self-understanding was necessary or whether pre-conciliar teaching could have evolved incrementally without conciliar rupture. The disagreement over whether the founding problem was real or manufactured is itself part of the constraint's operation.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over the interval as the consequences of hermeneutical ambiguity compound: early period (T0-T12) shows low extractiveness because the ambiguous encoding still feels like genuine coordination — different readings coexist peacefully as bishops implement provisionally. Mid-period (T12-T36) extractiveness rises as bishops and theologians compete openly for interpretive authority; traditionalists experience the cost of their doctrinal ground being simultaneously affirmed and eroded by the same texts. Late period (T36-T60) extractiveness stabilizes as the hermeneutical hierarchy solidifies: progressive and continuity readings dominate papal teaching and theological publishing; traditionalist readings are marginalized as 'misinterpretation,' suppressing their dissent through institutional authority rather than doctrinal refutation. Theater ratio rises from 0.25 to 0.58 as the performative work increases: the Council must continuously be reaffirmed as authoritative while its actual meaning remains contested; enforcement machinery (papal statements, curial directives, doctrinal commissions) works to suppress the appearance of rupture even as progressive implementation occurs. Suppression requirement rises as traditionalist resistance hardens and must be more actively managed through institutional discipline. The time grid is shared across all three metrics; every metric is authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   Progressive bishops experience the constraint as genuine coordination: it enabled the Council to function, prevented schism, and authorized aggiornamento without formal doctrinal reversal. They hold hermeneutical authority to declare what the texts mean. Traditionalists experience the same constraint as enforced extraction: the texts' language affirms their positions while the institutional interpretation suppresses them. They are coordinated into a structure they did not choose and cannot exit without severing identity. Conservative bishops occupy a painful middle: they were part of the Council's working majority but voted against key texts; they cannot teach against the Council's authority without schismatic appearance, yet the Council does not acknowledge their theological objections — they are coordinated out of the conversation. Lay faithful experience implementation divergence as lack of catechetical clarity: different parishes teach different doctrine, practice different liturgies, apply different moral norms; the ambiguous texts provide no grounds to declare which interpretation is correct. These divergent experientail positions are all structurally produced by the same overdetermined texts. The engine computes different types for each seat from the same constraint because the directionality differs: progressive beneficiaries may experience Rope (coordination without extraction), while traditionalist targets experience Snare (extraction disguised as coordination).
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive bishops hold directionality near 0.0 (full beneficiary: they authored the ambiguous language intentionally, control post-conciliar hermeneutics, their interpretations dominate papal teaching). Institutional continuity advocates hold directionality near 0.2 (beneficiary: they prevented explicit rupture language and control the 'development not revolution' narrative, though they depend on progressives' cooperation). Traditionalists hold directionality near 0.8 (target: their pre-conciliar positions are neither refuted doctrinally nor affirmed structurally; they are suppressed through hermeneutical authority that declares their readings 'misinterpretation'). Episcopal conservatives hold directionality near 0.85 (target: they voted no and lost; their theological objections are encoded in the texts but overridden by majority authority, excluding them from legitimate hermeneutical conversation). Post-conciliar bodies hold directionality near 0.4 (moderate payer: they must implement texts while managing the interpretive conflict; they have power but are constrained by the texts' ambiguity — they cannot resolve incompatibility because the texts intentionally resist resolution). This directionality map explains why the constraint computes as Tangled Rope from every seat: progressives benefit from coordination (shared authority structure) while extracting hermeneutical dominance; traditionalists are coordinated into a structure (remain in one communion) while paying the cost of doctrinal suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional rigidity and doctrinal distance from modernity) is contested in status: progressives attest it was real and pressing; traditionalists attest the problem was diagnosed incorrectly and the cure (ambiguous compromise) created worse pathology. The constraint prevents either diagnosis from being doctrinally refuted because the texts encode both. Mandatrophy is not yet resolved but is structurally emerging: as the founding problem (alienation of modern believers from preconciliar rigidity) remains partially unsolved fifty years later (traditionalist movements grow, theological diversity increases, moral teaching instability persists), the case for the constraint's obsolescence strengthens. However, institutional inertia and the prohibition on explicitly declaring the Council fallible prevent formal mandatrophy resolution. The constraint persists not because it continues solving the founding problem but because the institution's authority structure depends on the Council's presumed infallibility. This is early piton-signature: the constraint's function (coordinate diverse theological positions) has been largely replaced by hermeneutical enforcement (suppress the appearance of divergence), but the constraint remains because declaring it obsolete would undermine papal authority itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorial_intent_vs_textual_encoding,
    'Did the conciliar drafters intentionally encode incompatible readings, or did ambiguous compromise language emerge unintentionally from the need to accommodate diverse positions?',
    'Historical analysis of drafting committee records, Vatican archives released at 25+ year remove, testimony from key drafters (where available), cross-comparison of text evolution through voting and editorial rounds.',
    'If intentional encoding (supports this reading): the constraint is a deliberate structure of hermeneutical control. If unintentional (supports continuity or rupture readings): the ambiguity is accident, and one reading can claim the texts'' ''true meaning'' was simply obscured. The distinction determines whether the divided interpretation is feature or bug.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorial_intent_vs_textual_encoding, empirical, 'Whether the Council''s textual ambiguity was designed or emergent.').

omega_variable(
    hermeneutical_authority_legitimacy,
    'When the Council''s texts permit multiple incompatible readings, who holds authority to declare which reading is the correct interpretation of magisterial intent?',
    'Formal papal declaration (e.g., a Motu Proprio explicitly endorsing one reading and rejecting others) would resolve by institutional authority. Absent such declaration, no mechanism exists — the ambiguity is permanent by design.',
    'If hermeneutical authority is clearly vested in a single seat (Pope, episcopal college, theologians), that seat can suppress other readings. If authority is diffuse (multiple seats claim legitimate interpretive power), the constraint perpetuates as Tangled Rope indefinitely. If no resolution occurs, institutionalization of hermeneutical competition becomes the constraint''s steady state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hermeneutical_authority_legitimacy, preference, 'Which seat legitimately controls conciliar interpretation when texts are ambiguous.').

omega_variable(
    traditionalist_identity_lock_mechanism,
    'Is the traditionalist exit barrier (identity_locked) primarily structural (professional/institutional dependence on the Church) or internalized (fused identity with pre-conciliar doctrine that makes exit psychologically unthinkable)?',
    'Observation of post-exit trajectories: if traditionalists who sever institutional ties retain pre-conciliar doctrinal commitments and rebuild communities outside the Church, the lock is primarily structural. If they experience persistent identity crisis or doctrinal drift after exit, the lock is partially internalized.',
    'If structural only: traditionalists could exit at lower cost if institutional barriers were removed; suppression is maintained by institutional enforcement, not internalized. If internalized: traditionalists carry the suppression with them after exit; the constraint''s extractive force persists even after institutional coercion is removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_identity_lock_mechanism, empirical, 'Whether traditionalist suppression is structural or internalized identity lock.').

omega_variable(
    rupture_reading_empirical_challenge,
    'Do post-conciliar doctrinal developments (papal encyclicals, curial teachings, changed moral discipline) constitute logical continuation of pre-conciliar doctrine, or do they represent substantive rupture that the Council texts only ambiguously authorized?',
    'Systematic theological analysis comparing pre-conciliar and post-conciliar teaching on specific doctrines (ecclesiology, sexual ethics, religious freedom, ecumenism). Comparison of logical compatibility vs. material discontinuity.',
    'High compatibility evidence supports the continuity reading. High discontinuity evidence supports the rupture reading. Mixed evidence (some doctrines continuous, others ruptured) supports the composite-overdetermination reading: the texts accommodate both trajectories because they were intentionally designed to do so.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rupture_reading_empirical_challenge, empirical, 'Whether post-conciliar development represents continuous unfolding or substantive rupture.').

omega_variable(
    magisterial_authority_cost_to_traditionalists,
    'What proportion of the measured suppression (0.72) is attributable to explicit magisterial discipline (papal directives, curial censure, episcopal sanctions) vs. internalized doctrinal displacement (the psychological cost of inhabiting a doctrinal position the institution no longer teaches)?',
    'Comparative study of traditionalist communities with high vs. low institutional enforcement (e.g., diocese with actively progressive bishop vs. diocese with permissive bishop): if suppression and resistance track with enforcement intensity, suppression is primarily structural; if they persist regardless of enforcement, suppression is primarily internalized.',
    'If primarily structural: removing institutional enforcement would reduce suppression. If primarily internalized: the constraint would persist even after institutional enforcement ceased, as internalized identity-lock maintains the barrier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_authority_cost_to_traditionalists, empirical, 'Whether suppression is institutional enforcement or internalized identity lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t6, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 6, 0.31).
narrative_ontology:measurement_basis(vati_tr_t6, observed).
narrative_ontology:measurement(vati_tr_t12, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(vati_tr_t12, observed).
narrative_ontology:measurement(vati_tr_t24, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement_basis(vati_tr_t24, observed).
narrative_ontology:measurement(vati_tr_t36, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 36, 0.54).
narrative_ontology:measurement_basis(vati_tr_t36, observed).
narrative_ontology:measurement(vati_tr_t48, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 48, 0.57).
narrative_ontology:measurement_basis(vati_tr_t48, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 60, 0.58).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t6, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement_basis(vati_be_t6, observed).
narrative_ontology:measurement(vati_be_t12, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement_basis(vati_be_t12, observed).
narrative_ontology:measurement(vati_be_t24, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(vati_be_t24, observed).
narrative_ontology:measurement(vati_be_t36, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 36, 0.66).
narrative_ontology:measurement_basis(vati_be_t36, observed).
narrative_ontology:measurement(vati_be_t48, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 48, 0.68).
narrative_ontology:measurement_basis(vati_be_t48, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t6, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 6, 0.51).
narrative_ontology:measurement_basis(vati_su_t6, observed).
narrative_ontology:measurement(vati_su_t12, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement_basis(vati_su_t12, observed).
narrative_ontology:measurement(vati_su_t24, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement_basis(vati_su_t24, observed).
narrative_ontology:measurement(vati_su_t36, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 36, 0.7).
narrative_ontology:measurement_basis(vati_su_t36, observed).
narrative_ontology:measurement(vati_su_t48, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 48, 0.71).
narrative_ontology:measurement_basis(vati_su_t48, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.18).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, catholic_liturgical_discipline_post_conciliar).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, episcopal_authority_structures_post_vatican_ii).

% DUAL FORMULATION NOTE:
% Vatican II is a contested kernel with three reading-dependent constraints. This story instantiates the composite-overdetermination reading: Vatican II texts were intentionally drafted to encode incompatible ecclesiologies via ambiguous compromise language, making hermeneutical control the real locus of magisterial authority. Sibling readings (continuity and rupture) are separate constraint stories with different ε values, different beneficiary/victim structures, and different type classifications from each seat. All three readings are held simultaneously by different institutional seats; none prevails without triggering schism or delegitimizing the Council. The kernel-level constraint is the magisterial authority structure itself (vatican_ii_magisterial_authority); the three readings decompose it into structurally distinct constraints based on how the kernel's legitimacy is interpreted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
