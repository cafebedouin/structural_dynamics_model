% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation Continuity — Performance-Only Reading
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   Within the performance_only reading of the
 *   sacrifice_obligation_continuity kernel, the Torah's sacrificial
 *   commandments remain fully binding on every member of the covenant, and
 *   only physical performance discharges them. Because the Temple stands
 *   destroyed and the rite cannot lawfully occur elsewhere, the entire living
 *   obligated generation occupies a standing state of non-fulfillment: the
 *   obligation demands what circumstances forbid, and nothing presently
 *   available — study, prayer, charity, repentance — counts as discharge;
 *   study of the sacrificial tracts is preparation for a future restoration,
 *   not satisfaction. This file authors THAT reading only, as a clean
 *   epsilon-invariant constraint: the standing arrangement under contest is
 *   the binding-but-unperformable obligation itself, and epsilon (0.76) is
 *   assessed by this reading's own lights — the reading affirms the
 *   obligation's legitimacy while its own structure concedes that every
 *   obligated Jew today carries an unremedied deficiency. Three sibling
 *   readings (study_as_performance, messianic_suspension,
 *   archival_preservation) are separate constraints with their own epsilon
 *   values and victim sets, linked via network.affects_constraints; their
 *   values are not averaged here. The claimed type and the authored metrics
 *   are independent facts: the claim asserts a real coordination function
 *   (covenantal continuity across exile) coexisting with asymmetric
 *   extraction (an unremedied obligation borne by the immobile, administered
 *   by seats that collect authority from its persistence), while the metrics
 *   describe strongly extractive, actively enforced operation. Where computed
 *   per-seat types diverge from the claim, that divergence is the datum the
 *   corpus exists to take.
 *
 * KEY AGENTS:
 *   - - rabbinic_halakhic_authorities: agenda setter (institutional/identity_locked) — teaches the obligation's bindingness, adjudicates exemptions and intention, maintains liturgical rehearsal; collects authority and vocation from the arrangement it stewards
 *   - - contemporary_observant_jews: primary target (moderate/identity_locked) — bound by commandments they cannot perform; pay in standing deficiency, preparatory labor, and daily petition; exit means leaving the covenant
 *   - - levitical_priestly_line: prospective beneficiary (organized/identity_locked) — designated officiants of a restored cult; maintain genealogical and purity readiness now, collect actual receipts only upon an indefinitely deferred event
 *   - - restoration_movement_institutions: beneficiary (organized/constrained) — fabricate vessels, sew garments, breed qualification candidates; draw mission, membership, and funding from the obligation's bindingness
 *   - - secular_covenant_descendants: unconsulted bound party (moderate/arbitrage) — born into the obligation this reading asserts, absent from the discourse that administers it
 *   - - comparative_ritual_scholars: analytical observer (analytical/analytical) — documents the post-destruction conversion of rite into text and the contest over its normative force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.76).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.66).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.76).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation Continuity — Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, 'f276d8be-48cb-454c-bbab-69f43daa6c64').
narrative_ontology:cs_kernel_codification('f276d8be-48cb-454c-bbab-69f43daa6c64', fixed_text).
narrative_ontology:cs_authority_grounding('f276d8be-48cb-454c-bbab-69f43daa6c64', lineage).
narrative_ontology:cs_interpretation_layer_present('f276d8be-48cb-454c-bbab-69f43daa6c64').
narrative_ontology:cs_reading_relation('f276d8be-48cb-454c-bbab-69f43daa6c64', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('f276d8be-48cb-454c-bbab-69f43daa6c64', sacrifice_obligation_continuity__messianic_suspension, forecloses).
narrative_ontology:cs_reading_relation('f276d8be-48cb-454c-bbab-69f43daa6c64', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('f276d8be-48cb-454c-bbab-69f43daa6c64', foundational, sacrificial_obligation_remains_binding).
narrative_ontology:cs_axiom_status(sacrificial_obligation_remains_binding, holdable).
narrative_ontology:cs_axiom_grounding('f276d8be-48cb-454c-bbab-69f43daa6c64', sacrificial_obligation_remains_binding, deontological).
narrative_ontology:cs_axiom('f276d8be-48cb-454c-bbab-69f43daa6c64', foundational, study_is_preparation_not_fulfillment).
narrative_ontology:cs_axiom_status(study_is_preparation_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('f276d8be-48cb-454c-bbab-69f43daa6c64', study_is_preparation_not_fulfillment, deontological).
narrative_ontology:cs_axiom('f276d8be-48cb-454c-bbab-69f43daa6c64', secondary, non_performance_registers_as_present_deficiency).
narrative_ontology:cs_axiom_status(non_performance_registers_as_present_deficiency, holdable).
narrative_ontology:cs_axiom_grounding('f276d8be-48cb-454c-bbab-69f43daa6c64', non_performance_registers_as_present_deficiency, deontological).
narrative_ontology:cs_reference_frame('f276d8be-48cb-454c-bbab-69f43daa6c64', eternal_statutes_deferred_performance).
narrative_ontology:cs_drift_state('f276d8be-48cb-454c-bbab-69f43daa6c64', post_1967_restoration_activism, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f276d8be-48cb-454c-bbab-69f43daa6c64', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, rabbinic_halakhic_authorities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, levitical_priestly_line).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, restoration_movement_institutions).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, contemporary_observant_jews).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, secular_covenant_descendants).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, covenantal_continuity_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, temple_restoration_necessity).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, divine_command_immutability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach that the sacrificial commandments bind today, adjudicate who owes what and with what intention, maintain the liturgical schedule that petitions for restoration three times daily, and train students in the tractates that preserve procedure. Their institutional centrality, vocation, and deference flow from administering an obligation that cannot be discharged; releasing it would dissolve the ground of their own office, so exit from the arrangement is unavailable to them without ceasing to be what they are.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, rabbinic_halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__performance_only, rabbinic_halakhic_authorities, beneficiary).

% Are bound by commandments they cannot perform: they petition daily for the conditions of performance, study the procedures as preparation, and carry a standing deficiency that nothing in the present remedies. Leaving the arrangement means leaving the covenant — dissolving family, community, and self-understanding at once — so the cost of the obligation is paid without a realistic exit price attached to refusal.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, contemporary_observant_jews, payer,
    moderate, biographical, identity_locked, global).

% Are the designated officiants of a restored cult: they maintain genealogical registries, purity discipline, and liturgical familiarity now, and would receive the actual offerings, dues, and central station of the rite only if restoration occurs. Their receipts are deferred indefinitely; their present yield is status and vocation as custodians of a future service, and their lineage identity makes departure from the role costly in the same way it is for the authorities.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, levitical_priestly_line, beneficiary,
    organized, generational, identity_locked, global).

% Fabricate vessels, sew priestly garments, raise and vet candidates for the ashes of purification, and educate publics in the coming service. Their funding, membership, and entire mission exist because the obligation binds and awaits; if the obligation were discharged, suspended as faultless, or retired to memory, their reason for existing evaporates. Their scope is concentrated in one country even though the constituency they address is worldwide.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, restoration_movement_institutions, beneficiary,
    organized, generational, constrained, national).

% Were born into the covenant this reading binds and, by the reading's own terms, remain obligated and deficient despite lifelong indifference. They do not participate in the councils that administer the obligation, and the frame answers their objection doctrinally rather than procedurally. In practice they hold the cheapest position available: they keep the community-of-origin goods — belonging, history, peoplehood — while declining the observance costs, an asymmetry the arrangement tolerates because enforcing against them would exceed its coercive reach.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, secular_covenant_descendants, payer,
    moderate, biographical, arbitrage, global).

% Study how communities convert unperformable rites into obligation, memory, or anticipation after cultic centers fall; document the textual transmission, the liturgical embedding, and the modern institutional revivals. They hold no stake in which account of the obligation prevails and can see the whole structure from outside every seat.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, comparative_ritual_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__performance_only, rabbinic_halakhic_authorities).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a sanctuary-dependent commandment-set alive as binding commitment across an indefinite post-destruction interval: coordinates communal orientation toward restoration, embeds petition in daily liturgy, and preserves the procedural knowledge (tractates, measurements, purity law, priestly qualification) that resumption would require — maintained collectively rather than left to individual memory or lapse.
% TRANSFER_FUNCTION: Moves assurance of discharge from the living generation to a deferred future: the present pays in standing deficiency, preparatory labor, and petition and receives no atonement now, while present authority, vocation, and institutional purpose flow to the seats that administer the obligation and prepare for its resumption.
% ABSENT_VOICES: Secular covenant descendants — bound by this reading's own terms, absent from the councils that administer the obligation — would object that perpetual indebtedness was never consented to; holders of rival readings are answered doctrinally within this frame rather than seated procedurally. Their absence is part of how unanimity about bindingness is maintained inside the frame.
% DISAPPEARANCE_RATIONALE: If the binding-unperformable frame vanished overnight, the standing deficiency dissolves (nothing is owed that cannot be paid), preparatory study reframes as cultural or intellectual heritage, restoration institutions lose their warrant, and the interpreting authority loses a primary function; communities would rearrange around whichever successor frame they adopted — memory, study-as-fulfillment, or faultless suspension — rather than around unremedied obligation.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), commandments requiring a central sanctuary became unperformable. The founding problem: whether and how to keep those commandments alive as obligation — rather than letting them lapse into memory — during an interval of indefinite length, preserving both covenantal fidelity and the practical knowledge resumption would need.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: academic historiography of post-destruction Judaism corroborates that the founding crisis was real and that preserving obligation against lapse was a deliberate, contested strategy; statutory petition for restoration embedded in liturgy composed centuries before the modern authority structure independently attests the problem's long life. Whether it remains live is disputed along the kernel's own fault lines: the benefiting parties attest liveness (readiness is built, restoration pursued), holders of the archival reading attest the problem is retired and the obligation with it, and no seat outside all four readings adjudicates.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.76) because the arrangement's cost falls on every obligated Jew as a standing, unremediable deficiency: the reading grants no discharge channel, so the obligation extracts guilt, preparatory labor, and liturgical petition without returning atonement-assurance to anyone now living. Suppression (0.66) is real but non-coercive: persistence depends on active teaching of bindingness, thrice-daily liturgical reminder, childhood formation, and doctrinal foreclosure of substitute fulfillments — much of the suppressive force is internalized rather than externally applied (see the suppression_mechanism_ambiguity omega). Theater ratio (0.38) reflects a growing ceremonial-rehearsal layer — vessel fabrication, garment sewing, reenactment ceremonies, red-heifer husbandry — atop a core of sincerely meant preparatory study; the reading itself would score this lower, insisting the activity is readiness rather than performance. Accessibility collapse (0.72) is high within the frame — once the obligation is understood as binding and unperformable, alternatives collapse to waiting — but is tempered by the live existence of rival readings as adoptable frames. Resistance (0.58) is substantial: three rival readings contest the frame and large populations decline its terms, though organized resistance inside observing communities is thin. The temporal series run on one shared eight-point grid (1948-2025). The suppression_requirement series is authored because enforcement capacity demonstrably changed over the interval: institutional build-out after 1967 (expanded sacrificial-law curricula, restoration institutes founded 1987, annual reenactment ceremonies) raised active-maintenance intensity to a plateau by the early 2000s. Extractiveness rises monotonically because each increment of restoration feasibility narrows the gap between conceivable and actual performance, making the standing non-fulfillment less excusable rather than more.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the payer seats (contemporary_observant_jews, secular_covenant_descendants) the arrangement presents as enforced extraction: an obligation that binds, cannot be met, and offers no discharge — deficiency without remedy, with exit priced at covenant membership itself. From the agenda_setter seat the same structure presents as faithful stewardship of a real coordination function: keeping a commandment-set alive across an indefinite interval and preserving the technical knowledge resumption would require. The levite seat computes as a deferred-benefit hybrid: present costs (genealogical vigilance, purity discipline) against receipts payable only on an event with no scheduled date. The engine derives these divergences from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. rabbinic_halakhic_authorities sit nearest the beneficiary pole: they collect authority, vocation, and institutional centrality from the obligation's persistence, and their exit is identity-locked — releasing the obligation dissolves the ground of their office. contemporary_observant_jews sit near the target pole: they bear the full transfer with identity-locked exit; whatever identity goods covenant membership returns them are constitutive of the bind, not offsets against it. levitical_priestly_line are declared beneficiaries but collect prospectively: their present position carries costs against indefinitely deferred receipts, so their true directional value sits higher than a present-tense beneficiary derivation would place it; no override is authored because the deviation is modest and is documented here rather than forced through the override chain. restoration_movement_institutions collect in the present (funding, membership, mission) and sit near the beneficiary pole. secular_covenant_descendants are declared victims but hold arbitrage-grade exit: they capture community-of-origin goods while declining observance costs, so effective extraction against them is damped relative to the observant payer seat. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the constraint's global scope — verification of interior states like guilt and readiness is hard at wide scope, which amplifies effective extraction modestly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping a sanctuary-dependent commandment-set alive as obligation rather than memory after 70 CE — is genuinely contested: the benefiting parties attest it live (readiness is built, restoration pursued), holders of the archival reading attest it dead, and no seat outside all four readings adjudicates; hence founding_problem_status 'contested' paired with disappearance_verdict 'world_rearranges'. That pairing produces no dead-mandate flag, and rightly: the coordination function (covenantal continuity, communal orientation, preserved procedure) is real and exercised daily. The obsolescence risk is prospective, not present: if restoration never arrives, the arrangement's justification thins to pure anticipation, the preparatory layer grows theatrical, and the structure drifts toward inertia maintained by authority habit — the rising theater_ratio series is the early instrument for detecting that drift. Classification discipline cuts both ways: labeling the arrangement pure extraction would erase the genuine coordination that carried a dispersed community for two millennia; labeling it pure coordination would erase the unremedied deficiency the reading itself concedes. The tangled_rope claim holds both facts in one structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexical,
    'Which reading of the sacrifice_obligation_continuity kernel correctly characterizes the obligation''s present status — binding-and-unperformable (this file), discharged-by-study, suspended-blameless, or lapsed-to-memory?',
    'Not resolvable by data alone: the readings partition the same predicate space (bindingness, discharge condition, fault-status of present non-performance), so a single party cannot coherently hold two. Resolution proceeds by tracking which reading''s predicted victim set and extraction profile match observable community outcomes: prevalence of preparatory guilt, adoption of substitute-fulfillment practice, funding flows to restoration institutions.',
    'Sibling readings transform the structure: study_as_performance empties the victim set (obligation discharged, guilt unfounded); messianic_suspension removes fault while keeping readiness; archival_preservation drives epsilon toward zero and converts payers into heirs of memory. This file''s epsilon (0.76) is valid only for the performance_only reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexical, conceptual, 'This constraint is one reading of a four-way contested kernel; epsilon and victim set are reading-indexed.').

omega_variable(
    restoration_feasibility,
    'Will the conditions for lawful physical performance ever obtain, and on what timescale relative to the obligated generation''s horizon?',
    'Track political-legal status of the Temple Mount, viability of ritual-qualification programs (qualified priesthood, red-heifer candidacy, vessel completion), and restoration-movement growth rates; treat theological certainty as unresolvable and political feasibility as the operative variable.',
    'If restoration is infeasible on any relevant horizon, the arrangement''s deferred-benefit leg collapses and the structure trends toward pure extraction maintained by authority habit; if feasible, the mixed coordination-plus-extraction reading strengthens — present cost buys a real, if remote, future discharge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_feasibility, empirical, 'Feasibility of the restoration on which the reading''s deferred benefits depend.').

omega_variable(
    guilt_phenomenology,
    'Is the unremedied deficiency borne by the obligated generation phenomenologically real and widespread, or a doctrinal construction experienced thinly by most?',
    'Ethnographic and survey study of observant communities: prevalence of preparatory guilt, confessional language about unfulfilled sacrifice, differential distress between communities teaching the performance_only frame versus rival frames.',
    'If the guilt is thin, measured extraction drops materially and the payer seat computes nearer symmetric; if thick, the high-epsilon profile is confirmed and the arrangement''s human cost is as described.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guilt_phenomenology, empirical, 'Whether the extraction the reading imposes registers as lived experience.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (communal enforcement, childhood formation, liturgical scheduling) or internalized (self-administered guilt, identity fusion with covenant obligation)?',
    'Post-exit suppression trajectory: interview leavers across decades; if obligation-shaped guilt and readiness habits persist after communal exit, classify the residual as internalized.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — carriers take the obligation with them after exit, and exit options are weaker than the social picture suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in a communally enforced doctrinal obligation.').

omega_variable(
    cs_kernel_framing_underdetermination,
    'Is the kernel the fixed text (the Pentateuchal sacrificial legislation) or the interpretive practice-community that decides what the text demands now?',
    'Test framing sensitivity: re-run classification under a practice-grounded kernel (codification implicit, authority grounded in practice); if the commitment-system pattern classification shifts, the framing choice is load-bearing and must be documented per reading.',
    'Under a practice-kernel framing, drift migrates from the interpretive layer into the kernel itself and the reading''s stability claims weaken; under the text-kernel framing adopted here, drift is absorbed below the kernel by the interpretive apparatus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_kernel_framing_underdetermination, conceptual, 'Alternative framings of the kernel produce different commitment-system classifications.').

omega_variable(
    coalition_potential_of_payers,
    'Could the payer seats form an effective coalition to renegotiate or release the obligation, given their diffusion and identity-lock?',
    'Historical analysis of instances where mass communal sentiment shifted the obligation''s operative terms (e.g., the de facto victory of study-equivalence practice in many communities despite formal bindingness); compare organizational density of payer constituencies across regions.',
    'If coalition potential is real, the arrangement''s stability partly depends on keeping payers diffuse, and suppression functions partly as anti-coalition maintenance; if unreal, the identity-lock explanation stands alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_potential_of_payers, empirical, 'Whether diffuse identity-locked payers retain collective leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(socpo_tr_t1948, sacrifice_obligation_continuity__performance_only, theater_ratio, 1948, 0.22).
narrative_ontology:measurement_basis(socpo_tr_t1948, observed).
narrative_ontology:measurement(socpo_tr_t1959, sacrifice_obligation_continuity__performance_only, theater_ratio, 1959, 0.24).
narrative_ontology:measurement_basis(socpo_tr_t1959, observed).
narrative_ontology:measurement(socpo_tr_t1970, sacrifice_obligation_continuity__performance_only, theater_ratio, 1970, 0.28).
narrative_ontology:measurement_basis(socpo_tr_t1970, observed).
narrative_ontology:measurement(socpo_tr_t1981, sacrifice_obligation_continuity__performance_only, theater_ratio, 1981, 0.31).
narrative_ontology:measurement_basis(socpo_tr_t1981, observed).
narrative_ontology:measurement(socpo_tr_t1992, sacrifice_obligation_continuity__performance_only, theater_ratio, 1992, 0.34).
narrative_ontology:measurement_basis(socpo_tr_t1992, observed).
narrative_ontology:measurement(socpo_tr_t2003, sacrifice_obligation_continuity__performance_only, theater_ratio, 2003, 0.36).
narrative_ontology:measurement_basis(socpo_tr_t2003, observed).
narrative_ontology:measurement(socpo_tr_t2014, sacrifice_obligation_continuity__performance_only, theater_ratio, 2014, 0.37).
narrative_ontology:measurement_basis(socpo_tr_t2014, observed).
narrative_ontology:measurement(socpo_tr_t2025, sacrifice_obligation_continuity__performance_only, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(socpo_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(socpo_be_t1948, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1948, 0.62).
narrative_ontology:measurement_basis(socpo_be_t1948, observed).
narrative_ontology:measurement(socpo_be_t1959, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1959, 0.64).
narrative_ontology:measurement_basis(socpo_be_t1959, observed).
narrative_ontology:measurement(socpo_be_t1970, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1970, 0.67).
narrative_ontology:measurement_basis(socpo_be_t1970, observed).
narrative_ontology:measurement(socpo_be_t1981, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1981, 0.69).
narrative_ontology:measurement_basis(socpo_be_t1981, observed).
narrative_ontology:measurement(socpo_be_t1992, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1992, 0.71).
narrative_ontology:measurement_basis(socpo_be_t1992, observed).
narrative_ontology:measurement(socpo_be_t2003, sacrifice_obligation_continuity__performance_only, base_extractiveness, 2003, 0.73).
narrative_ontology:measurement_basis(socpo_be_t2003, observed).
narrative_ontology:measurement(socpo_be_t2014, sacrifice_obligation_continuity__performance_only, base_extractiveness, 2014, 0.75).
narrative_ontology:measurement_basis(socpo_be_t2014, observed).
narrative_ontology:measurement(socpo_be_t2025, sacrifice_obligation_continuity__performance_only, base_extractiveness, 2025, 0.76).
narrative_ontology:measurement_basis(socpo_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(socpo_su_t1948, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1948, 0.52).
narrative_ontology:measurement_basis(socpo_su_t1948, observed).
narrative_ontology:measurement(socpo_su_t1959, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1959, 0.54).
narrative_ontology:measurement_basis(socpo_su_t1959, observed).
narrative_ontology:measurement(socpo_su_t1970, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement_basis(socpo_su_t1970, observed).
narrative_ontology:measurement(socpo_su_t1981, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1981, 0.63).
narrative_ontology:measurement_basis(socpo_su_t1981, observed).
narrative_ontology:measurement(socpo_su_t1992, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1992, 0.65).
narrative_ontology:measurement_basis(socpo_su_t1992, observed).
narrative_ontology:measurement(socpo_su_t2003, sacrifice_obligation_continuity__performance_only, suppression_requirement, 2003, 0.66).
narrative_ontology:measurement_basis(socpo_su_t2003, observed).
narrative_ontology:measurement(socpo_su_t2014, sacrifice_obligation_continuity__performance_only, suppression_requirement, 2014, 0.66).
narrative_ontology:measurement_basis(socpo_su_t2014, observed).
narrative_ontology:measurement(socpo_su_t2025, sacrifice_obligation_continuity__performance_only, suppression_requirement, 2025, 0.66).
narrative_ontology:measurement_basis(socpo_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the sacrificial obligation after the Temple' covers four structurally distinct claims that share one kernel (sacrifice_obligation_continuity) and partition on bindingness, discharge condition, and fault-status of present non-performance. This file is the performance_only member. Epsilon differs sharply across members: archival_preservation approaches zero (memory without normative force); study_as_performance discharges the obligation through engagement (the victim set empties); messianic_suspension keeps readiness without fault; performance_only maximizes the victim set (every living obligated Jew) and carries the highest epsilon. All four read the same fixed text; the performance_only reading is the strict baseline against which the softer readings register as relaxations, and each relaxation cites prooftexts this reading must actively counter-explain — which is why requires_active_enforcement is true here. Family members link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
