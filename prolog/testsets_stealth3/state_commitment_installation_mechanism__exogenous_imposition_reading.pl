% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__exogenous_imposition_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: Exogenous Imposition Reading: Top-Down Installation of Commitments by Transformation-Mandate Authority
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the exogenous imposition reading of the kernel
 *   state_commitment_installation_mechanism: the claim that new normative and
 *   cultural commitments gain legitimacy through top-down installation by an
 *   authority holding a transformation mandate. The standing arrangement
 *   under contest — the referent of every metric here — is the installation
 *   mechanism itself as this reading describes it: decree issued at the apex,
 *   enforcement built out through administrative and clerical apparatus,
 *   conformity compelled on base populations, rival commitments criminalized.
 *   The classic material is early modern confessionalization and the
 *   disciplinary revolution (interval 0-200, mapping roughly c. 1550-1750):
 *   territorial states installing reformed creeds, liturgies, schooling, and
 *   moral discipline by edict. Per the epsilon-invariance rule this reading
 *   is authored alone as a clean single-epsilon constraint: the sibling
 *   readings (endogenous climb, hybrid cascade) are separate stories linked
 *   through the network, and all committer structure is routed to omega
 *   variables. The epsilon referent is the standing installation arrangement
 *   assessed by this reading's own lights — a comparative-coercion lens that
 *   takes the decree, the enforcement build-out, and the base resistance at
 *   face value — never any endorsed alternative arrangement. Claim and
 *   metrics are authored independently: claimed_type states what I believe is
 *   structurally true, the metrics describe the mechanism's actual operation.
 *
 * KEY AGENTS:
 *   - sovereign_transformation_authority: agenda-setter and primary beneficiary (institutional/arbitrage) — issues the decree, collects the returns of uniform commitment
 *   - central_administrative_apparatus: secondary beneficiary (institutional/constrained) — staffs the enforcement machinery, careers bound to it
 *   - installed_clergy_establishment: secondary beneficiary (organized/identity_locked) — holds the offices and vocations the installation creates
 *   - subject_populations: primary target (powerless/trapped) — bears exactions, service, and compelled affirmation
 *   - local_rival_practitioners: primary target (powerless/trapped) — holders of the displaced commitments, criminalized
 *   - provincial_magnates: dual-positioned seat (powerful/arbitrage) — pays exactions and loses jurisdiction while collecting offices and negotiated autonomy
 *   - popular_reform_movements: excluded voice (organized/constrained) — displaced grassroots initiatives, structurally absent from the arrangement
 *   - comparative_historians: analytical observer — sees the full comparative structure across episodes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.75).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.55).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "Exogenous Imposition Reading: Top-Down Installation of Commitments by Transformation-Mandate Authority").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, 'a18787f7-109b-4c0e-bae5-0c5af8ecc8cd').
narrative_ontology:cs_kernel_codification('a18787f7-109b-4c0e-bae5-0c5af8ecc8cd', distributed).
narrative_ontology:cs_authority_grounding('a18787f7-109b-4c0e-bae5-0c5af8ecc8cd', distributed).
narrative_ontology:cs_reading_relation('a18787f7-109b-4c0e-bae5-0c5af8ecc8cd', state_commitment_installation_mechanism__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('a18787f7-109b-4c0e-bae5-0c5af8ecc8cd', state_commitment_installation_mechanism__hybrid_cascade_reading, forecloses).
narrative_ontology:cs_axiom('a18787f7-109b-4c0e-bae5-0c5af8ecc8cd', foundational, legitimacy_conferred_by_mandate_authority).
narrative_ontology:cs_axiom_status(legitimacy_conferred_by_mandate_authority, holdable).
narrative_ontology:cs_axiom_grounding('a18787f7-109b-4c0e-bae5-0c5af8ecc8cd', legitimacy_conferred_by_mandate_authority, empirically_contingent).
narrative_ontology:cs_axiom('a18787f7-109b-4c0e-bae5-0c5af8ecc8cd', foundational, decree_installation_sufficient_for_commitment_validity).
narrative_ontology:cs_axiom_status(decree_installation_sufficient_for_commitment_validity, holdable).
narrative_ontology:cs_axiom_grounding('a18787f7-109b-4c0e-bae5-0c5af8ecc8cd', decree_installation_sufficient_for_commitment_validity, empirically_contingent).
narrative_ontology:cs_reference_frame('a18787f7-109b-4c0e-bae5-0c5af8ecc8cd', magisterial_installation_framework).
narrative_ontology:cs_drift_state('a18787f7-109b-4c0e-bae5-0c5af8ecc8cd', post_revisionist_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a18787f7-109b-4c0e-bae5-0c5af8ecc8cd', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, sovereign_transformation_authority).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, central_administrative_apparatus).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, installed_clergy_establishment).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, subject_populations).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, local_rival_practitioners).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, provincial_magnates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, provincial_magnates).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__exogenous_imposition_reading, transformation_mandate_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the decree installing the new commitment, commands the administrative and clerical apparatus that enforces it, and defines what counts as conformity. Collects the fiscal, military, and informational returns of uniform commitment: taxes assessed on a legible population, conscripts drawn from a disciplined one, forfeitures taken from dissenters. Can amend, re-scope, or abandon the installation project; its attachment to the mechanism is strategic rather than constitutive.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, sovereign_transformation_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Staffs the offices that record, assess, and enforce conformity: registries, fiscal bureaus, consistory courts, visitation commissions. Its jurisdictions, salaries, and promotion ladders exist because installation requires administration. Careers are bound to the apparatus; an official who opposed the installed commitment would forfeit position, and the apparatus as a whole has no life outside the enforcement role it was built for.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, central_administrative_apparatus, beneficiary,
    institutional, biographical, constrained, national).

% Holds the parishes, benefices, and teaching posts the installation creates or confirms. Delivers the mandated catechism, keeps the registers, administers discipline, and certifies conformity household by household. Its vocation is constituted by the installed commitment — a clergyman who rejected it would not merely lose office but lose the identity the office encodes.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, installed_clergy_establishment, beneficiary,
    organized, biographical, identity_locked, regional).

% Farm, work, and worship under the installed commitment. Owe the tithes, fees, and taxes the apparatus assesses, owe military service to the authority the commitment legitimates, and must affirm the creed, send children to the mandated schools, and appear before the discipline courts. Movement is possible in principle but costly and policed at the borders; staying means conforming, and conformity is audited.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, subject_populations, payer,
    powerless, generational, trapped, regional).

% Hold the commitments the decree displaces: older rites, local saints, dissenting congregations, vernacular practice. Continuance is criminalized — fines, penance, confiscation, expulsion, and in the hot phase death. The options are concealment, outward conformity with private practice, exile, or recantation, and each destroys something: community, property, or the practice itself.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, local_rival_practitioners, payer,
    powerless, biographical, trapped, local).

% Territorial lords and urban patriciates who lose jurisdiction, patronage, and religious leverage to the centralizing installation and pay their share of its exactions. The same installation hands them confirmed titles, offices, and access to the court, and their compliance is negotiated rather than decreed: they conform on schedule, host the visitations, and in exchange keep much of their local command. Their exit is not flight but renegotiation, and they use it.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, provincial_magnates, payer,
    powerful, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__exogenous_imposition_reading, provincial_magnates, beneficiary).

% Generate reform initiatives of their own — lay reading circles, prophetic movements, moral-renewal campaigns — that compete with the installed commitment and are displaced or suppressed by it. They are not parties to the installation: no decree consults them, no visitation records their consent. Their initiatives survive, where they survive, inside the forms the installed commitment permits.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, popular_reform_movements, excluded,
    organized, biographical, constrained, local).

% Compare installation episodes across polities and centuries — visitation records, consistory minutes, fiscal rolls, petition and revolt archives — to establish what decreed commitments required in order to hold, what they cost, and who collected. Hold no stake in any installation; their leverage is the comparative record itself.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__exogenous_imposition_reading, sovereign_transformation_authority).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__exogenous_imposition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Uniform commitment across a territory solves real collective-action problems: a single creed and moral discipline ends confessional civil conflict within the realm; a legible, standardized population can be taxed, conscripted, and adjudicated under common norms; shared catechism, schooling, and administrative language lower the cost of every subsequent transaction between center and locality.
% TRANSFER_FUNCTION: Moves compliance, fiscal resources (tithes, taxes, fees, forfeitures), military service, and identity-allegiance from base populations to the sovereign authority, which redistributes offices, salaries, and jurisdiction to the administrative and clerical apparatus that executes the installation.
% ABSENT_VOICES: The base populations were never parties to the arrangement — installation proceeds by decree, and no visitation, consistory, or registry records their consent, only their conformity or its absence. Popular reform movements whose own initiatives competed with the installed commitment are structurally absent: they appear in the record as objects of discipline, not as advocates. Their absence is not incidental; this reading's structure asserts there was no grassroots side to consult.
% DISAPPEARANCE_RATIONALE: If the installation mechanism vanished overnight, the installed commitments would not hold by their own weight: the fiscal-military apparatus built on legible, uniform populations loses its assessment base, the clerical and administrative offices created by the mechanism lose their function, and the realm re-fragments into the local and rival practices the decree displaced. The world this arrangement organizes rearranges around it.
% FOUNDING_PROBLEM: Post-Reformation territorial fragmentation: populations split across confessions, illegible to fiscal and military administration, prone to confessional civil war, and ungovernable by uniform law. The installation mechanism was built to manufacture a uniform, disciplined, legible population — one creed, one calendar, one moral discipline, one administrative language.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the archival record independent scholars work from: visitation and consistory records, fiscal rolls, and the resistance archive itself (petitions, recusancy rolls, revolt chronicles) attest both the fragmentation the mechanism addressed and the coercion of its operation. No surviving attestation from the base populations endorses the installation as their own project — their recorded voice is petition and revolt, which corroborates the coercion half of the founding problem while contesting the claim that the mechanism served them.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.75 at interval end) because the mechanism's returns — taxation on a legible population, conscription, tithes, office revenues, forfeitures — flow asymmetrically to the apex while the costs (compelled affirmation, criminalized practice, fiscal exaction) sit on base populations. Suppression is authored at 0.55 as the standing value, matching the interval end of the series: the mechanism shows a characteristic build-peak-normalize arc (0.35 at decree, peaking at 0.80 during the hot phase of expulsions and forced conversions, declining to 0.55 as conformity becomes routine administration), and the suppression_requirement series is authored because enforcement-capacity change is precisely what this story tracks. Theater rises from 0.15 to 0.45: early installation is functional coercion, while the late interval accumulates performative maintenance — anniversary celebrations, loyalty ceremonies, ritual affirmations — as the discipline becomes habitual. Accessibility collapse is 0.6: rival commitments do not vanish under installation, they go underground (crypto-practice, Nicodemism), so alternatives are suppressed but not eliminated. Resistance is 0.6: the mechanism reliably provokes base resistance — petition, recusancy, revolt — and the historical record of that resistance is part of what this reading asserts. All three series run on one shared time grid (nine points, 0-200) so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the sovereign authority's position the mechanism is statecraft it chose and can re-scope: uniform commitment is the price of legibility, fiscal reach, and confessional peace, and what it collects is the return on a project it owns. From the subject populations' position the same structure is compulsion they did not consent to and cannot exit: affirmation is audited, alternatives are criminalized, and the costs arrive as tithes, quotas, and penalties. The apparatus and clergy seats sit between — genuine beneficiaries whose careers and vocations depend on the mechanism's continuance, with constrained or identity-locked exit. The engine computes these per-seat classifications from the structural data; the divergence between the apex seat's coordination experience and the base seats' extraction experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereign transformation authority is the structural beneficiary and agenda-setter: it issues the decree, collects the returns, and holds arbitrage-grade exit (it can amend or abandon the project), placing it near the beneficiary end. The central apparatus and installed clergy are secondary beneficiaries — they collect the offices, salaries, and jurisdiction the mechanism creates; the clergy's identity lock deepens rather than damps their beneficiary position. Subject populations and local rival practitioners are the targets: they bear the exactions and the criminalized-alternative burden with trapped exit, placing them near the full-target end. Provincial magnates are genuinely dual-positioned — they pay exactions and lose jurisdiction (declared in the victim structure) yet collect offices and negotiated autonomy (secondary beneficiary role) with arbitrage-grade exit; the derivation should place them at moderate-high directionality rather than either pole. Popular reform movements are excluded rather than targeted: the mechanism does not so much extract from them as foreclose their advocacy route, which is why they appear on the stakeholder surface but not in the victim declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope claim guards against two mislabelings. Reading the mechanism as pure extraction would erase its real coordination function: uniform commitment solved genuine collective-action problems — confessional civil war, illegibility, the impossibility of taxation or conscription across doctrinally fragmented populations — and those problems were real, not cover. Reading it as pure coordination would erase the asymmetry: the same structure that produced confessional peace also produced confiscation, expulsion, and the criminalization of entire ways of life, and the coordination gains were collected at the apex while the costs sat at the base. On mandatrophy: the founding problem's status is contested rather than dead — the specific confessional emergency of the founding era resolved, but the mechanism persisted as default statecraft for every subsequent commitment-installation project (national languages, mass schooling, secularization campaigns), and whether those later installations serve live coordination problems or reproduce the template past its justification is exactly the mismatch question the founding_problem fields carry for the R5 consumer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel state_commitment_installation_mechanism — the exogenous imposition reading. What would the sibling readings (endogenous_climb_reading, hybrid_cascade_reading) change structurally, and where exactly is the disagreement located?',
    'Comparative adjudication across the three readings, each authored as a separate constraint story with its own epsilon and beneficiary/victim structure: cross-reading comparison of computed classifications locates the disagreement in (a) the direction of legitimacy flow (down from mandate authority vs up from institutional fringes) and (b) whether base uptake is a necessary condition of stabilization.',
    'If the hybrid reading''s stabilization condition is confirmed for most installation episodes, this reading''s victim/beneficiary structure shifts — base populations become partial co-authors of legitimacy rather than pure targets, lowering effective extraction for the payer seats; if the endogenous reading dominates the record, the apex-beneficiary declaration inverts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this story instantiates the exogenous imposition reading; siblings endogenous_climb and hybrid_cascade are separate constraints with separate epsilon values.').

omega_variable(
    installation_sufficiency_empirics,
    'Do commitments installed by decree actually hold without base uptake, or do installations systematically fail where fringe and base validation is absent?',
    'Comparative case analysis of installation episodes matched for enforcement capacity: code outcomes where decree was issued without subsequent popular uptake (failed imposed unions, mandated liturgical changes that sparked mass recusancy, reform programs that lapsed with their sponsor) against episodes where decree held; if failure-without-uptake is the norm, the reading''s sufficiency axiom is empirically overridden.',
    'Systematic failure without uptake collapses this reading toward the hybrid cascade reading and re-dates its drift state; isolated successes sustain the reading and keep the foreclosure relation live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(installation_sufficiency_empirics, empirical, 'Whether the reading''s foundational sufficiency axiom survives the comparative installation record.').

omega_variable(
    mandate_legitimacy_circularity,
    'Where does the transformation-mandate authority''s own legitimacy come from — and if that legitimacy is itself conferred by the commitments the authority installs, is the reading circular?',
    'Genealogical analysis of the mandate: trace whether the authority''s transformation claim precedes and is independent of the installed commitments (dynastic right, conquest, divine commission) or is constituted by them.',
    'If the mandate is constituted by the installed commitments, the mechanism''s legitimacy structure is bootstrapped — the returns flow to an authority whose warrant the mechanism itself manufactures — which strengthens the extraction reading of the arrangement and pushes classification toward the snare end; if the mandate is independent, the arrangement stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_legitimacy_circularity, conceptual, 'Whether the authority''s mandate is independent of, or manufactured by, the installation mechanism it runs.').

omega_variable(
    internalized_conformity_ambiguity,
    'Is the late-interval conformity genuine internalization of the installed commitment, or suppressed preference held in place by residual enforcement — and is the measured decline in suppression a real decay of coercive need or an artifact of successful deterrence?',
    'Post-enforcement trajectory analysis: crypto-practice persistence after enforcement relaxed, generational transmission of rival practice where detection risk fell, and the record of rapid re-polarization where enforcement collapsed through war or occupation.',
    'If conformity is suppressed preference, the standing suppression is higher than the scalar suggests and the theater ratio understates performative maintenance; if genuine internalization, the mechanism completed a transition from imposed commitment to self-sustaining identity — the strongest available case for its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_conformity_ambiguity, empirical, 'Whether late-interval conformity reflects internalization of the installed commitment or continued suppression of rival preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(stat_tr_t25, observed).
narrative_ontology:measurement(stat_tr_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(stat_tr_t50, observed).
narrative_ontology:measurement(stat_tr_t75, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 75, 0.25).
narrative_ontology:measurement_basis(stat_tr_t75, observed).
narrative_ontology:measurement(stat_tr_t100, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(stat_tr_t100, observed).
narrative_ontology:measurement(stat_tr_t125, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 125, 0.35).
narrative_ontology:measurement_basis(stat_tr_t125, observed).
narrative_ontology:measurement(stat_tr_t150, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 150, 0.4).
narrative_ontology:measurement_basis(stat_tr_t150, observed).
narrative_ontology:measurement(stat_tr_t175, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 175, 0.43).
narrative_ontology:measurement_basis(stat_tr_t175, observed).
narrative_ontology:measurement(stat_tr_t200, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 200, 0.45).
narrative_ontology:measurement_basis(stat_tr_t200, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement_basis(stat_be_t25, observed).
narrative_ontology:measurement(stat_be_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement_basis(stat_be_t50, observed).
narrative_ontology:measurement(stat_be_t75, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 75, 0.72).
narrative_ontology:measurement_basis(stat_be_t75, observed).
narrative_ontology:measurement(stat_be_t100, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 100, 0.76).
narrative_ontology:measurement_basis(stat_be_t100, observed).
narrative_ontology:measurement(stat_be_t125, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 125, 0.78).
narrative_ontology:measurement_basis(stat_be_t125, observed).
narrative_ontology:measurement(stat_be_t150, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 150, 0.77).
narrative_ontology:measurement_basis(stat_be_t150, observed).
narrative_ontology:measurement(stat_be_t175, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 175, 0.76).
narrative_ontology:measurement_basis(stat_be_t175, observed).
narrative_ontology:measurement(stat_be_t200, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 200, 0.75).
narrative_ontology:measurement_basis(stat_be_t200, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t25, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement_basis(stat_su_t25, observed).
narrative_ontology:measurement(stat_su_t50, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(stat_su_t50, observed).
narrative_ontology:measurement(stat_su_t75, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 75, 0.78).
narrative_ontology:measurement_basis(stat_su_t75, observed).
narrative_ontology:measurement(stat_su_t100, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 100, 0.8).
narrative_ontology:measurement_basis(stat_su_t100, observed).
narrative_ontology:measurement(stat_su_t125, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 125, 0.72).
narrative_ontology:measurement_basis(stat_su_t125, observed).
narrative_ontology:measurement(stat_su_t150, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 150, 0.62).
narrative_ontology:measurement_basis(stat_su_t150, observed).
narrative_ontology:measurement(stat_su_t175, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 175, 0.58).
narrative_ontology:measurement_basis(stat_su_t175, observed).
narrative_ontology:measurement(stat_su_t200, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 200, 0.55).
narrative_ontology:measurement_basis(stat_su_t200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, identity_coordination).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial claim 'new commitments gain legitimacy' decomposes into three structurally distinct mechanisms — endogenous climb, exogenous imposition, hybrid cascade — each with its own epsilon, beneficiary/victim structure, and classification. The kernel label conflates them; per the epsilon-invariance principle each reading is authored as a separate story linked through network edges. This (exogenous imposition) story is the most extractive of the three as authored: it alone asserts no grassroots role, so its victim set is largest and its coordination function is entirely apex-owned. The sibling files carry the reciprocal links and document how their epsilon values differ from this one's.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
