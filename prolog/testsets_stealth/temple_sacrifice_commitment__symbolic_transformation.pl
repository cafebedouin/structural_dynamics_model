% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Symbolic Transformation Reading of the Sacrifice Commitment
 *   domain: religious/halakhic/commitment-system
 *
 * SUMMARY:
 *   After the destruction of the Second Temple (70 CE), a binding command
 *   system whose entire material infrastructure — altar, priesthood,
 *   pilgrimage — had vanished faced covenant discontinuity. The rabbinic
 *   tradition responded by designating prayer and study as the sacrificial
 *   commandments' operative form. This file instantiates the
 *   symbolic_transformation reading of that arrangement: the transformation
 *   is AUTHORIZED, and prayer and study are the command's new instantiation,
 *   not substitutes for a suspended practice. The epsilon referent is the
 *   standing post-destruction rabbinic arrangement itself, assessed by this
 *   reading's own lights — since this reading endorses the standing
 *   arrangement as faithful, epsilon reflects the arrangement as it sees it:
 *   substantially legitimate, with visible structural residues (a
 *   self-certifying authorization loop, a permanently foreclosed materialist
 *   position, a displaced priestly caste). Constraint family: the colloquial
 *   label 'what happened to the sacrifice commandment' decomposes into four
 *   structurally distinct claims — this reading, study_as_exercise,
 *   performance_only, and hybrid_preparatory — each with its own epsilon,
 *   victim set, and authority implication; all four are linked via
 *   network.affects_constraints. Claim and metrics are authored
 *   independently: the claimed type is what I judge structurally true from
 *   this seat; the metrics are what I judge descriptively true of the
 *   arrangement's operation.
 *
 * KEY AGENTS:
 *   - rabbinic_authority_structure: agenda-setter (institutional/identity_locked) — administers the transformed practice and certifies its own authorization
 *   - practicing_lay_community: coordinated beneficiary with payer residue (organized/identity_locked) — receives covenantal continuity, pays deference and liturgical burden
 *   - material_performance_dissenters: primary target (powerless/constrained) — bear a redefined-away obligation they hold non-negotiable
 *   - kohanim_priestly_line: displaced vocational caste (organized/identity_locked) — hereditary service permanently closed, residual honors retained
 *   - halakhic_analysts: analytical observer — sees the full authorization structure from outside the practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.46).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.58).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.46).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Symbolic Transformation Reading of the Sacrifice Commitment").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious/halakhic/commitment-system").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, '1652141d-c69f-4720-9f8d-6ecd233d299b').
narrative_ontology:cs_kernel_codification('1652141d-c69f-4720-9f8d-6ecd233d299b', fixed_text).
narrative_ontology:cs_authority_grounding('1652141d-c69f-4720-9f8d-6ecd233d299b', lineage).
narrative_ontology:cs_interpretation_layer_present('1652141d-c69f-4720-9f8d-6ecd233d299b').
narrative_ontology:cs_reading_relation('1652141d-c69f-4720-9f8d-6ecd233d299b', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('1652141d-c69f-4720-9f8d-6ecd233d299b', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('1652141d-c69f-4720-9f8d-6ecd233d299b', temple_sacrifice_commitment__hybrid_preparatory, forecloses).
narrative_ontology:cs_axiom('1652141d-c69f-4720-9f8d-6ecd233d299b', foundational, prayer_study_constitute_current_instantiation).
narrative_ontology:cs_axiom_status(prayer_study_constitute_current_instantiation, holdable).
narrative_ontology:cs_axiom_grounding('1652141d-c69f-4720-9f8d-6ecd233d299b', prayer_study_constitute_current_instantiation, theological).
narrative_ontology:cs_axiom('1652141d-c69f-4720-9f8d-6ecd233d299b', foundational, rabbinic_power_to_transform_divine_commands).
narrative_ontology:cs_axiom_status(rabbinic_power_to_transform_divine_commands, holdable).
narrative_ontology:cs_axiom_grounding('1652141d-c69f-4720-9f8d-6ecd233d299b', rabbinic_power_to_transform_divine_commands, conventional).
narrative_ontology:cs_reference_frame('1652141d-c69f-4720-9f8d-6ecd233d299b', authorized_transformative_continuity).
narrative_ontology:cs_drift_state('1652141d-c69f-4720-9f8d-6ecd233d299b', contemporary_restorationist_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('1652141d-c69f-4720-9f8d-6ecd233d299b', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, practicing_lay_community).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, material_performance_dissenters).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, kohanim_priestly_line).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, kohanim_priestly_line).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, practicing_lay_community).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, prayer_as_service_of_the_heart).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, rabbinic_interpretive_authority).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, verbal_service_equivalence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fixes the liturgy that carries the sacrificial references, mandates the order of prayer and the curriculum of sacrificial-law study, and rules on what counts as faithful observance of the service commandments in the Temple's absence. Its standing to certify that the change from altar-service to prayer-and-study was legitimate rests on the same tradition it administers. Leaving that role would mean dissolving the office itself; the tradition and the institution are the same thing.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure, agenda_setter,
    institutional, generational, identity_locked, global).

% Prays the fixed liturgy three times daily on a schedule keyed to the old offerings, recites the sacrificial passages, and supports the institutions that teach them. Gains uninterrupted participation in the covenant's service obligations without any altar, priest, or pilgrimage. Pays in time, liturgical burden, and deference to interpretive rulings. Leaving the practice would mean leaving the community that formed them.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, practicing_lay_community, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, practicing_lay_community, payer).

% Hold that the service commandments bind in their material form — altar, offering, priest — and that no reinterpretation discharges them. Their position carries no standing in contemporary halakhic deliberation; restorationist efforts operate outside mainstream institutions and are treated as fringe or dangerous. They carry an obligation they believe remains wholly unmet, and their exit runs through abandoning the community that defines them.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, material_performance_dissenters, payer,
    powerless, generational, constrained, regional).

% Inherit a vocation the transformed arrangement permanently closes: the service was opened to all Israel through prayer, and no restoration of priestly prerogative is contemplated within the current frame. They retain residual honors — pronouncing the priestly blessing, first call to the Torah — as markers of lineage. Their identity is bound to a role the arrangement defines as historically complete.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, kohanim_priestly_line, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, kohanim_priestly_line, beneficiary).

% Study the transformation from outside the practice: historians of rabbinic literature, scholars of liturgy, and commitment-system theorists. They trace how the equation of prayer with sacrifice was argued, enforced, and transmitted, and can compare the authorization claim against parallel cases in other traditions.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, halakhic_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__symbolic_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a dispersed community's continuous observance of a command whose material infrastructure is unavailable, by designating prayer and study as the command's operative form and synchronizing communal life to the sacrificial schedule; preserves detailed operational knowledge of the rite.
% TRANSFER_FUNCTION: Moves interpretive deference and institutional support from the lay community to the rabbinic authority structure; moves the service obligation itself from the priestly line to the whole community; moves lay time and attention into mandated liturgy and study.
% ABSENT_VOICES: Material-performance dissenters would object that their non-negotiable obligation was redefined without consent; they hold no seat in halakhic deliberation and their modern representatives operate outside the institutions that administer the practice. The priestly line's own claim to the service likewise has no dedicated voice in contemporary adjudication.
% DISAPPEARANCE_RATIONALE: Communal prayer is scheduled on the sacrificial timetable and its liturgy is built from sacrificial language; overnight removal would unravel the fixed prayer order, break the claimed continuity of the service commandments, strip the rabbinic office of its legitimating function, and hand the material-performance position the default reading of the command.
% FOUNDING_PROBLEM: The destruction of the Second Temple removed the entire material infrastructure of a binding command system — altar, priesthood, pilgrimage — creating an unmet-obligation crisis that threatened covenant discontinuity and communal dissolution after catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: The material-performance dissenters themselves attest the founding problem is live — their entire position presupposes an unmet material obligation — and secular historians of ancient Judaism corroborate the infrastructural rupture of 70 CE as fact. Neither source belongs to the arrangement's benefiting parties.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).
:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.46) rather than high because this reading holds the transformation authorized — the high-extraction case named in the kernel context (unauthorized drift) is precisely what this reading denies. What keeps epsilon well above a pure coordination floor is structural, not doctrinal: the authorization is certified only by the authority structure that benefits from it, and the materialist position is foreclosed rather than answered. Suppression (0.58) is real but mostly historical-plus-internalized: coercive machinery (excommunication, liturgical gatekeeping, denial of standing to literalists) was built early and used at contest episodes; today identity-lock does most of the work, with the residual split estimated roughly 40% structural / 60% internalized (omega dissenter_suppression_mechanism). Theater (0.35) is rising slowly: recitation of the sacrificial orders increasingly functions as devotional habit, and restoration petitions increasingly run on liturgical momentum rather than live expectation — a drift-toward-piton signal watched but not yet reached. Accessibility collapse (0.55): within the rabbinic frame, alternatives to the transformed practice collapse almost completely, but sibling readings persist as live minority positions, keeping the measure moderate. Resistance (0.45): sustained low-grade dissent plus periodic restorationist surges. The measurement series share one eight-point grid (70/200/400/650/1000/1500/1800/2026) with every tracked metric authored at every point; the mild oscillations track external contest episodes (the Karaite schism, the nineteenth-century reform controversies, modern restorationism) rather than an internal reinforcement cycle.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as faithful stewardship: the transformation was authorized, the practice is the command, and administering it is obedience. The target seats experience the same structure as dispossession: dissenters hold a non-negotiable obligation redefined without their consent; the priestly line holds a vocation closed by interpretive fiat. The lay community sits near-symmetric — genuine continuity gained, deference paid. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic authority structure derives a directionality near the beneficiary pole: it collects legitimacy, deference, and institutional support from the very transformation it certifies. The lay community derives near-symmetric positioning — it appears in both the beneficiary array (continuity received) and, through its payer residue, bears diffuse costs. Material-performance dissenters derive near the full-target pole: the transformation operates directly on their obligation, their exit runs through communal rupture, and they hold no standing in the deliberation that redefined them. The priestly line derives target-side with moderation: the closure of their vocation is total within this reading, but residual honors and lineage status damp the extraction somewhat. Effective spatial scope is diaspora-global, which raises verification difficulty and amplifies effective extraction modestly for the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem — material infrastructure absent — remains live, so the arrangement has not outlived its function by its own genealogy. The classification discipline matters in both directions here. Reading the arrangement as pure coordination would erase the self-certification loop by which the transformation's beneficiaries authenticate their own authorization, and the dissenters whose obligation is redefined without consent. Reading it as pure extraction would erase a genuine coordination achievement: a dispersed community held in continuous covenantal practice for nineteen centuries without its material infrastructure. The tangled-rope claim holds both halves. The rising theater ratio is the metric to watch: if recitation becomes wholly habitual and the authorization question wholly forgotten, the arrangement drifts toward inertial maintenance — but the live founding problem and the persistent dissenting pressure keep it short of that.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is the symbolic_transformation reading of kernel temple_sacrifice_commitment; what structural deltas would each sibling reading introduce if adopted in place of this one?',
    'Compile and classify each sibling''s constraint file (temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory) and diff the computed per-seat classifications and epsilon profiles against this file.',
    'Under performance_only the victim set expands to the whole observing community (an obligation none can discharge), driving epsilon sharply upward and the classification toward snare; under study_as_exercise the authority''s mediating role dissolves and epsilon falls toward rope; under hybrid_preparatory a restoration condition imports a sunset clause and the shape moves toward scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: per-sibling structural deltas from swapping readings of the sacrifice-commitment kernel.').

omega_variable(
    authorization_warrant_independence,
    'Can the transformation''s authorization be established from warrants independent of the authority structure that benefits from it, or does the authorization claim rest solely on the beneficiaries'' own testimonial chain?',
    'Test the cited warrants (Hosea 14:3, Psalm 141:2, the Berakhot 26b equation of prayer with the daily offerings) for acceptance across factions that reject rabbinic institutional authority; survey parallel transformation cases in other command systems for independent verification patterns.',
    'If no independent warrant exists, the self-certification loop dominates, effective extraction rises toward the unauthorized-drift pole, and the classification shifts toward snare; if independent warrant holds, the arrangement remains a defensible tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorization_warrant_independence, conceptual, 'Whether the authorization claim is verifiable outside the benefiting authority structure.').

omega_variable(
    restoration_petition_coherence,
    'Does the liturgy''s daily petition for restoration of the sacrificial service cohere with the claim that prayer and study already instantiate it — and does the answer determine whether the transformation is complete or tacitly provisional?',
    'Trace how authoritative liturgists and decisors across eras reconciled the restoration petition with the transformation claim, and whether any mainstream source treats the petition as evidence that the current form is interim.',
    'If the petition reveals a tacitly provisional transformation, this reading collapses toward hybrid_preparatory, the arrangement acquires an undeclared sunset condition, and the extraction profile rises (an obligation held simultaneously discharged and outstanding).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_petition_coherence, conceptual, 'Internal coherence of the transformation-completeness claim against the liturgy''s own restoration petitions.').

omega_variable(
    dissenter_suppression_mechanism,
    'Is the present marginalization of material-performance dissenters maintained by structural exclusion (no standing in halakhic deliberation) or by internalized conformity (potential dissenters no longer generating dissent)?',
    'Post-exit trajectory: examine communities and movements that left the rabbinic frame to pursue material performance — whether dissent persists and organizes absent enforcement pressure, or dissipates.',
    'If internalized, measured resistance understates latent dissent and the suppression carried by targets exceeds the structural measure; if structural, the enforcement load is higher than observed and lifting institutional exclusion would release organized opposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissenter_suppression_mechanism, empirical, 'Structural versus internalized suppression of the material-performance position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsc_symbolic_transformation_tr_t70, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 70, 0.1).
narrative_ontology:measurement(tsc_symbolic_transformation_tr_t200, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 200, 0.15).
narrative_ontology:measurement(tsc_symbolic_transformation_tr_t400, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 400, 0.2).
narrative_ontology:measurement(tsc_symbolic_transformation_tr_t650, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 650, 0.24).
narrative_ontology:measurement(tsc_symbolic_transformation_tr_t1000, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1000, 0.28).
narrative_ontology:measurement(tsc_symbolic_transformation_tr_t1500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1500, 0.33).
narrative_ontology:measurement(tsc_symbolic_transformation_tr_t1800, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(tsc_symbolic_transformation_tr_t2026, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 2026, 0.35).

% Extraction over time
narrative_ontology:measurement(tsc_symbolic_transformation_be_t70, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 70, 0.38).
narrative_ontology:measurement(tsc_symbolic_transformation_be_t200, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 200, 0.48).
narrative_ontology:measurement(tsc_symbolic_transformation_be_t400, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 400, 0.53).
narrative_ontology:measurement(tsc_symbolic_transformation_be_t650, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 650, 0.56).
narrative_ontology:measurement(tsc_symbolic_transformation_be_t1000, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1000, 0.51).
narrative_ontology:measurement(tsc_symbolic_transformation_be_t1500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1500, 0.44).
narrative_ontology:measurement(tsc_symbolic_transformation_be_t1800, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1800, 0.5).
narrative_ontology:measurement(tsc_symbolic_transformation_be_t2026, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 2026, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(tsc_symbolic_transformation_su_t70, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 70, 0.62).
narrative_ontology:measurement(tsc_symbolic_transformation_su_t200, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 200, 0.66).
narrative_ontology:measurement(tsc_symbolic_transformation_su_t400, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 400, 0.6).
narrative_ontology:measurement(tsc_symbolic_transformation_su_t650, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 650, 0.64).
narrative_ontology:measurement(tsc_symbolic_transformation_su_t1000, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1000, 0.52).
narrative_ontology:measurement(tsc_symbolic_transformation_su_t1500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1500, 0.45).
narrative_ontology:measurement(tsc_symbolic_transformation_su_t1800, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement(tsc_symbolic_transformation_su_t2026, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 2026, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the natural-language label 'the fate of the sacrifice commandment after 70 CE' conflates four structurally distinct claims with different epsilon values, victim sets, and authority structures. This file authors the symbolic_transformation reading (authorized completed transformation; moderate epsilon; dissenters and priesthood as victims). The upstream claim common to the family — that the command remained binding despite infrastructural loss — is the most established member; the downstream contest over the transformation's authorization drives the epsilon spread across siblings. Each sibling file links back to this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
