% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinto-Buddhist Ritual Domain Partition (Life-Cycle vs Afterlife)
 *   domain: religious/historical
 *
 * SUMMARY:
 *   In Japan, Shinto shrines and Buddhist temples divide the ritual labor of
 *   a human life: shrines handle birth presentations, childhood milestones,
 *   weddings, purification, and New Year observance; temples handle funerals,
 *   graves, and memorial services. The division operates without any merged
 *   cosmology — the two traditions do not affirm each other's metaphysics,
 *   and no integrating doctrine governs the boundary. This file instantiates
 *   the PARTITION READING of the shinbutsu ontological commitment kernel: the
 *   claim that this stable division of domains, without ontological
 *   integration, is itself the standing arrangement under analysis. Its
 *   epsilon referent is that standing partition arrangement as this reading
 *   sees it — not the pre-Meiji honji-suijaku synthesis (the syncretic
 *   sibling's constraint) and not a mere tolerated inconsistency (the
 *   incoherence sibling's constraint). The interval runs from the Meiji
 *   separation edicts (T0 = 1868), which destroyed the integrated arrangement
 *   by state decree, to the present (T157 = 2025), tracing how a
 *   state-imposed separation decayed into a customary, low-coercion division
 *   of labor. KEY AGENTS (by structural relationship): - shrine_priesthood:
 *   Protected beneficiary and boundary-administrator
 *   (organized/identity_locked) — collects from the life-passage sphere -
 *   temple_clergy: Protected beneficiary and boundary-administrator
 *   (organized/identity_locked) — collects from the death sphere -
 *   lay_households: Net-benefiting payers (moderate/constrained) — receive
 *   complete ritual coverage and bear its fees -
 *   religious_minority_practitioners: Friction-bearing outsiders
 *   (powerless/mobile) — excluded from the settled bargain -
 *   alternative_ritual_entrants: Marginal competitors (moderate/mobile) —
 *   contest the edges of the death sphere - scholars_of_japanese_religiosity:
 *   Analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.32).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.22).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinto-Buddhist Ritual Domain Partition (Life-Cycle vs Afterlife)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious/historical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, '94077bb5-c8f8-41fd-83a3-fb5161e36b50').
narrative_ontology:cs_kernel_codification('94077bb5-c8f8-41fd-83a3-fb5161e36b50', distributed).
narrative_ontology:cs_authority_grounding('94077bb5-c8f8-41fd-83a3-fb5161e36b50', practice).
narrative_ontology:cs_interpretation_layer_present('94077bb5-c8f8-41fd-83a3-fb5161e36b50').
narrative_ontology:cs_reading_relation('94077bb5-c8f8-41fd-83a3-fb5161e36b50', shinbutsu_ontological_commitment__syncretic_reading, forecloses).
narrative_ontology:cs_reading_relation('94077bb5-c8f8-41fd-83a3-fb5161e36b50', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('94077bb5-c8f8-41fd-83a3-fb5161e36b50', foundational, ritual_domain_division_is_the_commitment).
narrative_ontology:cs_axiom_status(ritual_domain_division_is_the_commitment, holdable).
narrative_ontology:cs_axiom_grounding('94077bb5-c8f8-41fd-83a3-fb5161e36b50', ritual_domain_division_is_the_commitment, conventional).
narrative_ontology:cs_axiom('94077bb5-c8f8-41fd-83a3-fb5161e36b50', foundational, ontological_integration_unnecessary_for_coexistence).
narrative_ontology:cs_axiom_status(ontological_integration_unnecessary_for_coexistence, holdable).
narrative_ontology:cs_axiom_grounding('94077bb5-c8f8-41fd-83a3-fb5161e36b50', ontological_integration_unnecessary_for_coexistence, empirically_contingent).
narrative_ontology:cs_reference_frame('94077bb5-c8f8-41fd-83a3-fb5161e36b50', domain_partition_settlement).
narrative_ontology:cs_drift_state('94077bb5-c8f8-41fd-83a3-fb5161e36b50', contemporary_pluralist_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('94077bb5-c8f8-41fd-83a3-fb5161e36b50', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shrine_priesthood).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, temple_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, lay_households).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__partition_reading, religious_minority_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__partition_reading, lay_households).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, practice_over_creeds_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary and vocational priests serving shrines that hold the life-passage sphere: birth presentations (miyamairi), shichi-go-san milestones, weddings, purification rites, New Year observance. Income flows from offerings, ceremony fees, and talisman sales; the settled division guarantees shrines a protected ritual market and spares them funeral duties their pollution doctrines discourage. Administering the boundary is part of daily office — declining funeral requests and referring households to temples. Leaving the priesthood means abandoning a hereditary vocation, ordination training, and community standing.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shrine_priesthood, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__partition_reading, shrine_priesthood, agenda_setter).

% Priests of the Buddhist denominations holding the death sphere: funerals, graves, memorial services, and the parishioner (danka) rolls that attach households to temples through family graves. Funeral and memorial fees plus grave upkeep are the financial backbone of most parish temples, and the customary division secures that revenue against competition from shrines or secular entrants. Temples police their side of the line, treating funerary business as theirs by custom. Succession runs through sons and adopted clerical heirs; abandoning the temple means abandoning family, lineage, and the danka network at once.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, temple_clergy, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__partition_reading, temple_clergy, agenda_setter).

% Households moving through the life cycle: shrine visits for births and milestones, temple funerals and graves for deaths. The division hands them a complete, pre-assigned ritual itinerary — they never negotiate which institution handles which occasion. They pay for it in offerings, funeral fees that commonly exceed a million yen, and grave-maintenance dues, and they inherit danka obligations along with the family grave. Opting out — secular funerals, grave-free memorials, scattering ashes — is legally possible but carries family friction and a felt impiety toward ancestors.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, lay_households, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__partition_reading, lay_households, payer).

% Christians, adherents of new religions, Muslims, and committed secularists whose life-cycle needs do not map onto the two settled tracks. Nothing legally bars their own rites, but they face a society whose ritual infrastructure, cemetery customs, and family expectations assume the shrine-temple division, so every rite requires extra negotiation, explanation, or compromise. They were never seated when the division crystallized and remain outside the customary bargain that allocates their occasions to institutions they do not affirm.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, religious_minority_practitioners, excluded,
    powerless, biographical, mobile, national).

% Commercial funeral homes, non-religious celebrants, gravestone companies selling pre-need contracts, and operators of joint graves and ash-scattering services. They compete at the margins of the death sphere — increasingly successfully as temple succession fails — but the customary identification of funerals with temples denies them the core of the market and marks their offerings as deviations that families must justify to relatives.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, alternative_ritual_entrants, excluded,
    moderate, immediate, mobile, national).

% Historians and ethnographers of Japanese religion who document how the division arose, how the Meiji state imposed separation, and whether contemporary practice reflects living commitment or inherited habit. They take no side in the arrangement and bear none of its costs; their analyses are the principal outside check on the institutions' self-descriptions and on the flattering origin stories either clergy might tell.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, scholars_of_japanese_religiosity, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__partition_reading, temple_clergy).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles jurisdictional competition between two comprehensive ritual systems sharing one society: assigns life-passage rites to shrines and death rites to temples, so households obtain complete life-cycle coverage without the two institutions bidding against each other for the same occasions, and without either absorbing the other.
% TRANSFER_FUNCTION: Moves offerings, ceremony fees, funeral and memorial payments, and grave-maintenance dues from lay households to shrine and temple institutions; moves customary monopoly over death rites to the temples and over life-passage rites to the shrines; moves the burden of boundary administration to the two clergy groups jointly.
% ABSENT_VOICES: Religious minorities, secularists, and commercial or non-religious ritual providers were never seated when the division crystallized; they would object that the settled tracks tax every option outside them. Historically, the Buddhist establishment also had no voice when the Meiji state dictated separation by decree — the arrangement's founding terms were set unilaterally, and the excluded seats of that moment are the ancestors of today's excluded seats.
% DISAPPEARANCE_RATIONALE: If the partition vanished overnight, households would face unresolved jurisdictional competition at every life passage — which institution buries whom, at what price, with what rite — and the funeral economy would rearrange rapidly around price and preference as secular entrants moved into the core market. Clergy incomes on both sides would shift sharply; cemetery custom and danka relationships, currently anchored to the division, would unwind over a generation.
% FOUNDING_PROBLEM: From the medieval period onward, kami worship and Buddhism coexisted in Japan and repeatedly collided — over ritual precedence, doctrinal supremacy (the honji-suijaku debates and their inversions), and material patronage. The Meiji state resolved the collision by decreeing separation and destroying the integrated arrangement; the customary partition consolidated afterward as the working settlement of that older rivalry.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: Meiji government records (the Dajokan separation edicts and Home Ministry administration) attest that the separation was imposed top-down rather than negotiated, and academic scholarship (Breen and Teeuwen on the constructed history of Shinto; Reader and colleagues on contemporary practice) documents both the imposed origins and the dispute over whether the arrangement still performs a coordinating function or persists as habit. The shrine and temple institutions themselves attest the arrangement's continuing necessity, but that is self-interested testimony from the benefiting set; no corroborating source outside it claims the founding rivalry remains live.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__partition_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.32 at interval end): most of what households pay is payment for services actually rendered (funerals, graves, memorial observances), but a captive component persists in danka relationships, where the family grave ties the household to its temple and funeral pricing faces little intra-local competition. Suppression is low (0.22) and its trajectory is the story's spine: the Meiji state built heavy enforcement (separation edicts, haibutsu kishaku destruction of temples, compulsory shrine rites) and the occupation-era reforms dismantled it, leaving only customary and familial pressure. Theater rose sharply at the occupation juncture (State Shinto's ideological seriousness collapsed into cultural performance — hatsumode by the non-believing majority) and has partially settled as secular entrants force temples to deliver real service. Accessibility collapse is moderate (0.40): alternatives exist and grow (Christian weddings, secular funerals, joint graves, ash scattering) but the customary default exerts real pull. Resistance is low-moderate (0.28): periodic public criticism of temple fees, growth of non-religious funerals, family disputes over graves. The claim is authored independently: I claim ROPE because the arrangement solves a genuine collective-action problem (jurisdictional settlement between two comprehensive ritual systems), leaves participants net beneficiaries, imposes minimal coercive overhead in its standing form, and does not suppress alternatives — while the metrics above are my best descriptive estimates, and the engine computes per-seat classifications from the structural data. All three tracked series run on one shared seven-point grid so no metric row borrows another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the clergy seats, the partition is a legitimate vocational settlement: it secures each side's ritual market, spares shrines death-pollution duties their own doctrines discourage, and gives temples the funerary economy that sustains parish life. From the lay-household seat, the same structure is a convenient but costly default — complete coverage purchased with fees and inherited danka obligation. From the minority and entrant seats, it is an exclusionary bargain they were never party to, taxing every rite that does not fit the two tracks. Same arrangement, four experienced realities; the engine computes this divergence from power, exit, and declared position rather than from any authored verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The two clergy groups sit near the beneficiary end: protected spheres, identity-locked into their vocations, collecting the arrangement's institutional receipts. Lay households are declared beneficiaries but carry a payer secondary role and constrained exit (the family grave binds them), pulling their derived d toward symmetric — they are net beneficiaries whose benefit is partly offset by captive pricing. Religious minority practitioners are the declared victims: they bear friction costs on every life-passage rite, though their mobility damps d below the trapped-target range — this is exclusion with exit, not confiscation. Alternative ritual entrants sit mid-to-high: barred by custom from the core of the death market but able to operate at its margins. No directionality overrides are authored: the derivation from declared positions plus exit options captures these relationships without correction. Suppression, as a raw structural property, is not scaled by power or scope; only extractiveness is scaled, and the household seat's national scope modestly amplifies its effective extraction relative to its base epsilon.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — violent jurisdictional rivalry between kami worship and Buddhism, resolved top-down by Meiji state decree — is substantially dead: no institutional actor today contests the boundary by force, and the state that imposed it has dissolved. Yet the arrangement persists on a mixed fuel of custom and residual genuine demand: funerals must happen somewhere, graves bind descendants, and the two-track itinerary spares households a negotiation they would otherwise face. This is why the classification matters in both directions. Reading the partition as pure extraction (snare) erases the real coordination service and the household benefit; reading it as frictionless rope ignores the captive danka economics and the minority friction the victim declaration records. The open question is drift: if household uptake continues to hollow into pure inheritance, the arrangement slides toward piton — maintained theatrically by two clergy establishments whose successor pipelines are failing — while if the coordination remains live, the rope claim holds. Omega variables living_coordination_vs_inertial_custom and commitment_status_of_functional_division carry that uncertainty explicitly rather than resolving it by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_integration_axis,
    'This constraint is one reading of the shinbutsu_ontological_commitment kernel. Does the standing arrangement instantiate a deliberate domain-partition commitment (this reading), a unified cosmological order under honji-suijaku (syncretic sibling), or no stable commitment at all (incoherence sibling)?',
    'Comparative institutional history: pre-Meiji honji-suijaku doctrinal records, the Meiji separation edicts, and postwar practice surveys, read against each sibling''s predictive signature (doctrinal merger texts vs. jurisdictional settlements vs. ad hoc accommodation without normative content).',
    'If the syncretic sibling is correct of the pre-modern arrangement, this story''s interval misdates the constraint and its epsilon referent shifts to a different, integrated structure; if the incoherence sibling is correct, the coordination_function attributed here dissolves and classification drifts toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_integration_axis, conceptual, 'Which reading of the shinbutsu kernel the standing arrangement actually instantiates.').

omega_variable(
    living_coordination_vs_inertial_custom,
    'Is the contemporary partition a living coordination that households actively rely on, or inertial custom maintained by default and inheritance?',
    'Longitudinal surveys of household ritual decision-making: whether families consciously select the customary tracks or simply inherit them; uptake rates of deliberately non-customary options (secular funerals, grave-free memorials, ash scattering).',
    'If inertia dominates, theater_ratio rises over subsequent intervals and the constraint drifts toward piton despite low extraction; if the coordination is live, the rope claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_coordination_vs_inertial_custom, empirical, 'Whether the partition coordinates living practice or merely persists by habit.').

omega_variable(
    danka_fee_service_or_rent,
    'Are temple funeral and memorial fees priced at service cost, or do captive danka (parishioner) relationships sustain a rent premium above competitive rates?',
    'Regional price comparison across areas with varying temple density and danka lock-in; disclosure or estimation of temple cost structures; the migration rate toward cheaper secular substitutes as a revealed-preference test.',
    'A large rent component raises effective extraction for the payer seats and could push the computed classification toward tangled_rope despite the partition''s genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(danka_fee_service_or_rent, empirical, 'Whether death-sphere pricing tracks cost or captive-market rent.').

omega_variable(
    residual_suppression_mechanism,
    'Is the remaining conformity pressure around the customary tracks structural (family and community sanction, cemetery custom, ancestral-grave obligation) or internalized (conviction that proper deaths require Buddhist rites)?',
    'Post-exit trajectory of households that chose secular funerals or grave-free memorials: whether felt obligation and family friction persist after leaving the customary track.',
    'If internalized, effective suppression exceeds the structural measure and travels with households even as institutional enforcement stays low; if structural, further liberalization of cemetery and funeral regulation would collapse it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_suppression_mechanism, empirical, 'Structural vs. internalized source of the partition''s residual conforming pressure.').

omega_variable(
    commitment_status_of_functional_division,
    'Does the stable division of ritual domains constitute a commitment at all, or is it what the incoherence sibling calls tolerated inconsistency wearing institutional habit?',
    'Test for commitment-markers: explicit boundary assertions by clergy, sanction of boundary-crossing (a shrine performing funerals, a temple claiming life-passage rites), transmission of the division as a norm to ordained successors, versus mere unreflective repetition.',
    'If no commitment exists, this reading collapses into the incoherence sibling and the coordination_function attributed here is spurious; if the markers are present, the partition reading holds against the incoherence sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commitment_status_of_functional_division, conceptual, 'Whether the functional division is a normative commitment or mere tolerated inconsistency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 0, 157).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(shin_tr_t0, observed).
narrative_ontology:measurement(shin_tr_t26, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 26, 0.18).
narrative_ontology:measurement_basis(shin_tr_t26, observed).
narrative_ontology:measurement(shin_tr_t52, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 52, 0.22).
narrative_ontology:measurement_basis(shin_tr_t52, observed).
narrative_ontology:measurement(shin_tr_t78, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 78, 0.3).
narrative_ontology:measurement_basis(shin_tr_t78, observed).
narrative_ontology:measurement(shin_tr_t105, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 105, 0.34).
narrative_ontology:measurement_basis(shin_tr_t105, observed).
narrative_ontology:measurement(shin_tr_t131, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 131, 0.33).
narrative_ontology:measurement_basis(shin_tr_t131, observed).
narrative_ontology:measurement(shin_tr_t157, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 157, 0.3).
narrative_ontology:measurement_basis(shin_tr_t157, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(shin_be_t0, observed).
narrative_ontology:measurement(shin_be_t26, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 26, 0.28).
narrative_ontology:measurement_basis(shin_be_t26, observed).
narrative_ontology:measurement(shin_be_t52, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 52, 0.33).
narrative_ontology:measurement_basis(shin_be_t52, observed).
narrative_ontology:measurement(shin_be_t78, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 78, 0.3).
narrative_ontology:measurement_basis(shin_be_t78, observed).
narrative_ontology:measurement(shin_be_t105, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 105, 0.38).
narrative_ontology:measurement_basis(shin_be_t105, observed).
narrative_ontology:measurement(shin_be_t131, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 131, 0.36).
narrative_ontology:measurement_basis(shin_be_t131, observed).
narrative_ontology:measurement(shin_be_t157, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 157, 0.32).
narrative_ontology:measurement_basis(shin_be_t157, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(shin_su_t0, observed).
narrative_ontology:measurement(shin_su_t26, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 26, 0.7).
narrative_ontology:measurement_basis(shin_su_t26, observed).
narrative_ontology:measurement(shin_su_t52, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 52, 0.65).
narrative_ontology:measurement_basis(shin_su_t52, observed).
narrative_ontology:measurement(shin_su_t78, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 78, 0.55).
narrative_ontology:measurement_basis(shin_su_t78, observed).
narrative_ontology:measurement(shin_su_t105, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 105, 0.3).
narrative_ontology:measurement_basis(shin_su_t105, observed).
narrative_ontology:measurement(shin_su_t131, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 131, 0.24).
narrative_ontology:measurement_basis(shin_su_t131, observed).
narrative_ontology:measurement(shin_su_t157, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 157, 0.22).
narrative_ontology:measurement_basis(shin_su_t157, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, resource_allocation).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'shinbutsu-shugo' (Shinto-Buddhist syncretism) conflates three structurally distinct claims about the same historical relationship: cosmological unification (syncretic reading), functional domain partition without ontological integration (this file), and institutionally tolerated incoherence (incoherence reading). Each has its own epsilon, beneficiary structure, and failure modes; per the epsilon-invariance principle they are authored as separate stories linked through affects_constraints. The syncretic reading is upstream and historically prior (medieval doctrinal elaboration); this partition reading governs the post-separation standing arrangement, whose shape was fixed by the destruction of the upstream synthesis rather than by negotiation among the parties now governed by it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
