% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__contextual_supersession_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem (Deuteronomy 7) Read as Historically-Bounded, Superseded Directive
 *   domain: religious/ethical/hermeneutical
 *
 * SUMMARY:
 *   This story instantiates the contextual-supersession reading of the herem
 *   command in Deuteronomy 7: the claim that the command to devote the
 *   Canaanite nations to destruction was a directive bounded to ancient
 *   Israel's settlement period, later morally corrected by prophetic
 *   universalism and Christian covenant theology. Under this reading, the
 *   command carries no operative force today — it is a historical artifact
 *   whose moral content has been superseded, not a timeless mandate and not
 *   merely an allegory for internal spiritual struggle. This is a scaffold:
 *   the reading's own logic requires a sunset (the settlement period ended;
 *   the mandate expired with it), and it coordinates continued reverence for
 *   the text with disavowal of its violent content. The relatively low
 *   extraction reflects that most people living under traditions holding this
 *   reading experience no live constraint on intermarriage or association
 *   with outsiders; the residual extraction is concentrated entirely in
 *   separatist communities that reject the reading and enforce the older
 *   exclusionary norm internally.
 *
 * KEY AGENTS:
 *   - prophetic_universalist_theologians: institutional agenda-setters who administer and teach the reading
 *   - interfaith_and_intermarried_believers: primary beneficiaries of the constraint's removal
 *   - mainline_denominational_institutions: institutional beneficiaries preserving canonical legitimacy
 *   - believers_under_residual_separatist_enforcement: narrow victim class where the older reading persists coercively
 *   - durable_separation_congregations: excluded objecting party, sibling-reading holders
 *   - biblical_scholars_of_ancient_near_eastern_warfare: analytical observers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.22).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.35).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, scaffold).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem (Deuteronomy 7) Read as Historically-Bounded, Superseded Directive").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "religious/ethical/hermeneutical").

narrative_ontology:has_sunset_clause(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, 'ddd28819-220d-422a-8bd1-32cb4ed77026').
narrative_ontology:cs_kernel_codification('ddd28819-220d-422a-8bd1-32cb4ed77026', fixed_text).
narrative_ontology:cs_authority_grounding('ddd28819-220d-422a-8bd1-32cb4ed77026', lineage).
narrative_ontology:cs_interpretation_layer_present('ddd28819-220d-422a-8bd1-32cb4ed77026').
narrative_ontology:cs_reading_relation('ddd28819-220d-422a-8bd1-32cb4ed77026', herem_command_dt7__durable_separation_reading, coexists_with).
narrative_ontology:cs_reading_relation('ddd28819-220d-422a-8bd1-32cb4ed77026', herem_command_dt7__allegorical_displacement_reading, influences).
narrative_ontology:cs_axiom('ddd28819-220d-422a-8bd1-32cb4ed77026', foundational, herem_mandate_temporally_bounded_to_settlement_period).
narrative_ontology:cs_axiom_status(herem_mandate_temporally_bounded_to_settlement_period, holdable).
narrative_ontology:cs_axiom_grounding('ddd28819-220d-422a-8bd1-32cb4ed77026', herem_mandate_temporally_bounded_to_settlement_period, conventional).
narrative_ontology:cs_axiom('ddd28819-220d-422a-8bd1-32cb4ed77026', foundational, prophetic_universalism_morally_supersedes_conquest_ethic).
narrative_ontology:cs_axiom_status(prophetic_universalism_morally_supersedes_conquest_ethic, holdable).
narrative_ontology:cs_axiom_grounding('ddd28819-220d-422a-8bd1-32cb4ed77026', prophetic_universalism_morally_supersedes_conquest_ethic, instrumental).
narrative_ontology:cs_reference_frame('ddd28819-220d-422a-8bd1-32cb4ed77026', settlement_era_bounded_mandate).
narrative_ontology:cs_drift_state('ddd28819-220d-422a-8bd1-32cb4ed77026', post_holocaust_ethical_reassessment, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('ddd28819-220d-422a-8bd1-32cb4ed77026', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, prophetic_universalist_theologians).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, interfaith_and_intermarried_believers).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, mainline_denominational_institutions).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, believers_under_residual_separatist_enforcement).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, moral_progressive_revelation_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, covenant_supersession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach and publish that herem was a time-and-place-bound command tied to ancient Israel's settlement crisis, later morally corrected by prophetic universalism (Isaiah, Jonah) and Christian covenant theology. They administer the interpretive framework in seminaries and denominational curricula, and their scholarly and pastoral authority depends on the supersession reading being accepted as the responsible one.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, prophetic_universalist_theologians, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, prophetic_universalist_theologians, beneficiary).

% Marry, worship, and build families across ethnic and religious lines within traditions that hold this reading. The historically-bounded framing removes any live prohibition against intermarriage or association with outsiders that a literal herem reading would impose, relocating the relevant boundary to consent and shared belief rather than ancestry.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, interfaith_and_intermarried_believers, beneficiary,
    moderate, biographical, mobile, national).

% Rely on the supersession reading to maintain public legitimacy, ecumenical partnerships, and interfaith dialogue programs. A literal or durable-mandate reading of herem would be reputationally costly and legally fraught; this reading lets the institution retain the text as scripture while disclaiming its violent content as operative.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, mainline_denominational_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Belong to smaller separatist or fundamentalist congregations that reject the supersession reading and enforce endogamy or shunning of outsiders using herem-adjacent texts as durable mandate. From inside those congregations, exit costs family, community, and identity; the supersession reading exists in the wider religious culture but does not reach them directly — it operates as external pressure and occasional exit-ramp rather than internal correction.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, believers_under_residual_separatist_enforcement, payer,
    powerless, biographical, constrained, local).

% Hold that herem encodes a timeless mandate for bounded identity and reject the supersession framing as capitulation to secular universalism. They are not part of the interpretive consensus this reading represents and would object that supersession abandons the text's authority; their objection is visible in religious print and debate but does not alter how mainline institutions teach the passage.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, durable_separation_congregations, excluded,
    organized, generational, constrained, national).

% Study herem alongside comparable Ancient Near Eastern conquest-devotion practices (e.g., the Mesha Stele) to establish the historical genre and function of the command, independent of any confessional reading. Their work is cited by all three sibling readings to different ends.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, biblical_scholars_of_ancient_near_eastern_warfare, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__contextual_supersession_reading, diffuse).
narrative_ontology:fixing_cost_class(herem_command_dt7__contextual_supersession_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared interpretive settlement allowing communities that retain the Hebrew Bible as scripture to disavow its literal violence and exclusionary content as a live norm, while preserving the text's canonical status and continuity with the broader covenant narrative.
% TRANSFER_FUNCTION: Moves interpretive authority and moral legitimacy away from a literal-mandate reading of herem toward institutions and scholars who narrate the command as historically bounded; the practical effect is that the intermarriage and separation constraints herem would impose are lifted for most adherents, while communities that reject the reading retain and enforce them internally.
% ABSENT_VOICES: Durable-separation congregations and the descendants of peoples symbolically or historically identified with the 'nations' targeted by herem are not party to the mainline theological consensus; the former object that supersession dissolves scriptural authority, the latter are rarely consulted on how the text should be taught at all.
% DISAPPEARANCE_RATIONALE: If the supersession reading disappeared, mainline institutions would lose their primary tool for retaining herem passages as scripture without operative violence, forcing either an allegorical retreat or an uncomfortable confrontation with literal reading; separatist congregations that never held the reading would be unaffected. Whether the 'world rearranges' therefore depends entirely on which population is asked.
% FOUNDING_PROBLEM: Modern communities holding the Hebrew Bible as authoritative scripture needed a way to affirm the text's canonicity while disavowing its literal command to exterminate named ethnic groups — a live moral problem once herem was read outside its ancient conquest-narrative setting and placed alongside modern norms against ethnic violence.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religion scholars and historians of biblical reception (working outside any confessional beneficiary institution) corroborate that mainstream Jewish and Christian traditions converged on historical-bounding or allegorization specifically in response to post-Enlightenment and post-Holocaust ethical pressure on texts of ethnic violence — this is documented independently of the theologians who now teach the reading, though the theologians themselves are also the primary corroborating voices for its continued necessity.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, contested).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).
:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) and falling over the interval because this reading's entire function is to relocate the herem command's constraint from a live ethnic/marital boundary to a closed historical episode — most adherents experience zero operative extraction. The residual extraction figure is carried entirely by the payer stakeholder group (believers under residual separatist enforcement), who experience the older reading's force despite living within the wider religious culture that has moved past it. Suppression (0.35) and accessibility_collapse (0.3) are moderate-low: this reading does not require coercive enforcement to hold — it persists through scholarly and institutional consensus-building, not force — but it does compete against the durable-separation reading for interpretive territory, which generates real resistance (0.4) from congregations that reject it. Theater ratio rises over the interval (0.2 to 0.45) reflecting that as the supersession reading has become institutionally dominant in mainline settings, an increasing share of its maintenance activity is performative reaffirmation (academic conferences, denominational statements) rather than active pastoral work addressing the shrinking population still under the older reading's force.
 *
 * DIRECTIONALITY LOGIC:
 *   Prophetic universalist theologians and mainline institutions sit near the beneficiary end: they administer the reading and derive institutional legitimacy, ecumenical standing, and pedagogical continuity from it. Interfaith and intermarried believers are direct beneficiaries — the reading removes what would otherwise be a live scriptural objection to their unions. The narrow victim class (believers under residual separatist enforcement) sits near the target end precisely because the supersession reading does NOT reach them — they remain governed by a durable-mandate reading enforced within their own congregations, and the wider culture's adoption of supersession does little to relieve them without active exit, which is constrained by community and family cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabelings. First, against treating the reading itself as an active extraction mechanism — it is not: it removes rather than imposes a constraint on the general population, so classifying it as a snare would misdescribe a scaffold whose declared function (transitioning communities past a historically superseded mandate) is precisely to retire an older extractive structure. Second, against treating supersession as having fully solved its founding problem: the founding problem (reconciling scriptural authority with disavowal of ethnic violence) remains live wherever durable-separation congregations persist, so the sunset this reading declares for herem's operative force is a sunset for MOST adherents, not a universal resolution — the residual victim class shows the mandate has not actually expired everywhere it is nominally superseded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supersession_vs_allegorization_boundary,
    'Is ''moral supersession'' (this reading) genuinely distinct from ''allegorical displacement'' (the sibling reading), or do they converge in practice once both deny the command''s current operative force?',
    'Compare how each reading''s adherents handle a hypothetical modern-day herem-adjacent command: supersession would say ''it applied then and now the moral standard has advanced past it''; allegorization would say ''it never was about that referent.'' Survey adherent responses to test which framework they actually reason from.',
    'If the two readings converge functionally despite differing premises, the kernel''s three-way contest collapses to two live positions rather than three, changing how contamination propagates between them in the network model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supersession_vs_allegorization_boundary, conceptual, 'Whether supersession and allegorization are functionally distinct readings or converge in practice.').

omega_variable(
    progressive_revelation_doctrine_status,
    'Does the doctrine of progressive moral revelation (used to ground supersession) itself rest on a defensible theological premise, or is it a post-hoc device adopted specifically to neutralize embarrassing texts?',
    'Trace the doctrine''s textual and historical roots (e.g. appeals to Isaiah, Micah, the New Testament) versus its documented emergence and intensified use in post-Enlightenment and post-Holocaust theological literature specifically addressing herem and related texts.',
    'If the doctrine is substantially a post-hoc device, the supersession reading''s claim to derive from internal scriptural logic (prophetic correction) weakens relative to being an external ethical import; this would not change the reading''s practical function but would affect its claimed grounding_type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(progressive_revelation_doctrine_status, conceptual, 'Whether progressive revelation is internally grounded or an externally motivated interpretive device.').

omega_variable(
    residual_enforcement_measurement,
    'How large is the population actually living under durable-separation enforcement that supersession fails to reach, and is it growing or shrinking relative to mainline adoption of supersession?',
    'Denominational membership and defection data from separatist/fundamentalist congregations that explicitly hold the durable-separation reading, compared against mainline denomination membership trends over the same interval.',
    'A growing separatist population would indicate the supersession reading''s dominance is not increasing net protection for the victim class even as it becomes more institutionally entrenched elsewhere — decoupling institutional theater growth from real protective effect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_enforcement_measurement, empirical, 'Size and trend of the population still under durable-mandate enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(here_tr_t0, observed).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__contextual_supersession_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(here_tr_t40, observed).
narrative_ontology:measurement(here_tr_t80, herem_command_dt7__contextual_supersession_reading, theater_ratio, 80, 0.34).
narrative_ontology:measurement_basis(here_tr_t80, observed).
narrative_ontology:measurement(here_tr_t120, herem_command_dt7__contextual_supersession_reading, theater_ratio, 120, 0.38).
narrative_ontology:measurement_basis(here_tr_t120, observed).
narrative_ontology:measurement(here_tr_t160, herem_command_dt7__contextual_supersession_reading, theater_ratio, 160, 0.42).
narrative_ontology:measurement_basis(here_tr_t160, observed).
narrative_ontology:measurement(here_tr_t200, herem_command_dt7__contextual_supersession_reading, theater_ratio, 200, 0.45).
narrative_ontology:measurement_basis(here_tr_t200, observed).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(here_be_t0, observed).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(here_be_t40, observed).
narrative_ontology:measurement(here_be_t80, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 80, 0.33).
narrative_ontology:measurement_basis(here_be_t80, observed).
narrative_ontology:measurement(here_be_t120, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 120, 0.27).
narrative_ontology:measurement_basis(here_be_t120, observed).
narrative_ontology:measurement(here_be_t160, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 160, 0.24).
narrative_ontology:measurement_basis(here_be_t160, observed).
narrative_ontology:measurement(here_be_t200, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 200, 0.22).
narrative_ontology:measurement_basis(here_be_t200, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(herem_command_dt7__contextual_supersession_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the herem_command_dt7 kernel (Deuteronomy 7's devotion-to-destruction command). durable_separation_reading claims a timeless mandate for bounded identity — its ε is authored high because it treats the exclusionary force as live and operative. allegorical_displacement_reading denies the command ever had a literal ethnic referent, relocating conflict to internal moral struggle — its ε is authored near-zero because no real-world population bears its extraction. This reading (contextual_supersession) sits between them: it affirms the historical literal referent but declares its moral force expired, yielding low-but-nonzero ε concentrated in the narrow population where the older reading persists by enforcement. All three share the same kernel text and are linked here per the network decomposition rule; contamination in one reading's legitimacy (e.g. historical evidence undermining the settlement-period dating) would structurally pressure this reading's reference_frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
