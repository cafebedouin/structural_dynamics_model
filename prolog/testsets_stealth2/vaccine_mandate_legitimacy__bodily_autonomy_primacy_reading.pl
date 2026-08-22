% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Absolute Bodily Sovereignty Rule (Bodily-Autonomy-Primacy Reading of Vaccine Mandate Legitimacy)
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   vaccine_mandate_legitimacy kernel: the bodily-autonomy-primacy reading,
 *   under which medical self-sovereignty is absolute and state medical
 *   coercion is categorically impermissible regardless of outcome. The
 *   constraint is the categorical rule itself as an operative constraint on
 *   public health authority — enforced by constitutional courts, mobilized by
 *   liberty advocacy movements, relied on by vaccine objectors. Its
 *   structural signature, per this reading's own lights: a genuine
 *   bright-line coordination function (a non-negotiable bodily-integrity line
 *   no majority can game in a crisis, descended from the Nuremberg-Code
 *   informed-consent lineage) carrying an asymmetric extraction (the
 *   immunocompromised, congregate-care elderly, and medically fragile bear
 *   the transmission exposure that population-level mandates would have
 *   suppressed, and cannot exit their exposure). The epsilon referent follows
 *   the kernel-reading rule: the standing arrangement under contest is the
 *   mandate-legitimacy arrangement this rule governs, assessed by this
 *   reading's own lights — and this reading's lights do NOT whitewash its own
 *   operation. The structural delta's victims are counted, so epsilon sits
 *   substantially above zero rather than at the near-zero an idealized
 *   self-assessment would author. Sibling readings share the referent and
 *   author different epsilon and inverted victim sets; they are separate
 *   constraints, linked in network.affects_constraints, not folded into this
 *   one. The claimed type and the metrics are authored independently: the
 *   claim from structural belief, the metrics from what the operation
 *   descriptively shows.
 *
 * KEY AGENTS:
 *   - liberty_advocacy_movements: primary beneficiary (organized / identity_locked) — collects doctrinal precedent, membership, and funding each time the categorical line holds
 *   - vaccine_refusing_objectors: secondary beneficiary (moderate / constrained) — protected refusal is the rule's direct output; compliance remains available at identity cost
 *   - religious_conscience_objectors: secondary beneficiary (moderate / identity_locked) — protected by the bright line without case-by-case exemption hearings
 *   - immunocompromised_patients: primary target (powerless / trapped) — bears unmandated transmission exposure it cannot shed by vaccination, wealth, or relocation
 *   - congregate_care_elderly: concentrated victim (powerless / trapped) — facility-scale exposure with no exit
 *   - medically_fragile_children: concentrated victim (powerless / trapped) — protection depends entirely on other people's voluntary choices
 *   - public_health_agencies: constrained cost-bearer (institutional / constrained) — loses the mandate instrument, keeps the outbreak duty, bears the residual mortality
 *   - constitutional_courts: agenda setter (institutional / constrained) — draws and holds the categorical line; collects no rents; cannot exit the docket
 *   - bioethics_analysts: analytical observer (analytical / analytical) — sees the deontological force and the distributional incidence simultaneously
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.58).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.55).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Absolute Bodily Sovereignty Rule (Bodily-Autonomy-Primacy Reading of Vaccine Mandate Legitimacy)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '8871e5b6-b9de-49cc-8655-18276389fb18').
narrative_ontology:cs_kernel_codification('8871e5b6-b9de-49cc-8655-18276389fb18', distributed).
narrative_ontology:cs_authority_grounding('8871e5b6-b9de-49cc-8655-18276389fb18', lineage).
narrative_ontology:cs_interpretation_layer_present('8871e5b6-b9de-49cc-8655-18276389fb18').
narrative_ontology:cs_reading_relation('8871e5b6-b9de-49cc-8655-18276389fb18', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('8871e5b6-b9de-49cc-8655-18276389fb18', vaccine_mandate_legitimacy__risk_stratification_reading, forecloses).
narrative_ontology:cs_axiom('8871e5b6-b9de-49cc-8655-18276389fb18', foundational, state_medical_coercion_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_medical_coercion_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('8871e5b6-b9de-49cc-8655-18276389fb18', state_medical_coercion_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('8871e5b6-b9de-49cc-8655-18276389fb18', secondary, informed_consent_nonwaivable_precondition).
narrative_ontology:cs_axiom_status(informed_consent_nonwaivable_precondition, holdable).
narrative_ontology:cs_axiom_grounding('8871e5b6-b9de-49cc-8655-18276389fb18', informed_consent_nonwaivable_precondition, deontological).
narrative_ontology:cs_reference_frame('8871e5b6-b9de-49cc-8655-18276389fb18', absolute_bodily_sovereignty_baseline).
narrative_ontology:cs_drift_state('8871e5b6-b9de-49cc-8655-18276389fb18', post_pandemic_mandate_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('8871e5b6-b9de-49cc-8655-18276389fb18', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_refusing_objectors).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, religious_conscience_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_patients).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, congregate_care_elderly).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medically_fragile_children).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_agencies).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, informed_consent_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, nuremberg_code_bodily_integrity_principle).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, constitutional_bodily_integrity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Litigate, lobby, and litmus-test candidates to keep the categorical no-coercion line in place. When mandates fall, they collect doctrinal precedent, membership, and funding; their organizations are constituted around the principle, so abandoning it would dissolve what they are. Exit would mean disbanding the movement's own identity.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, beneficiary,
    organized, generational, identity_locked, national).

% Decline vaccination on personal or philosophical grounds and rely on the categorical rule to keep their refusal protected against termination, exclusion, or fines. Compliance remains available as an exit, but it carries the identity and conscience cost that made them objectors in the first place.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_refusing_objectors, beneficiary,
    moderate, biographical, constrained, national).

% Refuse on doctrinal grounds their faith communities hold; the bright line protects them without case-by-case exemption hearings. Their exit is apostasy — structurally unavailable from where they stand.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, religious_conscience_objectors, beneficiary,
    moderate, biographical, identity_locked, national).

% Take immunosuppressive therapy or have conditions that prevent them from mounting vaccine responses; they depend on population immunity they cannot generate personally. When mandates are categorically barred, they bear the transmission exposure of everyday life — work, transit, care settings — with no exit from their immune status.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_patients, payer,
    powerless, biographical, trapped, national).

% Live in nursing homes and long-term care facilities where a single respiratory introduction spreads through the building. They depend on staff and visitor vaccination coverage they cannot control, and they cannot leave to avoid exposure.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, congregate_care_elderly, payer,
    powerless, biographical, trapped, national).

% Are too young for vaccination or have conditions that make it contraindicated. Their protection is decided entirely by the vaccination choices of the people around them and by the rules governing those choices; they have no independent means to purchase safety.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medically_fragile_children, payer,
    powerless, biographical, trapped, national).

% Hold statutory duties to control outbreaks but lose the mandate instrument under the categorical rule. They manage the resulting exposure with weaker tools — communication, voluntary campaigns, mitigation funding — and bear the outbreak and mortality costs the rule's operation leaves on their desks. They cannot exit their statutory duties.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_agencies, payer,
    institutional, generational, constrained, national).

% Draw and hold the categorical line: strike mandates, reject balancing tests, and refuse outcome-based justifications. They collect no revenue from the rule; their stake is doctrinal coherence and institutional authority. They cannot exit their docket when crisis politics return the question term after term.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Map the full structure across jurisdictions: the deontological force of the categorical line, its Nuremberg-lineage founding, and the distributional incidence of its costs on those who cannot vaccinate to safety. They publish, testify, and take no side in the enforcement contest.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, bioethics_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the boundary-drawing problem for state power over bodies with a single non-negotiable line — no involuntary medical intervention, ever — instead of case-by-case balancing that majorities can recalibrate in each crisis. The bright line protects unpopular minorities (today's refusers, tomorrow's whomever) from majoritarian health politics and removes the recurring cost of relitigating the boundary under emergency pressure.
% TRANSFER_FUNCTION: Moves disease-exposure risk from a protected background onto those who cannot generate or purchase protection personally — the immunocompromised, congregate-care elderly, and medically fragile — and converts the withdrawn population-level protection into categorical liberty for mandate objectors and accumulated doctrinal precedent for liberty advocacy movements.
% ABSENT_VOICES: The immunocompromised and congregate-care elderly object, and are not silent — disability-rights organizations litigate, patients testify, mortality data is published — but their objection carries no electoral or doctrinal weight. They are a small, dispersed, mortality-bearing class whose coalition power is structurally weak against a mobilized liberty movement and a doctrine whose categorical form is precisely what makes their balancing claim inadmissible. Their voices are present in the room and outvoted by design.
% DISAPPEARANCE_RATIONALE: If the categorical rule vanished overnight, mandate authority would revert to proportionality balancing: targeted mandates in high-risk settings would return first, employment conditions would follow, and the liberty movement would lose its doctrinal anchor and litigate from a defensive posture. The vulnerable would regain a protection they currently cannot purchase; objectors would face exemption hearings instead of a categorical shield; courts would trade a bright line for a permanent balancing docket.
% FOUNDING_PROBLEM: State and medical power historically conscripted bodies: nonconsensual experimentation, forced sterilization, and treatment imposed on the powerless. The categorical rule descends from the Nuremberg Code and the informed-consent lineage and was built to make body-conscription categorically impossible — a line that does not bend to any collective benefit, because every atrocity in the lineage was justified by one.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated from outside the liberty movement: medical historians document the Nuremberg and forced-sterilization records; international bioethics bodies maintain the informed-consent lineage; and — decisively — public health authorities themselves, the rule's institutional adversaries, attest that coercive capacity remains live by repeatedly asserting and exercising mandate power in crises. An adversary's continued pursuit of the power the rule blocks is corroboration that the threat is real; no source inside the benefiting parties is needed to establish it.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.58: the rule's operation withdraws population-level protection from a small class that cannot generate it personally and cannot exit exposure — concentrated, severe, mortal risk — while the rule's protective function for everyone else is real and valued by the reading itself; justification is not zero-extraction. Suppression 0.55 is structural, not personal: the categorical line forecloses the proportionality middle ground (targeted mandates) and the outcome-based instrument entirely, by doctrine rather than by force against persons; suppression is authored as a raw structural property and is not scaled by power or scope. Theater 0.22: enforcement is real judicial work, not ritual — the low theater is part of what distinguishes this constraint from inertial maintenance. Accessibility collapse 0.70: once the categorical line is entrenched, mandate-based alternatives collapse in court regardless of their design; non-coercive mitigations remain available, which keeps the collapse short of natural-law grade. Resistance 0.70: crisis legislatures, public health authorities, and employers continuously push to restore mandate authority; the rule survives only through active judicial enforcement (requires_active_enforcement: true). The measurement series share one grid (t=0 maps to the Nuremberg Code era, t=75 to the post-mandate-litigation present, roughly one unit per year): base extractiveness rises from 0.30 to 0.58 as vaccines make population-level mandates effective — the categorical foreclosure's opportunity cost grows with the tool it blocks; suppression_requirement rises from 0.25 to 0.55 as public-health authority expands and crises recur, so holding the line takes ever more active enforcement; theater stays low throughout (0.10 to 0.22) because enforcement is real work, not performance. No enforcement-decay or oscillation story is claimed; the flat-to-rising enforcement trajectory is the dynamic being traced.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the liberty-movement seat the rule is the last line against atrocity — a categorical protection whose value is precisely that it does not bend to outcomes. From the immunocompromised patient's seat the same rule is a veto they cannot appeal: their survival depends on other people's voluntary choices. From the constitutional court's seat it is a line that must be held or abandoned wholesale, because balancing is what the rule forbids. The engine derives these per-seat classifications from power, exit, and role data; the divergence between the movement's civilizational-triumph narrative and the patient's mortality exposure is the measurement this story exists to take. On reading relations: both sibling edges are authored as forecloses because this reading's core premise ('categorically impermissible regardless of outcome') directly contradicts each sibling's contingency premise — outcome-justified mandate authority and actuarial-threshold legitimacy are both things a holder of the categorical premise is logically committed to rejecting; no single framework holds both. This is the rare genuine foreclosure pair, not default caution. Both of this reading's axioms remain holdable: the categorical position is live in contemporary constitutional discourse, held by real courts and movements.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: liberty_advocacy_movements collect precedent and organizational capital, and their identity-locked exit means they will not abandon the rule that constitutes them; objectors collect protected refusal. Victims sit near the full-target end: immunocompromised patients, congregate-care elderly, and medically fragile children are trapped — their exposure cannot be exited by vaccination, wealth, or relocation, which places them at the trapped extreme rather than the constrained one. Public health agencies are constrained cost-bearers: they lose the instrument and keep the duty. Constitutional courts administer without collecting — no rents flow to the seat, placing them near symmetric despite holding the agenda. Scope is national: verification of the rule's operation (does a mandate survive review?) is centralized in courts, so the scope amplification on extraction is modest. No directionality overrides are needed: the beneficiary/victim declarations plus exit options produce the correct d for every seat, and the two institutional seats are differentiated by role rather than requiring an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. First, the false summit: the reading's deontological rhetoric ('categorical,' 'absolute,' 'self-evident') tempts a natural-law framing of what is in fact a constructed, enforced, contested norm. The story refuses it: emerges_naturally is false, the rule requires active judicial enforcement, and it meets heavy resistance — it is a maintained human arrangement, not a law of nature, and no beneficiaries are smuggled in under a mountain claim. Second, the pure-extraction flattening: a snare reading would erase the rule's genuine founding function — the bright line against state medical atrocity, whose founding problem (forced sterilization, nonconsensual experimentation) is live and corroborated by adversaries. The hybrid classification keeps both faces on the table: real coordination, real victims, same structure. The R5 interview shows no mandatrophy: the founding problem is live, so this is not an atrophied mandate kept alive by performance — the theater ratio stays low and the founding-problem status is live with adversary corroboration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the bodily_autonomy_primacy_reading of the vaccine_mandate_legitimacy kernel — what structurally changes if a sibling reading is operative instead? Under public_health_primacy, the immunocompromised move from this constraint''s victim set to its beneficiary set (mandate authority protects them) and liberty advocacy movements move from beneficiaries to constrained parties; under risk_stratification, the victim set splits at an actuarial line (low-risk objectors coerced by blanket mandates, high-risk protected by targeted ones).',
    'Adoption is decided by courts and constitutional entrenchment; the corpus''s sibling stories fix the shared referent so the three victim sets and epsilon values can be compared reading against reading.',
    'Which reading is operative determines who counts as the constraint''s victims — the inversion between this story and the public-health sibling is the kernel''s central structural contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling adoption inverts the victim set.').

omega_variable(
    epsilon_referent_resolution,
    'Does epsilon measure this categorical rule''s own extraction from the victims its operation creates (as authored here: the reading''s own lights count the structural delta''s victims, so 0.58), or the contested mandate regime as this reading categorically condemns it (which would push epsilon toward 0.9)?',
    'Cross-reading comparison once the sibling stories compile on the shared referent: the corpus fixes whether reading-indexed epsilon is authored over the rule-in-operation or over the regime the rule contests.',
    'At 0.58 the rule profiles as a hybrid with real victims; at 0.9 under this reading''s condemnatory framing the seat classifications shift toward pure-extraction profiles and the per-seat divergence changes sign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_resolution, conceptual, 'Referent ambiguity for a reading whose constraint is itself a normative rule.').

omega_variable(
    justified_extraction_status,
    'Does the rule''s deontological justification — bodily integrity as inviolable — extinguish the extraction its victims bear, or does justified extraction remain extraction?',
    'Framework-level ruling on whether a constraint''s moral justification zeroes its extraction accounting; the reading''s own lights here say it does not (the delta''s victims are counted), but the opposing view is coherent.',
    'If justification extinguishes extraction, the rule computes as pure coordination protecting everyone; if it does not (as authored), the rule is a hybrid whose victims are real regardless of its justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justified_extraction_status, conceptual, 'Whether deontological justification zeroes the victim accounting of a rights-protecting constraint.').

omega_variable(
    mitigation_substitutability,
    'Can non-coercive mitigations — ventilation standards, paid sick leave, antiviral access, targeted voluntary campaigns for high-exposure settings — close most of the exposure gap the categorical rule opens for those who cannot vaccinate to safety?',
    'Comparative jurisdictional data: outcomes for immunocompromised and congregate-care populations in categorical-rule jurisdictions with robust mitigation funding versus those without.',
    'If mitigations close most of the gap, the rule''s victim cost is a policy choice layered on a rights structure and extraction drops toward pure coordination; if they do not, the extraction is structural — the rule''s operation itself concentrates mortal risk on the trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_substitutability, empirical, 'Whether the victim cost of the categorical rule is substitutable by voluntary means.').

omega_variable(
    movement_identity_lock_direction,
    'Is the liberty advocacy movement''s commitment to the categorical rule identity-fused (the principle constitutes the organizations, so exit is unthinkable) or instrumental (abandonable if categorical positions demonstrably harm the movement''s own constituents)?',
    'Observe movement behavior across episodes where categorical victories produce outbreaks harming refusers themselves: identity-fused movements double down; instrumental ones pivot toward risk-stratified positions.',
    'Identity fusion keeps enforcement pressure constant regardless of outcomes (current authoring); instrumentality would let the rule decay toward inertial maintenance if alignment breaks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(movement_identity_lock_direction, empirical, 'Whether the enforcing movement''s exit from the categorical position is identity-locked or instrumental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmleg_bap_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(vmleg_bap_tr_t0, observed).
narrative_ontology:measurement(vmleg_bap_tr_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(vmleg_bap_tr_t15, observed).
narrative_ontology:measurement(vmleg_bap_tr_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(vmleg_bap_tr_t30, observed).
narrative_ontology:measurement(vmleg_bap_tr_t45, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 45, 0.18).
narrative_ontology:measurement_basis(vmleg_bap_tr_t45, observed).
narrative_ontology:measurement(vmleg_bap_tr_t60, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement_basis(vmleg_bap_tr_t60, observed).
narrative_ontology:measurement(vmleg_bap_tr_t75, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 75, 0.22).
narrative_ontology:measurement_basis(vmleg_bap_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(vmleg_bap_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(vmleg_bap_be_t0, observed).
narrative_ontology:measurement(vmleg_bap_be_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement_basis(vmleg_bap_be_t15, observed).
narrative_ontology:measurement(vmleg_bap_be_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement_basis(vmleg_bap_be_t30, observed).
narrative_ontology:measurement(vmleg_bap_be_t45, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 45, 0.52).
narrative_ontology:measurement_basis(vmleg_bap_be_t45, observed).
narrative_ontology:measurement(vmleg_bap_be_t60, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 60, 0.57).
narrative_ontology:measurement_basis(vmleg_bap_be_t60, observed).
narrative_ontology:measurement(vmleg_bap_be_t75, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 75, 0.58).
narrative_ontology:measurement_basis(vmleg_bap_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(vmleg_bap_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(vmleg_bap_su_t0, observed).
narrative_ontology:measurement(vmleg_bap_su_t15, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement_basis(vmleg_bap_su_t15, observed).
narrative_ontology:measurement(vmleg_bap_su_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 30, 0.36).
narrative_ontology:measurement_basis(vmleg_bap_su_t30, observed).
narrative_ontology:measurement(vmleg_bap_su_t45, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 45, 0.43).
narrative_ontology:measurement_basis(vmleg_bap_su_t45, observed).
narrative_ontology:measurement(vmleg_bap_su_t60, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement_basis(vmleg_bap_su_t60, observed).
narrative_ontology:measurement(vmleg_bap_su_t75, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 75, 0.55).
narrative_ontology:measurement_basis(vmleg_bap_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'vaccine mandate legitimacy' covers three structurally distinct claims that share one referent (the mandate-legitimacy arrangement under contest) and diverge in epsilon and victim structure. This story instantiates the categorical reading: the bright-line rule itself, whose operation benefits liberty advocacy movements and objectors and victimizes those who cannot vaccinate to safety. The public-health-primacy sibling inverts the victim set (immunocompromised as protected beneficiaries; objectors as externality sources); the risk-stratification sibling splits it at an actuarial line. The upstream claim (the informed-consent/Nuremberg lineage) is cited as evidence by this reading and contested by the public-health sibling. All family members link via affects_constraints; epsilon is not averaged across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
