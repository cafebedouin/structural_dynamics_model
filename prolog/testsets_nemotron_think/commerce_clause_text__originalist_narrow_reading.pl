% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Commerce Clause Originalist Narrow Reading
 *   domain: constitutional/law/federalism
 *
 * SUMMARY:
 *   This constraint story captures the originalist narrow reading of the
 *   Commerce Clause (Article I, Section 8, Clause 3): 'Congress shall have
 *   Power ... To regulate Commerce with foreign Nations, and among the
 *   several States, and with the Indian Tribes.' The reading holds that
 *   'among the several States' means commerce that physically crosses state
 *   borders, plus the instrumentalities (channels, vehicles, transmission
 *   lines) of that crossing. It excludes intrastate production,
 *   manufacturing, and local activity — even when aggregated they
 *   substantially affect interstate markets. The reading claims Mountain
 *   status: the constitutional text is a fixed, natural-law-like limit on
 *   federal power. Beneficiaries are state governments (retaining police
 *   power) and anti-consolidation advocates. Victims are federal lawmakers
 *   seeking uniform national standards and parties harmed by interstate
 *   externalities the narrow reading blocks Congress from addressing. The
 *   claim/metric gap is deliberate: the reading CLAIMS mountain (fixed text)
 *   while metrics describe a constraint whose extractiveness from national
 *   coordination has varied dramatically across history — the engine measures
 *   that divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.35).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.2).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, mountain).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Commerce Clause Originalist Narrow Reading").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional/law/federalism").

domain_priors:emerges_naturally(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, '10adce6e-bd4b-46f9-9750-3601feb28c30').
narrative_ontology:cs_kernel_codification('10adce6e-bd4b-46f9-9750-3601feb28c30', fixed_text).
narrative_ontology:cs_authority_grounding('10adce6e-bd4b-46f9-9750-3601feb28c30', lineage).
narrative_ontology:cs_interpretation_layer_present('10adce6e-bd4b-46f9-9750-3601feb28c30').
narrative_ontology:cs_reading_relation('10adce6e-bd4b-46f9-9750-3601feb28c30', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('10adce6e-bd4b-46f9-9750-3601feb28c30', commerce_clause_text__substantial_effects_limited_reading, coexists_with).
narrative_ontology:cs_axiom('10adce6e-bd4b-46f9-9750-3601feb28c30', foundational, commerce_clause_text_fixes_scope_at_border_crossing).
narrative_ontology:cs_axiom_status(commerce_clause_text_fixes_scope_at_border_crossing, holdable).
narrative_ontology:cs_axiom_grounding('10adce6e-bd4b-46f9-9750-3601feb28c30', commerce_clause_text_fixes_scope_at_border_crossing, deontological).
narrative_ontology:cs_axiom('10adce6e-bd4b-46f9-9750-3601feb28c30', foundational, state_police_power_retained_over_intrastate_activity).
narrative_ontology:cs_axiom_status(state_police_power_retained_over_intrastate_activity, holdable).
narrative_ontology:cs_axiom_grounding('10adce6e-bd4b-46f9-9750-3601feb28c30', state_police_power_retained_over_intrastate_activity, deontological).
narrative_ontology:cs_reference_frame('10adce6e-bd4b-46f9-9750-3601feb28c30', founding_era_dual_sovereignty).
narrative_ontology:cs_drift_state('10adce6e-bd4b-46f9-9750-3601feb28c30', post_new_deal_expansion, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('10adce6e-bd4b-46f9-9750-3601feb28c30', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, federal_lawmakers_seeking_uniform_regulation).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, interstate_externality_affected_parties).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, constitutional_textualism).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, state_police_power_retention).
narrative_ontology:constraint_vindicates(commerce_clause_text__originalist_narrow_reading, enumerated_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain exclusive police power over intrastate economic activity under this reading. The narrow Commerce Clause shields state regulatory autonomy from federal preemption. They cannot exit the constitutional framework but operate within the reserved sphere this reading protects.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    institutional, generational, analytical, national).

% Advocacy groups, scholars, and political actors who oppose federal regulatory expansion. They invoke the narrow reading as a structural bulwark. Their exit is intellectual — they can shift frameworks — but their influence depends on the reading's doctrinal viability.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates, beneficiary,
    organized, biographical, mobile, national).

% Congress members and executive agencies who need national regulatory solutions for problems that cross state lines but involve intrastate activity (labor standards, environmental protection, civil rights). The narrow reading withholds the Commerce Clause authority they would use. Their exit is constrained — they can pursue state-by-state solutions or seek constitutional amendment, both costly.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, federal_lawmakers_seeking_uniform_regulation, payer,
    institutional, biographical, constrained, national).

% Communities and individuals harmed by pollution, labor exploitation, or market failures that originate in one state but damage others. The narrow reading blocks federal remedies. They have no effective exit — they cannot move to escape interstate externalities, and state-level remedies are structurally inadequate for cross-border harms.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, interstate_externality_affected_parties, payer,
    powerless, biographical, trapped, regional).

% Supreme Court justices who adopt originalist methodology. They administer the constraint by authoring opinions that enforce the narrow reading. Their role is to declare what the fixed text means; they do not collect rents but they set the doctrinal agenda that makes the constraint operative.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, originalist_justices, agenda_setter,
    institutional, generational, analytical, national).

% Academic observers who analyze the reading's textual, historical, and doctrinal coherence. They neither collect from nor pay into the constraint; they map the interpretive landscape and its practical consequences.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constitutional text coordinates federal-state boundaries by fixing the Commerce Clause scope to trade crossing state borders and instrumentalities of interstate movement, preventing federal encroachment on state police powers over intrastate activity.
% TRANSFER_FUNCTION: The arrangement allocates regulatory authority over intrastate economic activity to state governments, withholding it from the federal legislature. The 'transfer' is the regulatory capacity that remains at the state level versus moving to the federal level under a broader reading.
% ABSENT_VOICES: Residents of states with weak regulatory capacity who would benefit from federal floor-setting (e.g., minimum wage, environmental standards); future generations facing novel interstate externalities (climate change, digital platform effects, pandemic response) that the narrow reading's border-crossing framework cannot reach.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, Congress could regulate all economic activity with substantial aggregate effects on interstate commerce, fundamentally altering the federal-state balance and enabling national solutions to problems currently confined to state-level action.
% FOUNDING_PROBLEM: The Constitution needed to authorize federal regulation of genuinely interstate commerce — trade crossing state borders and the channels/instrumentalities of that trade — while preserving state autonomy over local economic activity and police powers.
% FOUNDING_PROBLEM_CORROBORATION: Founding-era sources (Federalist Nos. 42, 45; ratification debates) corroborate the dual-sovereignty design and the Commerce Clause's limited scope. However, the New Deal-era Court (NLRB v. Jones & Laughlin Steel, Wickard v. Filburn) and subsequent congressional practice corroborate a shifted understanding where the founding problem was substantially resolved by expanding federal power to meet national economic integration.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__originalist_narrow_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, ExtMetricName, E),
    domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(commerce_clause_text__originalist_narrow_reading),
    narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35 at interval end) reflects the reading's own assessment: the constitutional limit imposes real but bounded costs on national coordination — higher during periods of national crisis (New Deal, 0.6) and lower when state-level regulation suffices. Suppression is low (0.2) because the constraint is a textual limit, not an actively enforced prohibition; it operates through judicial review striking down federal laws, not through coercive machinery. Theater is low (0.15) — originalist justices genuinely treat the text as binding, not as performance. Accessibility collapse is high (0.85) — the textual argument leaves little room for alternative readings within the originalist framework. Resistance is low (0.2) from the reading's perspective: the text itself doesn't resist; competing readings resist the reading.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (states, anti-consolidation advocates) experience this as a Mountain — a fixed constitutional shield. The payer seats (federal lawmakers, externality victims) experience it as a Snare or Tangled Rope — an actively maintained barrier to needed regulation. The agenda-setter seat (originalist justices) experiences it as a genuine coordination function (textual fidelity) with modest performative overhead. The engine computes this seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and anti-consolidation advocates are structural beneficiaries (d near 0.0) — the constraint subsidizes their regulatory autonomy. Federal lawmakers are targets (d near 0.8) — the constraint extracts their preferred regulatory tools. Interstate externality victims are trapped targets (d near 1.0) — they bear costs with no exit. Originalist justices are agenda_setters with analytical exit (d ≈ 0.5, symmetric: they administer but don't personally collect). Constitutional scholars are analytical observers (d = 0.5 by definition). The engine derives these from beneficiary/victim declarations plus exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (authorizing interstate commerce regulation while preserving state autonomy) is CONTESTED. Originalists argue it remains live — federal overreach continues. The New Deal settlement argues it was resolved by expanding federal power to match national economic integration. The reading persists not because the founding problem is dead, but because the constitutional text remains the same and originalism treats textual fixation as a duty, not a policy choice. Mandatrophy is NOT resolved — the constraint's mandate (the text) has not outlived its function from the reading's perspective; the dispute is whether the function itself has changed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_determinacy,
    'Does the Commerce Clause text genuinely bear only the narrow reading, or is the text inherently indeterminate such that the narrow reading is a contested construction?',
    'Corpus linguistics of founding-era ''commerce among the states'' usage; historical analysis of whether the Philadelphia Convention understood ''commerce'' to include manufacturing/production.',
    'If the text is genuinely determinate and narrow, the Mountain claim holds. If indeterminate, the reading is a choice masquerading as discovery — reclassifies toward Tangled Rope (coordination + extraction) or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_determinacy, conceptual, 'Whether the constitutional text itself fixes the Commerce Clause scope at border-crossing.').

omega_variable(
    state_benefit_realization,
    'Do state governments actually benefit from the narrow reading, or does the inability to address interstate problems at the federal level harm states collectively (race-to-the-bottom, collective action failures)?',
    'Empirical comparison of regulatory outcomes in policy areas where the narrow reading blocks federal action vs. areas where federal standards exist; analysis of interstate compacts as substitutes.',
    'If states are net harmed, the beneficiary declaration is false — the reading extracts from states too, making it a Piton or Snare rather than a Mountain with beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_benefit_realization, empirical, 'Whether the declared beneficiaries (state governments) are genuine net beneficiaries.').

omega_variable(
    instrumentalities_expansion,
    'Does the ''instrumentalities of interstate movement'' category secretly expand the narrow reading to cover most modern economic activity (internet, banking, logistics), making the narrow/expansive distinction collapse in practice?',
    'Doctrinal analysis of how courts applying the narrow reading treat digital platforms, financial networks, and supply chains — do ''instrumentalities'' become a backdoor for expansive regulation?',
    'If instrumentalities swallow the distinction, the reading''s Mountain claim is theatrical — the constraint presents as fixed but operates as a movable boundary. Theater ratio would be understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalities_expansion, conceptual, 'Whether the instrumentalities category undermines the reading''s textual fixity.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint properly a single reading of the commerce_clause_text kernel, or does the originalist methodology itself constitute a distinct kernel?',
    'Compare whether originalist and non-originalist readings share the same commitment to the constitutional text as kernel, or whether originalism posits a different kernel (founding-era public meaning vs. living constitutionalism''s evolving meaning).',
    'If originalism is a distinct kernel, this story misidentifies the kernel_id. The constraint family decomposition would need restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel_id correctly captures the shared commitment across readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_text__originalist_narrow_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(comm_tr_t76, commerce_clause_text__originalist_narrow_reading, theater_ratio, 76, 0.1).
narrative_ontology:measurement(comm_tr_t148, commerce_clause_text__originalist_narrow_reading, theater_ratio, 148, 0.3).
narrative_ontology:measurement(comm_tr_t206, commerce_clause_text__originalist_narrow_reading, theater_ratio, 206, 0.2).
narrative_ontology:measurement(comm_tr_t235, commerce_clause_text__originalist_narrow_reading, theater_ratio, 235, 0.15).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(comm_be_t76, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 76, 0.2).
narrative_ontology:measurement(comm_be_t148, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 148, 0.6).
narrative_ontology:measurement(comm_be_t206, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 206, 0.4).
narrative_ontology:measurement(comm_be_t235, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 235, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(comm_su_t76, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 76, 0.2).
narrative_ontology:measurement(comm_su_t148, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 148, 0.8).
narrative_ontology:measurement(comm_su_t206, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 206, 0.4).
narrative_ontology:measurement(comm_su_t235, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 235, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, commerce_clause_text__substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings form a constraint family decomposing the 'Commerce Clause' label. The originalist narrow reading (this story) claims Mountain with ε=0.35. The expansive_federal_reading claims Mountain (from its seat) with higher ε from national coordination needs. The substantial_effects_limited_reading claims Tangled Rope — genuine coordination function (national market regulation) with asymmetric extraction (states lose police power). The ε values differ because the referent arrangements differ: this reading assesses the standing arrangement (narrow textual limit) by its own lights; the expansive reading assesses the standing arrangement (broad federal power) by its lights. They are not the same constraint viewed differently — they are different constraints instantiated from the same kernel text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
