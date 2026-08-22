% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Continuing Revelation as Prophetic Override of Plural Marriage Mandate
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   This story instantiates the prophetic_override_reading of the eternal
 *   marriage covenant kernel: the doctrinal claim that continuing revelation
 *   vests the living prophet with authority to supersede a prior revelation —
 *   including one previously declared essential for exaltation — when
 *   circumstances (here, existential federal legal pressure) require it.
 *   Under this reading, the 1890 Manifesto is not a suspension leaving
 *   eternal doctrine untouched (the temporal_accommodation_reading) and not
 *   evidence that the earlier revelation was never truly immutable (which
 *   would contradict the immutable_commandment_reading), but an exercise of
 *   the prophetic office's ongoing authority to receive new binding
 *   instruction that displaces the old. The coordination function this
 *   reading serves is real: it lets a religious institution navigate
 *   existential external pressure without conceding that its governing
 *   authority structure was ever wrong. The extraction is borne by those who
 *   had already conformed to the superseded mandate and by those who continue
 *   to hold it as still binding.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.58).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.62).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Continuing Revelation as Prophetic Override of Plural Marriage Mandate").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '87a402f5-0b9e-47a3-8187-d38d9879499f').
narrative_ontology:cs_kernel_codification('87a402f5-0b9e-47a3-8187-d38d9879499f', fixed_text).
narrative_ontology:cs_authority_grounding('87a402f5-0b9e-47a3-8187-d38d9879499f', lineage).
narrative_ontology:cs_interpretation_layer_present('87a402f5-0b9e-47a3-8187-d38d9879499f').
narrative_ontology:cs_reading_relation('87a402f5-0b9e-47a3-8187-d38d9879499f', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('87a402f5-0b9e-47a3-8187-d38d9879499f', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('87a402f5-0b9e-47a3-8187-d38d9879499f', foundational, living_prophet_authority_supersedes_prior_revelation).
narrative_ontology:cs_axiom_status(living_prophet_authority_supersedes_prior_revelation, holdable).
narrative_ontology:cs_axiom_grounding('87a402f5-0b9e-47a3-8187-d38d9879499f', living_prophet_authority_supersedes_prior_revelation, conventional).
narrative_ontology:cs_axiom('87a402f5-0b9e-47a3-8187-d38d9879499f', secondary, circumstantial_necessity_triggers_new_binding_revelation).
narrative_ontology:cs_axiom_status(circumstantial_necessity_triggers_new_binding_revelation, holdable).
narrative_ontology:cs_axiom_grounding('87a402f5-0b9e-47a3-8187-d38d9879499f', circumstantial_necessity_triggers_new_binding_revelation, instrumental).
narrative_ontology:cs_reference_frame('87a402f5-0b9e-47a3-8187-d38d9879499f', d_and_c_132_binding_revelation).
narrative_ontology:cs_drift_state('87a402f5-0b9e-47a3-8187-d38d9879499f', post_manifesto_legal_crisis_resolution, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('87a402f5-0b9e-47a3-8187-d38d9879499f', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, monogamous_post_manifesto_membership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, plural_wives_and_families_disavowed).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_dissenting_members).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__prophetic_override_reading, living_prophet_supersession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the authority to declare that new revelation has been received and administers church discipline and doctrine accordingly. By invoking continuing revelation, the hierarchy can retire a practice that has become an existential legal liability (federal anti-polygamy prosecutions, seizure of church property, denial of statehood) while preserving claims to unbroken prophetic authority and institutional continuity. It controls which past revelations are read as eternal and which are read as time-bound.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_institutional_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, church_institutional_hierarchy, beneficiary).

% Members who join or remain after the 1890 Manifesto receive a faith community no longer targeted by federal prosecution, disenfranchisement, or property confiscation. They benefit from the doctrine's flexibility without having to reconcile personal practice with the earlier mandate; the override lets them treat obedience to living prophetic authority, not fidelity to a specific revealed commandment, as the operative loyalty test.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, monogamous_post_manifesto_membership, beneficiary,
    moderate, generational, constrained, national).

% Women and children in existing plural marriages entered under a mandate declared essential to exaltation. When the practice is suspended, their marriages become legally unrecognized, some families are broken up or driven underground, and their prior obedience is retroactively reclassified as no longer required — without restitution for the social, legal, and economic costs already incurred under the earlier command.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, plural_wives_and_families_disavowed, payer,
    powerless, biographical, trapped, regional).

% Members who hold that the earlier revelation was eternal and non-negotiable are excommunicated or marginalized for continuing the practice the institution itself once mandated as necessary for exaltation. They bear the cost of taking the immutable-commandment reading at its word after the institution has moved off it, and have no venue within the church to contest the reclassification.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_dissenting_members, payer,
    powerless, generational, trapped, national).

% Applies legal and economic pressure (Edmunds-Tucker Act, disincorporation, property seizure, statehood denial) that structurally forces the timing and content of the doctrinal shift, without being a party to the internal theological account of why the shift occurred. Its coercive leverage is the unstated but decisive engine of the override, yet the doctrine's internal narrative frames the change as purely revelatory.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_government, excluded,
    institutional, biographical, analytical, national).

% The office through which the override is exercised: declares that circumstances now require a different divine instruction, without needing to concede that federal coercion rather than independent revelation drove the timing. Retains and even strengthens authority by demonstrating the capacity to redirect doctrine while preserving claimed continuity of prophetic office.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, current_prophetic_leadership, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Examine correspondence, court records, and internal church communications from the period to assess whether the 1890 declaration reflects genuine new revelation or institutional accommodation to survive legal destruction. Their reconstructions are not binding on the church but shape the external record against which the doctrine's own account is measured.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, historians_and_outside_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__prophetic_override_reading, church_institutional_hierarchy).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__prophetic_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the institution a legitimate doctrinal mechanism to change course on a specific commanded practice without abandoning the claim that its leadership speaks with continuous divine authority — allowing the community to reorganize around a new practice without a schism-inducing admission that the prior revelation was simply wrong or coerced.
% TRANSFER_FUNCTION: Moves the cost of the practice's abandonment from the institution (which retains legal standing, property, and claimed authority) onto individuals who had conformed to the prior mandate — plural families lose recognition and stability, and dissenters who hold the earlier line lose membership standing — while moving continued legitimacy and survival to the institutional hierarchy and the majority membership who adapt.
% ABSENT_VOICES: The plural wives and children whose marriages were dissolved or delegitimized had no formal voice in the declaration process; fundamentalist dissenters who took the earlier revelation as eternal are excluded from the discourse that produced the reversal and are treated as apostate for maintaining the church's own prior position.
% DISAPPEARANCE_RATIONALE: If the continuing-revelation doctrine were unavailable as a mechanism, the institution would have faced an unmediated choice between open capitulation to external legal pressure (undermining claims of independent divine authority) or continued defiance risking institutional destruction; the existence of the mechanism materially changed which outcome occurred and how the change was narrated internally and externally.
% FOUNDING_PROBLEM: The doctrine of continuing revelation itself predates this specific application; its deployment here was to resolve an acute crisis: federal legal action threatened to dissolve the church corporation, seize its temples and properties, and deny Utah statehood unless the plural marriage practice ended.
% FOUNDING_PROBLEM_CORROBORATION: The acute federal legal crisis that occasioned the 1890 declaration is corroborated by external, non-church sources — federal court records, congressional legislation (Edmunds-Tucker Act), and contemporaneous press accounts — establishing that the immediate practical crisis has long since ended; the church's own account, however, continues to frame the change as revelatory rather than as a resolved legal emergency, so status is reported as dead from outside corroboration while contested internally.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises steadily across the interval (0.20 to 0.58) tracking the escalation of federal legal pressure and the corresponding institutional pivot — early revelation and practice carried low extraction (a functioning, uncontested internal doctrine), but as the practice became legally and politically costly, the override mechanism increasingly transferred cost onto plural families and dissenters while shielding the institution. Theater ratio rises in parallel (0.10 to 0.44) because the doctrinal framing of 'new revelation received' performs continuity of prophetic authority while the underlying driver — external coercion — goes unacknowledged in the internal narrative. Suppression climbs sharply through the 1880s (formal excommunication procedures, loyalty tests, disciplinary action against fundamentalist holdouts) and plateaus after 1890 at a substantial but not total level, reflecting continued but reduced internal enforcement against dissent.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seats (hierarchy, prophetic office), the override functions as continuous, legitimate exercise of an authority structure that was always designed to be responsive — a rope, coordinating the community through a real crisis. From the payer seats (disavowed plural families, fundamentalist dissenters), the same structural event operates as an enforced reclassification that transfers the cost of a reversed mandate onto those who had complied with it in good faith — indistinguishable in effect from extraction dressed as revelation. The engine's per-seat computation is expected to diverge along exactly this line; this divergence is the intended structural finding of authoring this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The church hierarchy and current prophetic leadership sit at the beneficiary end: they retain institutional survival, property, and unbroken claims to authority, and actually gain a demonstrated capacity to adapt doctrine under pressure. Monogamous post-Manifesto membership benefits from reduced external persecution. Plural wives and disavowed families and fundamentalist dissenters sit at the target end: they bear costs generated directly by the override mechanism — the first group through loss of marital and social recognition after having conformed to a prior mandate, the second through exclusion for continuing to hold the church's own former position. The federal government is excluded from the doctrinal account entirely despite being the load-bearing structural cause of the timing — this exclusion is itself analytically significant: the doctrine's internal narrative requires that coercion not appear as the operative variable.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem the override mechanism addresses in THIS application — imminent legal destruction of the church as a corporate and political entity — is resolved and corroborated as dead by external, non-church sources (federal statute and court record), even though the institution's own internal account treats the change as an ongoing exercise of live revelatory authority rather than a resolved historical accommodation. This mismatch (status=dead, but the institution narrates continuity rather than resolution) is exactly the signal the mandatrophy check is built to surface: a doctrine whose triggering crisis has ended can still be actively maintained as though the crisis were live, because acknowledging its resolution would require conceding that revelation followed legal necessity rather than the reverse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_coercion_causal_priority,
    'Did the 1890 declaration reflect genuine independent revelation that happened to coincide with federal pressure, or was the federal legal crisis the proximate cause of a doctrinal reclassification narrated afterward as revelation?',
    'Comparison of the timing and content of internal church correspondence and journal entries against the legislative and judicial timeline of federal anti-polygamy enforcement; assessment of whether doctrinal language anticipates or follows major legal setbacks.',
    'If coercion is causally prior, the prophetic_override_reading functions as a legitimating overlay on an externally forced accommodation, strengthening the reading of extraction under a coordination cover story. If revelation is genuinely independent of the legal timeline, the override reading''s coordination function is more structurally sound and the extraction score would likely be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_coercion_causal_priority, empirical, 'Whether prophetic override was causally independent of or driven by federal legal pressure.').

omega_variable(
    which_reading_the_hierarchy_actually_holds,
    'Does the current institutional hierarchy internally operate on the prophetic_override_reading (revocable mandate), the temporal_accommodation_reading (unaltered eternal doctrine, suspended practice), or does it strategically invoke whichever reading best serves a given audience or moment?',
    'Analysis of internal doctrinal instruction materials, temple ceremony language changes over time, and official statements addressed to different audiences (legal, membership, historical) for consistency or audience-dependent framing.',
    'If the hierarchy holds a single reading consistently, that reading is the structurally operative kernel and the others are purely external/dissenting framings. If the hierarchy shifts between readings by audience, the kernel itself is best modeled as strategically ambiguous, which would support a higher theater_ratio for whichever single reading is authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_the_hierarchy_actually_holds, conceptual, 'Whether one reading is the actual operative institutional position or the ambiguity itself is functional.').

omega_variable(
    restitution_owed_to_disavowed_families,
    'Given that plural families incurred real, irreversible social and legal costs under a mandate later reclassified as no longer binding, is any institutional restitution or acknowledgment owed to them, and has any been offered?',
    'Review of church statements, financial records, or membership policy regarding families affected by the transition; comparison with how the institution has handled other doctrinally reversed practices.',
    'Absence of acknowledgment or restitution supports treating the extraction from this group as unresolved and ongoing rather than a one-time historical cost; presence of restitution would lower the effective extraction score for this group specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restitution_owed_to_disavowed_families, preference, 'Whether unaddressed harm to disavowed plural families constitutes ongoing extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 1852, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1852, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1852, 0.1).
narrative_ontology:measurement(eter_tr_t1862, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1862, 0.15).
narrative_ontology:measurement(eter_tr_t1874, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1874, 0.22).
narrative_ontology:measurement(eter_tr_t1887, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1887, 0.32).
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(eter_tr_t1904, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1904, 0.44).

% Extraction over time
narrative_ontology:measurement(eter_be_t1852, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1852, 0.2).
narrative_ontology:measurement(eter_be_t1862, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1862, 0.28).
narrative_ontology:measurement(eter_be_t1874, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1874, 0.38).
narrative_ontology:measurement(eter_be_t1887, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1887, 0.5).
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1890, 0.58).
narrative_ontology:measurement(eter_be_t1904, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1904, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1852, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1852, 0.2).
narrative_ontology:measurement(eter_su_t1862, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1862, 0.3).
narrative_ontology:measurement(eter_su_t1874, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1874, 0.45).
narrative_ontology:measurement(eter_su_t1887, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1887, 0.6).
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1890, 0.62).
narrative_ontology:measurement(eter_su_t1904, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1904, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the eternal_marriage_covenant kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. The immutable_commandment_reading treats the original revelation as fixed eternal law; the temporal_accommodation_reading treats the Manifesto as suspending practice while preserving doctrine unaltered; this prophetic_override_reading treats the living prophetic office as having standing authority to supersede the prior revelation outright. All three share the same underlying historical kernel (the D&C 132 revelation and the 1890 Manifesto) but differ in where they locate binding authority and therefore in beneficiary/victim structure and extraction profile. Network edges are structural, not causal-necessity claims: this reading's institutional legitimation dynamics create downstream pressure on how the sibling readings are held by different factions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
