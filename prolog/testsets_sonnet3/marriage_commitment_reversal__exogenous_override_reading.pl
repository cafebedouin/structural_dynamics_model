% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: 1890 Manifesto as Federally Coerced Suspension of Plural Marriage Practice
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   This story authors ONE reading of a contested kernel — the 1890 Manifesto
 *   reversal of LDS plural marriage practice. Under this reading, the
 *   reversal was substantively an externally coerced institutional
 *   capitulation: escalating federal legislation (culminating in the
 *   Edmunds-Tucker Act's disincorporation and property-escheatment
 *   provisions) placed the Church's corporate existence and temple holdings
 *   under direct threat, and the Manifesto's public timing and content track
 *   that legal pressure rather than an independent doctrinal process.
 *   Critically, under this reading the underlying revelation (Doctrine and
 *   Covenants Section 132, establishing plural marriage as an eternal
 *   principle) was never doctrinally rescinded — only the public practice was
 *   suspended — leaving a durable doctrine-practice gap the institution has
 *   managed since 1890. This reading treats the federal government as the
 *   primary beneficiary (extracting institutional sovereignty and
 *   marriage-law uniformity) and the Church, its practicing members, and its
 *   property holders as the primary victims. This is NOT the only defensible
 *   reading: a sibling reading (endogenous_reinterpretation_reading) holds
 *   that Wilford Woodruff's own account of receiving a September 23
 *   revelation constitutes a genuine internal doctrinal event, independent of
 *   (or alongside) the federal pressure, which would make ε for that reading
 *   near zero rather than high. A third reading (practice_doctrine_gap)
 *   treats the persistent Section 132/practice split as the central
 *   structural fact rather than adjudicating causation at all. These are
 *   three distinct constraints sharing a kernel, not one constraint measured
 *   three ways; per the ε-invariance principle each is authored separately
 *   with its own ε and its own network link.
 *
 * KEY AGENTS:
 *   - federal_territorial_government: institutional/analytical — primary beneficiary, sets and enforces terms
 *   - anti_polygamy_political_coalitions: organized/mobile — secondary beneficiary, supplies political pressure
 *   - lds_institutional_sovereignty: institutional/trapped — primary victim of coerced capitulation
 *   - practicing_plural_families: powerless/trapped — bears the direct human cost
 *   - church_property_holders: moderate/constrained — bears administrative/legal cost
 *   - post_manifesto_polygamous_practitioners: powerless/trapped — excluded voice, living the unresolved gap
 *   - historians_of_mormon_polygamy: analytical/analytical — observer seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.81).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.78).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "1890 Manifesto as Federally Coerced Suspension of Plural Marriage Practice").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, 'e8c46872-f6d7-4ff1-91c5-a07670af6168').
narrative_ontology:cs_kernel_codification('e8c46872-f6d7-4ff1-91c5-a07670af6168', fixed_text).
narrative_ontology:cs_authority_grounding('e8c46872-f6d7-4ff1-91c5-a07670af6168', extraction).
narrative_ontology:cs_interpretation_layer_present('e8c46872-f6d7-4ff1-91c5-a07670af6168').
narrative_ontology:cs_reading_relation('e8c46872-f6d7-4ff1-91c5-a07670af6168', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8c46872-f6d7-4ff1-91c5-a07670af6168', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('e8c46872-f6d7-4ff1-91c5-a07670af6168', foundational, reversal_caused_by_external_coercion_not_revelation).
narrative_ontology:cs_axiom_status(reversal_caused_by_external_coercion_not_revelation, holdable).
narrative_ontology:cs_axiom_grounding('e8c46872-f6d7-4ff1-91c5-a07670af6168', reversal_caused_by_external_coercion_not_revelation, empirically_contingent).
narrative_ontology:cs_axiom('e8c46872-f6d7-4ff1-91c5-a07670af6168', foundational, unrescinded_scripture_preserves_doctrinal_claim_despite_suspended_practice).
narrative_ontology:cs_axiom_status(unrescinded_scripture_preserves_doctrinal_claim_despite_suspended_practice, holdable).
narrative_ontology:cs_axiom_grounding('e8c46872-f6d7-4ff1-91c5-a07670af6168', unrescinded_scripture_preserves_doctrinal_claim_despite_suspended_practice, conventional).
narrative_ontology:cs_reference_frame('e8c46872-f6d7-4ff1-91c5-a07670af6168', section_132_as_binding_eternal_revelation).
narrative_ontology:cs_drift_state('e8c46872-f6d7-4ff1-91c5-a07670af6168', post_edmunds_tucker_enforcement_1890, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e8c46872-f6d7-4ff1-91c5-a07670af6168', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, anti_polygamy_political_coalitions).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, practicing_plural_families).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, church_property_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Passed and enforced the Edmunds-Tucker Act, disincorporating the Church, escheating its property, disenfranchising practitioners, and threatening seizure of temples. Set the terms under which the Church could retain assets and Utah could pursue statehood: public renunciation of the practice. Collects consolidated federal authority over a previously autonomous territorial religious body and removes a persistent obstacle to Utah's political assimilation.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_government, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_government, agenda_setter).

% National Protestant reform networks and congressional blocs that campaigned for decades to criminalize plural marriage as a moral and political threat. They receive vindication and political credit when the practice is publicly suspended, and continued leverage over the Church's public conduct afterward.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, anti_polygamy_political_coalitions, beneficiary,
    organized, generational, mobile, national).

% The Church as an institution faced confiscation of its temples, dissolution of its corporate charter, and criminal prosecution of its leadership. It issued the Manifesto (1890) declaring an intention to submit to federal marriage law and advising members to conform, but the underlying revelation (Doctrine and Covenants Section 132) was never rescinded or doctrinally repudiated. The institution surrendered practice under duress while preserving the eternal principle in scripture, producing a durable doctrine-practice gap it has managed ever since. It had no exit: dissolution or compliance were the only options on the table.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty, payer,
    institutional, civilizational, trapped, national).

% Members already living in plural marriages bore the direct human cost of reversal: forced dissolution or concealment of family structures, loss of legal standing for existing wives and children, prosecution risk, and social stigma from both federal authorities and, later, the institution's own public distancing. They had essentially no exit — leaving the faith did not undo existing marriages or restore legal protections, and remaining meant living the doctrine in secrecy or abandoning family units already formed.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, practicing_plural_families, payer,
    powerless, biographical, trapped, regional).

% Local congregations, trustees, and institutional officers whose property and legal standing were directly targeted by escheatment provisions. They bore the immediate administrative and financial cost of federal seizure actions pending compliance.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, church_property_holders, payer,
    moderate, generational, constrained, regional).

% Members and some leaders who continued or resumed plural marriage in secret after 1890, believing the underlying revelation still bound them since it was never doctrinally renounced. Their position was never given institutional voice — the Church publicly disavowed and later excommunicated continuing practitioners rather than acknowledging the doctrine-practice tension they were living inside.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, post_manifesto_polygamous_practitioners, excluded,
    powerless, biographical, trapped, regional).

% Document the sequence of federal legislation, prosecutions, and the Manifesto's text and aftermath, comparing the coercion timeline against the Church's own account of the change as revelatory. Analyze property records, congressional debate, and internal Church correspondence from the period.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, historians_of_mormon_polygamy, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the coordination sense proper to this reading: the arrangement is presented by the federal government as coordinating national marriage law uniformly across territories, but from this reading's structural analysis the 'coordination' is the vehicle for extracting compliance from a resistant religious institution, not a genuine shared-benefit solution the Church and federal government jointly needed.
% TRANSFER_FUNCTION: Moves institutional autonomy, ecclesiastical authority over marriage practice, and disputed property/legal standing from the LDS Church and its practicing members to federal territorial authority, in exchange for the Church's survival as a legally recognized corporate entity and Utah's eventual path to statehood.
% ABSENT_VOICES: Practicing plural families and continuing practitioners after 1890 had no seat in either the federal legislative process or the Church's own public messaging; their lived situation was resolved by both institutions treating the practice as terminated while the underlying doctrine remained scripturally intact.
% DISAPPEARANCE_RATIONALE: Had the federal coercion (Edmunds-Tucker enforcement, threatened disincorporation and temple seizure) not existed, there is no structural reason internal to the Church's own doctrinal development that the practice would have been publicly suspended in 1890 on this timeline — the institution's own scripture remained unrevised. Removing the external threat removes the reading's entire causal mechanism; the practice's public suspension depends on it.
% FOUNDING_PROBLEM: The federal government sought to eliminate plural marriage as a practice it viewed as incompatible with national marriage law and social order, using territorial control, disincorporation threats, and property seizure as leverage against an institution it could not otherwise compel.
% FOUNDING_PROBLEM_CORROBORATION: Congressional debate records and federal prosecutorial correspondence from the period, plus later independent historical scholarship (e.g. analyses of the Edmunds-Tucker Act's escheatment provisions and the timing of the Manifesto relative to imminent temple seizure), corroborate from outside the Church's own institutional voice that the timing and content of the 1890 Manifesto tracked the external legal threat closely. This is external corroboration, not the Church's own retrospective narrative, which frames the change as revelatory rather than coerced.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply across the interval (0.35 to 0.81) tracking the escalation from early anti-bigamy statutes (Morrill Act, 1862) through the Edmunds Act (1882) to the Edmunds-Tucker Act (1887) and its enforcement through 1890 — this reading holds the coercive pressure, not any internal doctrinal shift, as the driving variable, so extraction accumulates with legislative and prosecutorial escalation. Theater ratio rises in parallel (0.2 to 0.62) because as the interval progresses, an increasing share of the institution's public posture — public disavowal statements, later formal excommunication policies for continuing practitioners — functions as performed compliance rather than substantive doctrinal change, precisely because Section 132 remains canonically intact. Suppression climbs steeply early (0.4 to 0.78) reflecting the buildup of federal enforcement infrastructure (marshals, prosecutions, disincorporation proceedings) then plateaus once the Manifesto achieves its coercive purpose and active enforcement de-escalates.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal territorial government sits at the beneficiary end: it acquires exactly what it sought (public suspension of the practice, removal of the primary obstacle to statehood, and an institution now dependent on federal goodwill for its corporate survival) at essentially no lasting cost to itself. LDS institutional sovereignty and practicing plural families sit at the target end: the institution loses de facto control over its own marriage doctrine's public expression under existential threat, and practicing families lose family and legal standing with no meaningful exit — leaving the faith does not restore what federal law had already stripped. Post-manifesto continuing practitioners occupy the most extreme trapped position: they are excluded from voice in either institution's account and bear prosecution and excommunication risk for acting on a doctrine never formally rescinded.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mislabeling the Manifesto as pure voluntary coordination (which would erase the coercive mechanism) or as a clean natural evolution of doctrine (which would erase the preserved Section 132 principle). By classifying it as a snare from this reading's lights — genuine victims (institutional sovereignty, practicing families), an identifiable extracting beneficiary (federal government), and active enforcement (disincorporation and prosecution machinery) — the classification holds the coercion visible rather than accepting either institution's preferred retrospective framing at face value. The doctrine-practice gap (Section 132 preserved, practice suspended) is exactly the residue a purely coercive reversal would leave; a genuine internal doctrinal reversal would more plausibly have produced a revision or repudiation of the underlying revelation, which did not occur under this reading's account.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_revelation_causal_primacy,
    'Was the 1890 Manifesto''s issuance causally driven primarily by federal legal coercion, or by an independent internal revelatory event (Woodruff''s account of a September 23 vision), with the timing coincidence being either causal convergence or post-hoc rationalization?',
    'Comparative analysis of the sequence and content of Woodruff''s private journal entries, the drafting history of the Manifesto text, and the precise timing of the disincorporation and escheatment proceedings against the Church''s corporate assets — establishing whether the revelatory account was contemporaneous with or subsequent to legal pressure, and whether internal Church correspondence from the period frames the decision in coercive or doctrinal terms.',
    'If coercion is established as causally sufficient and primary, this reading''s high-extraction, victim/beneficiary structure holds. If the revelatory account is established as independently sufficient (i.e., the Manifesto would plausibly have occurred on a similar timeline absent the specific federal threats), the endogenous_reinterpretation_reading''s near-zero-extraction classification becomes the more descriptively accurate account of the same historical episode — as a DIFFERENT constraint, not a revision of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_revelation_causal_primacy, conceptual, 'Causal primacy dispute between coercion and revelation readings of the 1890 Manifesto — the central committer-structure ambiguity of the marriage_commitment_reversal kernel.').

omega_variable(
    doctrine_practice_gap_persistence_mechanism,
    'Does the persistence of Section 132 as unrescinded canonical doctrine after 1890 reflect institutional strategic ambiguity (deliberately preserving a claim for future reactivation), genuine theological conservatism (scripture is not revised even when practice changes), or simple institutional inertia (no one undertook the doctrinal work to formally reconcile the two)?',
    'Examination of internal Church governance records, subsequent doctrinal statements (e.g. the 1904 Second Manifesto, later temple sealing policy for deceased spouses), and comparison with how the institution has handled other doctrine-practice tensions to identify a consistent institutional pattern.',
    'A strategic-ambiguity finding would strengthen this reading''s snare classification (the gap is maintained on purpose, extracting flexibility from an unresolved public commitment). A pure-inertia finding would suggest a milder, less deliberately extractive persistence — closer to piton dynamics for the doctrine-practice relationship specifically, though this would not change the coercion-driven character of the original 1890 reversal itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_practice_gap_persistence_mechanism, empirical, 'Why Section 132 was never formally rescinded despite the practice''s suspension.').

omega_variable(
    federal_beneficiary_durability,
    'Did the federal government''s extraction of institutional sovereignty from the Church produce a durable transfer (permanent loss of ecclesiastical marriage authority) or a temporary one (later renegotiated through statehood and subsequent Church political integration)?',
    'Track the post-1896 statehood settlement and 20th-century Church political normalization to assess whether the extracted sovereignty was ever functionally restored or compensated through other institutional gains (political influence, demographic growth, financial recovery).',
    'If durable, supports treating this as ongoing high-extraction; if substantially offset by later gains, suggests the extraction was concentrated in the 1887-1896 window and the constraint''s current-day operation should be measured differently (a further decomposition candidate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_beneficiary_durability, empirical, 'Whether the federal extraction of institutional sovereignty was permanent or later offset.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marr_tr_t8, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(marr_tr_t16, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement(marr_tr_t24, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 24, 0.58).
narrative_ontology:measurement(marr_tr_t32, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 32, 0.62).
narrative_ontology:measurement(marr_tr_t40, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(marr_be_t8, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(marr_be_t16, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(marr_be_t24, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(marr_be_t32, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 32, 0.81).
narrative_ontology:measurement(marr_be_t40, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t8, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(marr_su_t16, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(marr_su_t24, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(marr_su_t32, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(marr_su_t40, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'the 1890 Manifesto reversal' per the ε-invariance principle. exogenous_override_reading (this story) authors high ε (0.81) on the premise that federal coercion was causally primary and the doctrine-practice gap is a symptom of unresolved extraction. endogenous_reinterpretation_reading authors near-zero ε on the premise that Woodruff's revelatory account is causally sufficient, making the same episode a Mountain/Rope-adjacent doctrinal development rather than extraction. practice_doctrine_gap treats the structural ambiguity itself (not causal attribution) as the constraint, and is expected to sit as a Piton or Tangled Rope depending on how the persistence of the gap is metered. All three link to each other via affects_constraints; none averages or references the others' ε values internally, per DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
