% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   domain: religious/political theology
 *
 * SUMMARY:
 *   This constraint isolates ONE reading of the eternal marriage covenant
 *   kernel: the claim that continuing revelation doctrine vests the living
 *   prophet with authority to supersede prior revelation — including a
 *   revelation as explicit as D&C 132's declaration of plural marriage as an
 *   eternal requirement for exaltation — when circumstances (here, sustained
 *   federal prosecution threatening the institution's legal existence)
 *   require it. This is distinct from the immutable_commandment_reading
 *   (which holds D&C 132 as fixed, unchangeable law) and the
 *   temporal_accommodation_reading (which holds the eternal principle
 *   untouched while only its earthly practice is suspended in deference to
 *   civil law). The prophetic_override_reading is structurally different from
 *   both: it locates ultimate authority in the living office rather than in
 *   either the fixed text or a doctrine/practice split, and treats the 1890
 *   Manifesto as an act of NEW revelation that genuinely alters the doctrinal
 *   landscape, not merely a practical suspension or an inert artifact.
 *
 * KEY AGENTS:
 *   - presiding_quorum_authority: Primary agenda-setter — issues the Manifesto framed as received revelation, exercises the override power, bears responsibility for the institution's survival (institutional/civilizational/arbitrage-at-leadership-level)
 *   - church_institutional_survival: Beneficiary in the abstract sense — the continued legal and organizational existence of the church as an entity, which the override directly secures against disincorporation and property seizure
 *   - monogamous_church_membership: Beneficiary — the great majority of members whose lives were not organized around plural marriage and who benefit from normalized relations with U.S. civil society post-Manifesto
 *   - plural_wives_and_children_post_manifesto: Primary victims — families whose marriages were solemnized under the prior revelation and who bear the human cost of the doctrinal reversal, facing social stigma, legal precarity, and in many cases de facto abandonment of standing
 *   - fundamentalist_dissenters: Secondary victims — those who maintained belief in the immutability of D&C 132 and were excommunicated or marginalized for continuing the practice after the override
 *   - federal_government: External institutional actor whose prosecutorial pressure (Edmunds-Tucker Act, disincorporation threat) is the proximate trigger activating the override power
 *   - believers_committed_to_1843_revelation_as_binding: Victims of the doctrinal reversal's legitimacy claim — those who took the earlier revelation as binding divine command now told a later revelation supersedes it, creating a crisis of trust in the stability of revealed truth itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.42).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.55).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Continuing Revelation as Prophetic Override of Plural Marriage Mandate").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious/political theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '9e36dae9-825e-4a42-8638-605b24398ebc').
narrative_ontology:cs_kernel_codification('9e36dae9-825e-4a42-8638-605b24398ebc', formalized).
narrative_ontology:cs_authority_grounding('9e36dae9-825e-4a42-8638-605b24398ebc', lineage).
narrative_ontology:cs_interpretation_layer_present('9e36dae9-825e-4a42-8638-605b24398ebc').
narrative_ontology:cs_reading_relation('9e36dae9-825e-4a42-8638-605b24398ebc', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('9e36dae9-825e-4a42-8638-605b24398ebc', eternal_marriage_covenant__temporal_accommodation_reading, influences).
narrative_ontology:cs_axiom('9e36dae9-825e-4a42-8638-605b24398ebc', foundational, living_office_supersedes_prior_textual_revelation).
narrative_ontology:cs_axiom_status(living_office_supersedes_prior_textual_revelation, holdable).
narrative_ontology:cs_axiom_grounding('9e36dae9-825e-4a42-8638-605b24398ebc', living_office_supersedes_prior_textual_revelation, conventional).
narrative_ontology:cs_axiom('9e36dae9-825e-4a42-8638-605b24398ebc', secondary, existential_institutional_threat_activates_override_authority).
narrative_ontology:cs_axiom_status(existential_institutional_threat_activates_override_authority, holdable).
narrative_ontology:cs_axiom_grounding('9e36dae9-825e-4a42-8638-605b24398ebc', existential_institutional_threat_activates_override_authority, instrumental).
narrative_ontology:cs_reference_frame('9e36dae9-825e-4a42-8638-605b24398ebc', living_prophetic_office_as_final_revelatory_authority).
narrative_ontology:cs_drift_state('9e36dae9-825e-4a42-8638-605b24398ebc', federal_prosecution_crisis_1887_1890, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('9e36dae9-825e-4a42-8638-605b24398ebc', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_institutional_survival).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, presiding_quorum_authority).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, monogamous_church_membership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, plural_wives_and_children_post_manifesto).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_dissenters).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, believers_committed_to_1843_revelation_as_binding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises the office through which continuing revelation is received and announced; issues the 1890 Manifesto framed as new divine guidance superseding prior practice. Faces disincorporation and asset seizure absent a change in practice, and resolves this by invoking the override power the doctrine itself grants the office. Bears no comparable personal cost to the reversal and retains full institutional authority afterward.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, presiding_quorum_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).

% The continued legal existence, property, and organizational integrity of the church as an entity — directly preserved by the override, since the alternative (continued mass prosecution and disincorporation under the Edmunds-Tucker Act) threatened the institution's ability to function at all.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_institutional_survival, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(eternal_marriage_covenant__prophetic_override_reading, church_institutional_survival).

% The majority of members whose family lives were not structured around plural marriage. They benefit from the normalization of relations with U.S. civil society, reduced social stigma, and statehood for Utah that followed the Manifesto, at essentially no direct personal cost.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, monogamous_church_membership, beneficiary,
    moderate, generational, mobile, national).

% Women and children whose marriages were solemnized as eternal, exaltation-necessary covenants under the prior revelation. The override leaves their marital and inheritance status legally and socially precarious; the institution that promised eternal standing to their unions now treats those unions as a liability to be managed down. Exit from the faith community forfeits the spiritual and social capital their whole lives were organized around, and there is no institutional remedy offered for the reversal's costs to them specifically.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, plural_wives_and_children_post_manifesto, payer,
    powerless, biographical, trapped, regional).

% Believers who hold that the 1843 revelation is binding and unchangeable and continue or advocate the practice after the Manifesto. They are excommunicated or otherwise cast out of institutional standing, effectively trading membership in the mainstream church for continued fidelity to what they understand as the earlier, still-valid command.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_dissenters, payer,
    powerless, generational, trapped, regional).

% Members who accepted D&C 132 as literally binding divine command and now must reconcile continued faith in the institution's revelatory authority with the fact that a decades-old 'eternal and unchangeable' commandment has been overridden. Some rationalize the reversal through the continuing-revelation frame; others experience it as evidence that revealed truth claims are contingent on institutional survival needs rather than fixed, undermining trust without offering an alternative faith community that resolves the tension.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, believers_committed_to_1843_revelation_as_binding, payer,
    moderate, biographical, constrained, national).

% The external institutional actor whose sustained legal pressure (Edmunds-Tucker Act, disincorporation proceedings, seizure of church property) is the proximate trigger for the override. Not a party to the internal doctrinal reasoning and not consulted on how the church frames its own theological response — its role is purely as the external force the override responds to.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_government, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__prophetic_override_reading, diffuse).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__prophetic_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Continuing revelation as a general doctrine coordinates a religious community's capacity to adapt binding guidance to changed circumstances without requiring a new founding text or schism every time conditions shift — it lets the living authority structure resolve conflicts between prior revealed commands and present survival needs through a single legitimating mechanism the community already recognizes.
% TRANSFER_FUNCTION: The override moves institutional survival and majority-membership normalization from the plural-marriage minority to the church as an organization and its mainstream membership: the crisis-resolution benefit accrues to the institution and the broad membership, while the standing, status, and certainty costs of the doctrinal reversal accrue to plural wives, their children, and those who took the earlier revelation as permanently binding.
% ABSENT_VOICES: Plural wives themselves were largely not consulted in the decision to issue the Manifesto; their marital status was managed as a policy consequence rather than negotiated as a stakeholder interest. Fundamentalist dissenters who would object that the override illegitimately reverses a divine command are excluded from the institution's decision-making entirely once their objection is voiced — dissent is grounds for excommunication, not a seat at the table.
% DISAPPEARANCE_RATIONALE: If the prophetic override doctrine and the specific 1890 exercise of it were retracted, the institution would face renewed legal jeopardy under continued federal anti-polygamy prosecution, statehood for Utah would likely not have proceeded on the historical timeline, and the church would need to either resume open plural marriage practice (reinstating direct legal conflict) or find an alternative legitimating mechanism for suspending it — the entire subsequent institutional trajectory (statehood, mainstream normalization, doctrinal handling of the fundamentalist schism) depends on this override having occurred and having been accepted as legitimate by the mainstream body.
% FOUNDING_PROBLEM: The specific founding problem was existential: sustained federal prosecution (Edmunds-Tucker Act and predecessor legislation) threatened the church's legal disincorporation and the seizure of its property, making continuation of open plural marriage practice incompatible with the institution's survival as a functioning legal entity.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Mormonism and legal historians of the Edmunds-Tucker Act period (outside the church's own institutional voice) corroborate that the federal legal and property threat was real, acute, and the direct proximate cause of the timing of the 1890 Manifesto — this is corroborated in the secular historical record independent of the church's own theological framing. The church's own institutional narrative, by contrast, tends to foreground the revelatory framing over the survival-pressure framing, which is precisely the asymmetry this story is built to surface.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).
:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high: the override mechanism itself does not extract resources in an ongoing rent-seeking sense — it is a one-time (though precedent-setting) exercise of doctrinal authority to resolve an existential threat. The extraction that exists is diffuse and reputational/relational: plural families lose institutional standing, and the doctrine of continuing revelation itself absorbs a legitimacy cost (if truth can be superseded once under duress, its stability as 'eternal truth' is compromised for adherents who took the earlier claim literally). Suppression is moderate-high (0.55) because dissent from the new position was met with real institutional force (excommunication, denial of temple privileges, in later decades outright expulsion of fundamentalist splinter groups) — this was not a soft transition. Theater ratio (0.4) reflects that a substantial performative component exists: the override is framed publicly as pure spiritual continuity ('nothing has changed doctrinally, only practice') while functioning practically as doctrinal reversal, producing an observable gap between stated and operative function. Accessibility collapse (0.6) is moderate: alternative readings (fundamentalist continuation) remained technically thinkable and were in fact pursued by a minority, but institutional accessibility to those alternatives collapsed sharply post-1890 as the mainstream church actively closed off legitimacy for continued practice.
 *
 * PERSPECTIVAL GAP:
 *   From the presiding quorum's seat, continuing revelation is a coordination mechanism functioning exactly as designed — the living prophetic office exists precisely to receive new guidance when circumstances change, and 1890 is the doctrine working correctly under pressure. From the seat of a plural wife whose marriage was solemnized as an eternal, exaltation-necessary covenant, the same mechanism appears as an institution retroactively destabilizing a promise it made to her in the name of survival. The engine's per-seat computation should register this asymmetry: the agenda-setter seat likely computes closer to rope/tangled_rope (functioning coordination under duress), while the victim seats likely compute closer to snare (a promise unilaterally revoked with no meaningful recourse) — the divergence IS the structural fact being measured, not a discrepancy to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The presiding quorum authority sits at the beneficiary/agenda-setter pole: it wields the override power, and the override secures the institution the quorum leads. Church institutional survival and the monogamous majority membership are near-beneficiaries: the override resolves an existential threat and normalizes their social position, at low direct cost to them. Plural wives, their children, and committed 1843-revelation believers sit at the target pole: they bear concrete losses (marital status, inheritance, social standing) that the override does not compensate, and their exit options are constrained — leaving the faith community forfeits both spiritual and social capital built around the very marriages now destabilized. Fundamentalist dissenters are pushed further toward trapped exit: continuing the practice they believe is required for exaltation now means expulsion from the institution that mediates that belief.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of continuing revelation as a doctrine — flexibility to receive new guidance as circumstances change, preventing a religious community from being permanently bound to time-bound instructions — remains partially live in the abstract (the doctrine is invoked for other purposes across church history) but the SPECIFIC founding problem this override solved (federal prosecution threatening institutional survival) is long dead. The mechanism (prophetic override authority) persists as an active doctrinal tool decades after its triggering crisis resolved, which is exactly the mandatrophy risk this classification exists to flag: distinguishing a live coordination function (the general doctrine of continuing revelation, still exercised) from the specific historical exercise now serving primarily as legitimating precedent for institutional positions rather than active crisis response.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_versus_expediency,
    'Was the 1890 Manifesto a genuine reception of new revelation superseding D&C 132, or a survival-driven policy reversal retroactively clothed in revelatory authority?',
    'Comparative analysis of the private journals, correspondence, and contemporaneous statements of the presiding quorum against the public revelatory framing issued to membership; examine whether the doctrinal content of D&C 132 was ever formally rescinded or only its practice suspended.',
    'If genuine revelation, the prophetic-override reading is structurally sound as a coordination mechanism for doctrinal evolution. If expediency retroactively framed as revelation, the override function is a legitimating fiction covering institutional capitulation to federal force — shifting this constraint toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_versus_expediency, conceptual, 'Genuineness of the revelatory claim underlying the override versus post-hoc legitimation of survival-driven policy change.').

omega_variable(
    override_scope_ambiguity,
    'Does the prophetic override power, once exercised on marriage practice, extend to overriding other ''eternal and unchangeable'' doctrines, and if so what limits it?',
    'Trace subsequent invocations of continuing revelation doctrine across other formerly ''eternal'' teachings (priesthood restriction, temple ordinances) to see whether the override mechanism activated by federal pressure in 1890 became a general-purpose doctrinal-revision tool or remained narrowly bounded to the marriage question.',
    'A broad, repeatedly-invoked override power supports classifying continuing revelation as institutional adaptive machinery (tangled rope, serving both flock coordination and hierarchy self-preservation); a narrowly bounded, one-time invocation supports a more constrained reading closer to scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(override_scope_ambiguity, empirical, 'Whether the override mechanism generalizes beyond the marriage practice case.').

omega_variable(
    kernel_framing_choice,
    'Is the eternal marriage covenant kernel best modeled as a fixed doctrinal text (D&C 132) with authority residing in the text, or as a living authority structure (the presiding prophet) with the text as one historical utterance among many?',
    'Compare how the institution itself frames the relationship in official pronouncements: does it describe the Manifesto as amending, superseding, or merely suspending D&C 132''s application? The institutional self-description determines whether text or living authority is the operative kernel component.',
    'If text-primacy is the operative framing, the prophetic-override reading is itself a subordinate mechanism operating under the fixed-text kernel (closer to the temporal_accommodation_reading). If living-authority-primacy is operative, the override reading is the dominant framing and the fixed text is demoted to historical artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the kernel''s authoritative center of gravity is the 1843 text or the living prophetic office — this story assumes living-authority-primacy per the assigned reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(eter_tr_t20, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(eter_tr_t30, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(eter_tr_t40, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(eter_tr_t50, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(eter_be_t20, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(eter_be_t30, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(eter_be_t40, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(eter_be_t50, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(eter_su_t20, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(eter_su_t30, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(eter_su_t40, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(eter_su_t50, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the eternal_marriage_covenant kernel. immutable_commandment_reading treats D&C 132 as fixed, unchangeable eternal law with no legitimate override mechanism — under that reading, this story's central claim (that the living prophet can supersede the 1843 revelation) is not merely a different emphasis but a direct contradiction of the immutable reading's foundational premise. temporal_accommodation_reading agrees with this reading that the Manifesto did not destroy the eternal doctrine, but differs sharply on WHAT changed: accommodation holds the doctrine untouched and only practice suspended (deference to civil law, not new revelation), while this reading holds that genuine new revelation altered the doctrinal landscape itself. The three readings produce different victim sets (accommodation reading treats plural families as still doctrinally validated though practically constrained; this reading treats their doctrinal status itself as revised) and different ε profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
