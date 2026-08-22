% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Prophetic Override of Plural Marriage Doctrine via Continuing Revelation (1890 Manifesto Reading)
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   Between 1852 and 1890, the LDS Church's public endorsement of plural
 *   marriage as required doctrine collided with escalating federal
 *   legislation designed to dismantle the church as a corporate and political
 *   entity. The 1890 Manifesto, issued by church president Wilford Woodruff,
 *   is read here as an exercise of continuing revelation: the same authority
 *   structure that established plural marriage as eternal law (D&C 132,
 *   presented as revealed through Joseph Smith) is invoked to supersede that
 *   mandate's binding practice. The override reading treats this as neither a
 *   doctrinal reversal nor a mere legal accommodation, but as the intended,
 *   load-bearing function of continuing revelation — the mechanism exists
 *   precisely so a living prophet can update binding practice under changed
 *   circumstances without admitting institutional error.
 *
 * KEY AGENTS:
 *   - church_institutional_leadership: agenda_setter/beneficiary (institutional/arbitrage) — exercises and benefits from the override authority
 *   - existing_plural_wives: payer (powerless/trapped) — bear the practical cost of a mandate withdrawn after they complied with it
 *   - fundamentalist_dissenters: payer/excluded (powerless/trapped) — punished for continuing to hold the reading the institution itself taught until 1890
 *   - federal_government: excluded (institutional/analytical) — the exogenous pressure source, not a party to the internal theology
 *   - religious_historians: observer (analytical/analytical) — assess revelation-vs-capitulation gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.58).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.62).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Prophetic Override of Plural Marriage Doctrine via Continuing Revelation (1890 Manifesto Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, 'de09ac9e-6a0d-46ef-bad5-3b4a84500b91').
narrative_ontology:cs_kernel_codification('de09ac9e-6a0d-46ef-bad5-3b4a84500b91', formalized).
narrative_ontology:cs_authority_grounding('de09ac9e-6a0d-46ef-bad5-3b4a84500b91', lineage).
narrative_ontology:cs_interpretation_layer_present('de09ac9e-6a0d-46ef-bad5-3b4a84500b91').
narrative_ontology:cs_reading_relation('de09ac9e-6a0d-46ef-bad5-3b4a84500b91', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('de09ac9e-6a0d-46ef-bad5-3b4a84500b91', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('de09ac9e-6a0d-46ef-bad5-3b4a84500b91', foundational, living_prophet_authority_supersedes_prior_written_revelation).
narrative_ontology:cs_axiom_status(living_prophet_authority_supersedes_prior_written_revelation, holdable).
narrative_ontology:cs_axiom_grounding('de09ac9e-6a0d-46ef-bad5-3b4a84500b91', living_prophet_authority_supersedes_prior_written_revelation, conventional).
narrative_ontology:cs_axiom('de09ac9e-6a0d-46ef-bad5-3b4a84500b91', secondary, institutional_survival_constraint_can_trigger_doctrinal_supersession).
narrative_ontology:cs_axiom_status(institutional_survival_constraint_can_trigger_doctrinal_supersession, holdable).
narrative_ontology:cs_axiom_grounding('de09ac9e-6a0d-46ef-bad5-3b4a84500b91', institutional_survival_constraint_can_trigger_doctrinal_supersession, instrumental).
narrative_ontology:cs_reference_frame('de09ac9e-6a0d-46ef-bad5-3b4a84500b91', continuing_revelation_supersession_authority).
narrative_ontology:cs_drift_state('de09ac9e-6a0d-46ef-bad5-3b4a84500b91', post_manifesto_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('de09ac9e-6a0d-46ef-bad5-3b4a84500b91', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_institutional_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_membership_at_large).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, monogamous_second_generation_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, existing_plural_wives).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, existing_plural_families).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_dissenters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the office through which continuing revelation is received and announced; declares the 1890 Manifesto as a binding new revelation superseding the practice mandate of D&C 132, while preserving the doctrine's underlying eternal validity in teaching. Secures the institution's survival — federal incorporation, temple property, statehood prospects — by exercising the very authority structure (living prophet over fixed text) that the covenant's own doctrine grants it.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, church_institutional_leadership, beneficiary).

% Already sealed under the prior revelation's terms; the override does not dissolve their existing marriages but ends the practice going forward, leaving them in a suddenly irregular legal and social status — neither fully vindicated nor fully protected, dependent on local leadership discretion for continued support and standing.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, existing_plural_wives, payer,
    powerless, biographical, trapped, regional).

% Children and household units built under the superseded mandate face social stigma, legal ambiguity under continuing federal prosecution risk, and loss of institutional protection as the church distances itself from the practice it once required for exaltation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, existing_plural_families, payer,
    powerless, biographical, trapped, regional).

% Hold that the original revelation is eternal and cannot be overridden by circumstance; excommunicated or marginalized for continuing the practice the institution now disavows. Their theological objection — that a prophet cannot revoke what a prophet declared eternal — is treated as apostasy rather than adjudicated on its own terms.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_dissenters, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_dissenters, excluded).

% Gains relief from federal prosecution, social ostracism, and the practical burdens of plural households; benefits from the church's continued institutional existence and the path to Utah statehood, without having personally been required to relinquish an existing marriage.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_membership_at_large, beneficiary,
    organized, generational, constrained, national).

% Applies escalating legal pressure (Edmunds-Tucker Act, disincorporation, disenfranchisement) that functions as the external force activating the override; not a party to the covenant's internal theology but the decisive exogenous pressure the doctrine of continuing revelation is invoked to absorb.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_government, excluded,
    institutional, generational, analytical, national).

% Assess whether the Manifesto represents genuine prophetic revelation or institutional capitulation retrofitted with revelatory language; examine private correspondence, church financial records, and the gap between public Manifesto language and continued private plural marriages for roughly a decade after 1890.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__prophetic_override_reading, church_institutional_leadership).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__prophetic_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Continuing revelation doctrine allows the institution to update binding practice without conceding that the underlying authority structure or prior revelation was ever wrong — it coordinates doctrinal continuity with practical survival by relocating the change inside the authority mechanism itself rather than treating it as external capitulation.
% TRANSFER_FUNCTION: Moves the burden of the practice's abandonment from the institution (which retains property, legal standing, and doctrinal face) onto existing plural families and dissenting members, who absorb the social, legal, and relational costs of a practice the institution now disavows going forward while declining to dissolve what already exists.
% ABSENT_VOICES: Fundamentalist dissenters who hold the immutable-commandment reading are the clearest excluded voice — their argument that eternal law cannot be prophetically overridden is treated as grounds for excommunication rather than engaged theologically. Existing plural wives themselves were not decision-makers in the Manifesto's drafting or announcement.
% DISAPPEARANCE_RATIONALE: If the prophetic-override mechanism were unavailable, the institution would have faced continued federal seizure of assets, disincorporation, and likely inability to achieve Utah statehood; conversely, without this specific reading of continuing revelation, the church's claim to authoritative flexibility under external pressure would collapse into either rigid schism (fundamentalist split, which did occur) or open acknowledgment of doctrinal capitulation.
% FOUNDING_PROBLEM: Federal anti-polygamy prosecution (Edmunds-Tucker Act) threatened to dissolve the church as a legal corporate entity, seize its temples and properties, and disenfranchise its members — an existential institutional survival crisis that the doctrine of continuing revelation was invoked to resolve without requiring the institution to admit prior error.
% FOUNDING_PROBLEM_CORROBORATION: Federal legal historians and the documented Edmunds-Tucker asset-seizure record attest the original external pressure is long resolved; the doctrine of continuing revelation itself, however, persists as a standing authority structure invoked on unrelated matters well beyond the polygamy crisis, corroborated by later church statements (e.g., 1978 priesthood revelation) citing the same override mechanism — an attestation from within the institution's own subsequent practice, not from a source independent of the benefiting party.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is authored at 0.58 by 1890: the override mechanism resolves an institutional survival crisis by transferring the practice's costs onto those who had already complied with the prior mandate, while the institution retains its authority structure, property, and legal standing intact. Suppression (peaking at 0.65 in 1887, just before the Manifesto, then settling near 0.60-0.62) reflects the active machinery required to enforce the transition: excommunication of continuing practitioners, denial of continued authority to the fundamentalist reading, and the legal apparatus used against holdouts. Theater ratio rises steadily to 0.45 because a substantial share of subsequent institutional activity — reaffirming that the 'eternal principle' remains true while practice is suspended — functions to preserve continuity-of-authority appearances rather than to resolve the substantive claims of harmed plural families.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat, this looks like the intended, legitimate operation of the covenant's own authority structure — continuing revelation working exactly as designed. From the payer seats (plural wives, families, dissenters), the same event computes as an extraction: a mandate imposed as eternal and required for exaltation, then withdrawn without restitution once it became a liability to the institution that imposed it. The engine's per-seat computation should surface this divergence structurally rather than resolve it toward either reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Church institutional leadership sits near the full-beneficiary end: it exercises the override and captures the resulting institutional survival, legal normalization, and eventual statehood. Existing plural wives and families sit near the full-target end: trapped by prior compliance, they bear irregular status and stigma with no exit — the override was not for them, it was through them. Fundamentalist dissenters are structurally targeted twice: first extracted from by having believed and practiced the original mandate, then excluded and punished for continuing to hold what was, until recently, official doctrine. Church membership at large is a genuine beneficiary with lower intensity — they gain relief from federal pressure without having personally practiced or renounced plural marriage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal legal dissolution threat) is genuinely dead — Utah achieved statehood, church corporate status was restored, prosecution ended. Yet the override mechanism itself, continuing revelation as a tool for superseding prior binding revelation under institutional pressure, persisted and was invoked again in materially different contexts (1978 priesthood revelation). This is not automatically mandatrophy: continuing revelation as a general doctrine may have ongoing coordination value distinct from the specific 1890 application. But treating the Manifesto's specific transfer of costs onto existing plural families as still-justified by the (now-dead) survival crisis would be a mandatrophy error — the crisis that justified the transfer is resolved, but the families who paid for it were never made whole, and the doctrine's authority to have imposed that cost is retrospectively naturalized as 'revelation' rather than examined as institutional response to duress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_capitulation,
    'Was the 1890 Manifesto a genuine exercise of continuing revelation, or institutional capitulation to federal pressure subsequently framed in revelatory language?',
    'Comparative analysis of private correspondence and internal church records from 1885-1890 against the public Manifesto text; examination of the roughly decade-long continuation of new plural marriages performed privately after 1890 (documented in later disciplinary proceedings), which is difficult to reconcile with a clean revelatory break.',
    'If capitulation, the override reading is itself a cover story for extraction under duress rather than a genuine coordination mechanism, strengthening the case for tangled_rope or even snare classification. If genuine revelation, the coordination function (updating binding practice without institutional collapse) is more substantively real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_capitulation, conceptual, 'Whether the override was genuine prophetic authority or retrofitted institutional survival strategy.').

omega_variable(
    override_scope_generality,
    'Does the prophetic override mechanism validated by this reading generalize to any future doctrine, or was it a one-time exception specific to the polygamy crisis?',
    'Track subsequent invocations of continuing revelation to supersede prior binding doctrine (e.g., 1978 priesthood revelation) and assess whether the institution treats the override capacity as a standing feature or an emergency-only mechanism.',
    'A generalized override capacity constitutes an ongoing coordination/extraction structure independent of the 1890 crisis; a one-time exception would mean this specific constraint''s structure does not persist beyond its founding problem''s resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_scope_generality, empirical, 'Whether continuing revelation as override authority is a standing institutional feature or a bounded historical exception.').

omega_variable(
    committer_framing_location,
    'Where exactly does the prophetic_override_reading diverge from the temporal_accommodation_reading, given both accept the Manifesto''s legitimacy — is the difference merely rhetorical (how much authority is claimed) or does it produce different downstream practical consequences?',
    'Compare institutional actions justified under each reading in subsequent decades: the override reading licenses future doctrinal supersession broadly, while the accommodation reading would only license temporary legal compliance without touching doctrine — track whether later revelations (1978) were justified using override language or accommodation language.',
    'If institutional practice consistently uses override-style justification for later doctrinal changes, this reading has more explanatory and predictive power than the narrower accommodation reading, and the two readings are not merely stylistic variants but license structurally different future authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_location, conceptual, 'Locating the structural disagreement between the override and accommodation readings of the same historical event.').


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
narrative_ontology:measurement(eter_tr_t1882, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1882, 0.25).
narrative_ontology:measurement(eter_tr_t1887, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1887, 0.35).
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(eter_tr_t1904, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1904, 0.45).

% Extraction over time
narrative_ontology:measurement(eter_be_t1852, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1852, 0.3).
narrative_ontology:measurement(eter_be_t1862, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1862, 0.35).
narrative_ontology:measurement(eter_be_t1882, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1882, 0.45).
narrative_ontology:measurement(eter_be_t1887, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1887, 0.52).
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1890, 0.58).
narrative_ontology:measurement(eter_be_t1904, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1904, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1852, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1852, 0.3).
narrative_ontology:measurement(eter_su_t1862, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1862, 0.4).
narrative_ontology:measurement(eter_su_t1882, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1882, 0.55).
narrative_ontology:measurement(eter_su_t1887, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1887, 0.65).
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1890, 0.62).
narrative_ontology:measurement(eter_su_t1904, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1904, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__prophetic_override_reading, 0.1).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the eternal_marriage_covenant kernel (the 1890 Manifesto and its doctrinal basis). The immutable_commandment_reading treats D&C 132 as eternal and non-negotiable, making prophetic override illegitimate. The temporal_accommodation_reading treats the Manifesto as narrow legal compliance leaving eternal doctrine untouched. This reading (prophetic_override_reading) treats the Manifesto as a full exercise of continuing revelation superseding the prior mandate outright. Each carries distinct beneficiary/victim structure and distinct ε (this reading's ε=0.58 reflects the transfer cost imposed on existing plural families under an override framing that claims the strongest authority scope of the three).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
