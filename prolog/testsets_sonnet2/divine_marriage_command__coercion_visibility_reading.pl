% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: 1890 Manifesto as Coercion-Acknowledged Institutional Survival Compromise
 *   domain: religious/political theology
 *
 * SUMMARY:
 *   This constraint is the coercion-visibility reading of the
 *   divine_marriage_command kernel: the 1890 Manifesto ending institutional
 *   sanction of plural marriage is read as an acknowledged response to
 *   escalating federal legal coercion (property confiscation,
 *   disincorporation threats, denial of statehood), with theological
 *   legitimacy for the change grounded in institutional survival necessity
 *   rather than new revelation superseding the old, and without treating the
 *   underlying command as still doctrinally binding. This reading closes the
 *   M-set gap other readings leave open by admitting non-revelatory
 *   (exogenous, coercive) grounds as a legitimate input to doctrinal change —
 *   which is itself the reading's distinguishing and destabilizing feature:
 *   if coercion counts as a valid input, the authority structure's claim to
 *   insulated revelatory grounding is compromised.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.55).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "1890 Manifesto as Coercion-Acknowledged Institutional Survival Compromise").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious/political theology").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, '7efbb1d6-3168-4320-9b9b-2f94de26982d').
narrative_ontology:cs_kernel_codification('7efbb1d6-3168-4320-9b9b-2f94de26982d', formalized).
narrative_ontology:cs_authority_grounding('7efbb1d6-3168-4320-9b9b-2f94de26982d', extraction).
narrative_ontology:cs_interpretation_layer_present('7efbb1d6-3168-4320-9b9b-2f94de26982d').
narrative_ontology:cs_reading_relation('7efbb1d6-3168-4320-9b9b-2f94de26982d', divine_marriage_command__continuationist_reading, influences).
narrative_ontology:cs_reading_relation('7efbb1d6-3168-4320-9b9b-2f94de26982d', divine_marriage_command__substitutionist_reading, influences).
narrative_ontology:cs_axiom('7efbb1d6-3168-4320-9b9b-2f94de26982d', foundational, coercion_is_legitimate_theological_input).
narrative_ontology:cs_axiom_status(coercion_is_legitimate_theological_input, holdable).
narrative_ontology:cs_axiom_grounding('7efbb1d6-3168-4320-9b9b-2f94de26982d', coercion_is_legitimate_theological_input, instrumental).
narrative_ontology:cs_axiom('7efbb1d6-3168-4320-9b9b-2f94de26982d', foundational, institutional_survival_can_ground_doctrinal_shift_absent_new_revelation).
narrative_ontology:cs_axiom_status(institutional_survival_can_ground_doctrinal_shift_absent_new_revelation, holdable).
narrative_ontology:cs_axiom_grounding('7efbb1d6-3168-4320-9b9b-2f94de26982d', institutional_survival_can_ground_doctrinal_shift_absent_new_revelation, conventional).
narrative_ontology:cs_reference_frame('7efbb1d6-3168-4320-9b9b-2f94de26982d', pre_manifesto_revelatory_authority).
narrative_ontology:cs_drift_state('7efbb1d6-3168-4320-9b9b-2f94de26982d', post_statehood_narrative_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7efbb1d6-3168-4320-9b9b-2f94de26982d', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, church_institutional_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, monogamous_mainstream_membership).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, plural_wives_and_children_disavowed).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, excommunicated_continuing_practitioners).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, institutional_survival_as_legitimate_theological_ground).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued and administers the Manifesto under direct federal legal and property-confiscation pressure (Edmunds-Tucker Act enforcement, threatened disincorporation and statehood denial). Retains institutional continuity, restores property, and achieves statehood by presenting the change as a pragmatic, coercion-acknowledged accommodation rather than new revelation. Controls the official narrative of why the change occurred and can revise emphasis on the coercion framing as political needs shift.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, church_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, church_institutional_leadership, beneficiary).

% Gains social legitimacy, legal safety, and access to statehood-conferred civic participation once the institution disavows plural marriage. Benefits from reduced federal targeting and improved standing in broader American society, at the cost of accepting a theological explanation (coercion-driven survival necessity) that admits the change was not purely revelatory.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, monogamous_mainstream_membership, beneficiary,
    organized, generational, mobile, national).

% Existing plural families are administratively and socially disavowed by the new institutional posture. Wives lose legal and religious recognition of their marriages; children's legitimacy and inheritance status become precarious. They cannot exit the consequences of a doctrinal shift enacted for institutional reasons they had no part in setting, and many bear ongoing social and legal costs for decades after the Manifesto.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, plural_wives_and_children_disavowed, payer,
    powerless, biographical, trapped, local).

% Members who continue practicing plural marriage after the Manifesto, believing the underlying revelation still binds, are excommunicated and cast as apostate. They bear the institution's need to demonstrate genuine compliance to federal authorities, absorbing the credibility cost so the mainstream institution can claim clean rupture.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, excommunicated_continuing_practitioners, payer,
    powerless, biographical, trapped, regional).

% Applied the coercive legal pressure (property seizure, disincorporation threats, denial of statehood) that this reading identifies as the actual cause of the doctrinal shift. Not part of the church's internal theological conversation, but its actions are the exogenous variable this reading insists must be counted as a legitimate causal input to doctrine — a status the institution's other readings resist acknowledging.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_government, excluded,
    institutional, biographical, analytical, national).

% Examine court records, correspondence, and institutional statements to assess how much of the Manifesto's timing and content tracks federal legal pressure versus internal revelatory process. Their historical reconstruction is the primary external corroboration (or challenge) available to any reading of the kernel.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, historians_and_outside_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the institution a legible, defensible account of why doctrine changed when it did — allowing leadership, membership, and external authorities to coordinate around a single agreed narrative of transition rather than an unresolved rupture.
% TRANSFER_FUNCTION: Moves social and legal legitimacy, physical property, and civic access (statehood, enfranchisement) from the disavowed practice and its adherents to the mainstream institution and its remaining membership; the cost of the transfer is borne by those whose marriages and revelatory claims are administratively voided.
% ABSENT_VOICES: The plural wives and their children, and the continuing practitioners who were excommunicated, are not parties to the leadership's negotiation with federal authorities and have no voice in how the coercion-versus-revelation framing is resolved; federal officials who shaped the pressure are likewise outside the internal theological conversation this reading is about.
% DISAPPEARANCE_RATIONALE: If this reading's acknowledgment of coercion vanished from the institutional account, the leadership would lose its most legible defense against charges that doctrine bent to external force, the historical record of federal pressure would have to be explained by some other route, and both disavowed families and continuing practitioners would lose the one framing that treats their loss as caused by real external duress rather than pure institutional choice or new revelation.
% FOUNDING_PROBLEM: The federal government's escalating legal campaign (Edmunds-Tucker Act, threatened disincorporation, denial of statehood, mass disenfranchisement and imprisonment of practitioners) made continued institutional existence as a functioning corporate and political entity untenable while plural marriage was practiced.
% FOUNDING_PROBLEM_CORROBORATION: Independent legal historians and federal court records from the period corroborate that institutional survival, not new revelation, was the proximate and acknowledged driver of the timing and language of the Manifesto; the church's own later statements (post-statehood) increasingly minimize this reading in favor of a revelatory framing, which the disavowed-family and excommunicated-practitioner seats dispute as retroactive narrative management.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steadily across the interval (0.30 to 0.68) tracking the escalation of federal legal pressure and the institution's growing dependence on the coercion narrative to manage both external legitimacy and internal dissent; it plateaus post-1890 because the settlement (statehood, property restoration) is reached and the extraction from disavowed families becomes a fixed rather than escalating cost. Theater ratio rises even faster (0.20 to 0.72) because increasing institutional energy goes into managing the NARRATIVE of coercion-driven change — public statements calibrated to satisfy federal observers while preserving internal doctrinal continuity — rather than into resolving the underlying claims of disavowed plural families. Suppression requirement peaks around the Manifesto's issuance (1887-1890) when excommunication of continuing practitioners is most actively enforced to demonstrate compliance, then eases somewhat as the settlement stabilizes and enforcement becomes routine rather than urgent.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership sits nearest full beneficiary: it authored the acknowledgment, controls its emphasis, and gained property restoration, statehood, and institutional continuity from the shift. Mainstream monogamous membership benefits substantially but indirectly, receiving legitimacy without bearing the direct cost of disavowal. Plural wives, their children, and continuing practitioners sit at the full-target end: trapped by biographical stakes (marriages, family legitimacy, standing in the only community many had), unable to exit the consequences of a decision made above them for institutional reasons. The federal government is excluded from the internal theological ledger even though its coercion is the exogenous variable this reading foregrounds — it does not collect or pay through this constraint, it applies the pressure that makes the constraint's central admission necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   The coercion-visibility reading prevents two mislabelings at once: it refuses to treat the Manifesto as pure top-down extraction unmoored from real external pressure (which would erase the genuine coordination problem the institution faced — actual survival as a legal entity), and it refuses to treat the change as costless, purely voluntary revelatory correction (which would erase the real victims of disavowal and excommunication). By naming institutional-survival-necessity as the theological ground, it keeps both the coordination function (avoiding institutional destruction) and the extraction (transferring the cost of compliance onto disavowed families and continuing practitioners) visible in the same account — which is precisely why tangled_rope, not mountain, rope, or pure snare, is the structurally accurate claim from this reading's own seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_as_valid_theological_input,
    'Can institutional survival under external coercion function as a legitimate theological ground for doctrinal change, or does admitting coercion as an input necessarily delegitimize the authority structure''s claim to revelatory insulation?',
    'Comparative analysis of how the institution treats subsequent doctrinal changes: if later changes are also grounded partly in external pressure without triggering legitimacy crisis, coercion-as-input is normalized within the tradition; if the institution instead retreats toward pure-revelation framing for later changes, this reading''s admission is treated internally as an exception requiring containment.',
    'If coercion is normalized as valid input, the authority structure shifts toward a practice-and-survival grounding that tolerates future admitted accommodation; if treated as exceptional, this reading remains a minority/dissenting account and the institution''s official narrative gravitates toward the substitutionist reading over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_as_valid_theological_input, conceptual, 'Whether admitting coercion as a doctrinal input is structurally sustainable for the authority claim.').

omega_variable(
    narrative_selection_over_time,
    'Is the institution''s later (post-statehood) tendency to minimize the coercion framing in favor of a revelatory framing itself a further extraction — retroactively erasing the coerced parties'' claim to have been genuinely wronged by external force plus internal accommodation?',
    'Track official institutional statements about the Manifesto across subsequent decades for framing shifts; corroborate against contemporaneous (1890s) correspondence and court testimony to establish which framing was operative at the time of decision versus which is retrospective narrative management.',
    'If retrospective narrative shifts toward revelation are confirmed, the coercion-visibility reading is not merely one of several equally live options but the historically prior and later-suppressed account — raising the stakes of this reading''s marginalization within the institution''s own self-presentation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_selection_over_time, empirical, 'Whether later institutional narrative shifts constitute an additional extraction from the coercion-visibility account itself.').

omega_variable(
    disavowed_family_standing_ambiguity,
    'Is the treatment of plural wives and children as victims of this specific doctrinal shift, or as victims of the underlying practice itself (which some readings hold was always going to end badly for them)?',
    'Distinguish costs attributable to the Manifesto''s specific administrative disavowal (loss of legal/religious recognition, inheritance disputes) from costs attributable to plural marriage''s pre-existing legal precarity under federal law even absent the Manifesto.',
    'If most costs predate the Manifesto and merely continue, this reading''s victim attribution should be narrowed to the marginal harm of disavowal specifically, not the full precarity of plural family status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disavowed_family_standing_ambiguity, empirical, 'Whether victim harm should be attributed to the Manifesto specifically or to antecedent legal conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 1862, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1862, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1862, 0.2).
narrative_ontology:measurement(divi_tr_t1874, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1874, 0.35).
narrative_ontology:measurement(divi_tr_t1882, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1882, 0.5).
narrative_ontology:measurement(divi_tr_t1887, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1887, 0.62).
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1890, 0.68).
narrative_ontology:measurement(divi_tr_t1904, divine_marriage_command__coercion_visibility_reading, theater_ratio, 1904, 0.72).

% Extraction over time
narrative_ontology:measurement(divi_be_t1862, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1862, 0.3).
narrative_ontology:measurement(divi_be_t1874, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1874, 0.42).
narrative_ontology:measurement(divi_be_t1882, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1882, 0.55).
narrative_ontology:measurement(divi_be_t1887, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1887, 0.63).
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1890, 0.68).
narrative_ontology:measurement(divi_be_t1904, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 1904, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1862, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1862, 0.35).
narrative_ontology:measurement(divi_su_t1874, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1874, 0.45).
narrative_ontology:measurement(divi_su_t1882, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1882, 0.58).
narrative_ontology:measurement(divi_su_t1887, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1887, 0.62).
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1890, 0.55).
narrative_ontology:measurement(divi_su_t1904, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 1904, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
