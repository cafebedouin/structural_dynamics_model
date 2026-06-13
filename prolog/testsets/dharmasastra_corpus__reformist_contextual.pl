% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra Reformist-Contextual Reading: Ethical Core vs. Historical Prescriptions
 *   domain: religious_law/textual_interpretation
 *
 * SUMMARY:
 *   The reformist-contextual reading of Dharmasastra claims that the ethical
 *   core—dharma as righteous conduct, virtue, duty—is separable from
 *   time-bound prescriptions (caste hierarchy, gender roles, ritual
 *   restrictions). This reading dominates modern Hindu institutions,
 *   education, and public discourse, especially in diaspora communities and
 *   academic contexts. It enables contemporary practitioners to maintain the
 *   texts' authority while explicitly renouncing oppression. The constraint
 *   operates as a TANGLED ROPE: it coordinates genuine ethical principle
 *   (dharma as universalizable virtue) while simultaneously extracting
 *   continued subordination from lower castes and women through
 *   reinterpretation (prescriptions persist in practice but are claimed to be
 *   merely contextual). The extraction persists because the reformist reading
 *   makes resistance harder—lower-caste communities must either accept that
 *   their oppression is historically contingent (and thus accept the
 *   reinterpretation) or reject the entire tradition, which most do not wish
 *   to do. The reading forecloses literal abolition while preventing outright
 *   rejection.
 *
 * KEY AGENTS:
 *   - brahminical_interpretation_authority — institutional seat that controls which readings count as legitimate; preserves textual authority while discarding oppressive elements explicitly
 *   - lower_caste_communities — powerless, identity-locked; bear the cost of prescriptions that persist in practice but are reframed as contextual
 *   - women_subject_to_gender_prescriptions — powerless, identity-locked; face reinterpreted rather than abolished gender duties
 *   - hindu_modernist_educated_class — powerful beneficiaries who gain ethical legitimacy while avoiding the prescriptions that constrain lower-caste and women practitioners
 *   - orthodox_literalist_communities — excluded from reformist legitimacy; would argue the reinterpretation dissolves the text's authority
 *   - abolitionist_rejection_advocates — excluded; argue that reinterpreting rather than abandoning texts preserves oppressive potential
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.58).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.42).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.58).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra Reformist-Contextual Reading: Ethical Core vs. Historical Prescriptions").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious_law/textual_interpretation").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, '11f2351b-08a7-4dfc-818d-e94d9751d81d').
narrative_ontology:cs_kernel_codification('11f2351b-08a7-4dfc-818d-e94d9751d81d', fixed_text).
narrative_ontology:cs_authority_grounding('11f2351b-08a7-4dfc-818d-e94d9751d81d', lineage).
narrative_ontology:cs_interpretation_layer_present('11f2351b-08a7-4dfc-818d-e94d9751d81d').
narrative_ontology:cs_reading_relation('11f2351b-08a7-4dfc-818d-e94d9751d81d', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('11f2351b-08a7-4dfc-818d-e94d9751d81d', dharmasastra_corpus__abolitionist_rejection, influences).
narrative_ontology:cs_axiom('11f2351b-08a7-4dfc-818d-e94d9751d81d', foundational, ethical_core_separable_from_prescriptions).
narrative_ontology:cs_axiom_status(ethical_core_separable_from_prescriptions, holdable).
narrative_ontology:cs_axiom_grounding('11f2351b-08a7-4dfc-818d-e94d9751d81d', ethical_core_separable_from_prescriptions, deontological).
narrative_ontology:cs_axiom('11f2351b-08a7-4dfc-818d-e94d9751d81d', secondary, texts_reflect_historical_conditions).
narrative_ontology:cs_axiom_status(texts_reflect_historical_conditions, holdable).
narrative_ontology:cs_axiom_grounding('11f2351b-08a7-4dfc-818d-e94d9751d81d', texts_reflect_historical_conditions, empirically_contingent).
narrative_ontology:cs_reference_frame('11f2351b-08a7-4dfc-818d-e94d9751d81d', dharma_as_eternal_ethical_principle).
narrative_ontology:cs_drift_state('11f2351b-08a7-4dfc-818d-e94d9751d81d', modern_equality_commitment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('11f2351b-08a7-4dfc-818d-e94d9751d81d', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, brahminical_interpretation_authority).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, caste_hierarchy_maintainers).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, lower_caste_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, women_subject_to_gender_prescriptions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, hindu_modernist_educated_class).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, textual_scholars_and_institutions).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, textual_authority_preservation).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, ethical_universalism_within_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Brahminical scholars and institutions (modern reform movements, academic departments, temple authorities) set the framework for how Dharmasastra is read. They claim authority to separate eternal ethical principles from historically contingent prescriptions. This authority rests on controlling textual interpretation and institutional legitimacy within Hindu tradition. They benefit from preserving textual authority while appearing to discard oppression.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, brahminical_interpretation_authority, agenda_setter,
    institutional, generational, identity_locked, continental).

% Bear the costs of the reformist reading: hierarchical prescriptions persist in practice (temple access restrictions, occupational inheritance, marriage norms) while being reframed as spiritual stages or cultural context. Reframing makes resistance harder because the text itself is presented as already reformed; rejecting the reading requires rejecting the tradition. Exit is constrained by religious identity and community membership.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, lower_caste_communities, payer,
    powerless, generational, identity_locked, continental).

% Face reinterpretation rather than abolition of gender duties (modesty, obedience, ritual restriction, widow roles). Reinterpretation often preserves prescriptions in modified form while claiming to have abandoned them. Women bear both the literal constraints and the interpretive claim that the constraints are merely contextual. Religious identity lock prevents easy exit.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, women_subject_to_gender_prescriptions, payer,
    powerless, biographical, identity_locked, continental).

% Educated, often English-literate urban Hindus (disproportionately upper-caste) who benefit from the reformist reading because it lets them maintain Hindu identity and textual authority while disavowing caste hierarchy explicitly. They can speak of Dharmasastra as universal ethical wisdom without defending prescriptions that disadvantage lower castes and women. Their social mobility and institutional access are not impeded by the prescriptions they reinterpret away.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, hindu_modernist_educated_class, beneficiary,
    powerful, generational, mobile, continental).

% Modern universities and institutes of Hindu studies who interpret Dharmasastra scholarship. They benefit from the reformist reading because it makes the texts fit modern ethical frameworks (human rights, gender equality, caste abolition). Their institutional legitimacy depends partly on showing that Hindu philosophy is compatible with modernity. They set interpretive standards that feed into education and public discourse.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, textual_scholars_and_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, textual_scholars_and_institutions, beneficiary).

% Organizations and communities invested in maintaining ritual caste distinctions benefit from the reformist reading because it allows them to preserve caste structures in practice while claiming the text has been reformed. Reinterpretation of varna as spiritual stages lets caste prescriptions persist without explicit textual justification. The constraint's enforcement depends on this simultaneous preservation and reframing.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, caste_hierarchy_maintainers, beneficiary,
    organized, generational, constrained, continental).

% Reject the reformist reading entirely, maintaining that varna and jati prescriptions are eternal and revealed. They are excluded from the reformist framework's legitimacy conversation because the reformist reading denies literalism's foundation. Were they seated, they would argue that separating ethics from prescriptions dissolves the text's authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_literalist_communities, excluded,
    organized, generational, constrained, continental).

% Reject the entire Dharmasastra framework as fundamentally tied to oppression. They argue that reinterpreting rather than abandoning texts preserves their authority and leaves room for oppressive readings to resurface. They are excluded from the reformist dialogue because the reformist position depends on the text being recoverable for ethical purposes.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, abolitionist_rejection_advocates, excluded,
    moderate, generational, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__reformist_contextual, brahminical_interpretation_authority).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__reformist_contextual, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles Hindu tradition with modernity by preserving textual authority and spiritual lineage while explicitly repudiating caste hierarchy and gender oppression as time-bound prescriptions. Enables contemporary Hindus to claim Dharmasastra as ethical wisdom without defending historical oppression. Creates a shared interpretive framework that appears to resolve the contradiction between textual tradition and modern ethics.
% TRANSFER_FUNCTION: Transfers interpretive authority from literal caste prescriptions to abstract universal principles. Moves the burden of resistance from the text itself to the historical context. Lower-caste communities and women pay by accepting that their oppression is contextual rather than text-bound, which makes challenging the prescriptions harder. Upper-caste beneficiaries and the modernist educated class gain by maintaining both textual authority and ethical legitimacy.
% ABSENT_VOICES: Orthodox literalist communities reject the reading but are excluded from legitimacy by its definition. Abolitionist advocates reject the entire framework and are excluded because the reformist position depends on textual recovery. Lower-caste voices advocating for explicit textual rejection are marginalized within the reformist consensus. Women theologians who reject the gender prescriptions entirely are sidelined in favor of interpretations that preserve some form of the prescriptions.
% DISAPPEARANCE_RATIONALE: If the reformist reading and its authority structure vanished overnight, Hindu communities would face an unmediated choice between orthodox literalism (accepting caste hierarchy) and abolitionism (rejecting Dharmasastra entirely). The reformist middle position would collapse. Educational curricula would shift; institutions claiming to have reformed the texts would lose legitimacy; lower-caste communities would gain clearer ground for rejecting the texts altogether.
% FOUNDING_PROBLEM: Dharmasastra texts contain prescriptions (caste, gender, ritual status) that are ethically incompatible with modern human-rights frameworks and commitments to equality. The founding problem is the apparent impossibility of maintaining Hindu textual tradition while abandoning caste hierarchy and gender prescription.
% FOUNDING_PROBLEM_CORROBORATION: Attested by multiple parties outside the reformist beneficiary set: abolitionist advocates argue it cannot be solved by reinterpretation; lower-caste scholars argue prescriptions persist despite reframing; comparative religionists document how the texts function in practice. Educational institutions and public intellectuals in India and diaspora openly acknowledge the tension between traditional texts and equality commitments—the reformist solution is contested precisely because the problem is real and urgent.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) and rising because the constraint begins as a seemingly benign reinterpretation (low extraction in period 0, ~0.42) but accumulates extraction over time as the reinterpretation becomes institutionalized and prescriptions persist in practice despite being claimed as contextual. Theater rises steadily (0.32 → 0.48) because the constraint's persistence increasingly depends on performative commitment to equality while actual prescriptions are preserved. Suppression is moderate (0.42) because it operates primarily through interpretive authority and identity lock rather than direct coercion; lower-caste communities cannot easily reject the texts because the texts are framed as already reformed. The suppression measured here is the internalized difficulty of rejecting a tradition that claims to have answered your objections. Accessibility collapse is moderate-high (0.61) because alternatives to the reformist reading are systematically marginalized: orthodox literalism is presented as backwards, abolitionism as tradition-denying, and any reading that keeps the prescriptions explicit is socially stigmatized. The coercion grid shows suppression stable across the interval (the mechanism does not intensify, but does not decay); resistance rises slightly, suggesting mounting pressure from constituencies who view the reinterpretation as inadequate.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (brahminical interpretation authority, textual scholars) should compute as moderate beneficiaries or even neutral coordinators—they frame themselves as solving a real problem (incompatibility between tradition and modernity). Lower-caste and women victim seats should compute as substantially targeted (high d). The engine's per-seat computation will show this divergence: from the authority seat, the constraint is genuine coordination; from the victim seats, it is enforced extraction with a reinterpretational cover. The claim (tangled_rope) reflects the structural reality that both functions are present.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahminical interpretation authority is the beneficiary (controls which readings are legitimate, maintains textual authority, preserves institutional position). Hindu modernist educated class are beneficiaries (gain ethical legitimacy without accepting prescriptions that constrain them; mobility allows them to opt out in practice). Lower-caste communities and women are victims (prescriptions persist, reinterpreted rather than abolished; reinterpretation makes resistance harder because the text is claimed to have already accounted for their objections). Orthodox literalist communities are trapped but excluded, not victimized within this reading's framework. Abolitionist advocates are excluded entirely. The directionality derives from: who benefits from the constraint persisting → beneficiaries; who pays through persistent prescriptions → victims; who cannot easily exit because of identity fusion with the tradition → identity-locked exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The reformist reading has not resolved its founding problem; it has displaced it. The founding problem is the apparent incompatibility between hierarchical prescriptions and equality commitments. The reformist solution appears to solve it (separate ethics from prescriptions), but the prescriptions persist in practice while being reframed as contextual. This is a classic mandatrophy case: the constraint was built to solve a real problem (how to maintain tradition while accepting modernity), but the solution preserves the problem it claims to have solved. Lower-caste and women practitioners still face prescriptions; they are just no longer openly text-bound. The solution has become a theater—the performative commitment to equality while actual practices persist—because the founding problem cannot be solved by reinterpretation alone. Abolishing the prescriptions entirely would require accepting that the text is not eternally valid, which the reformist reading refuses. Keeping the prescriptions explicitly would require rejecting equality commitments, which the reading also refuses. The reading is trapped between incompatible demands and uses reinterpretation as a way to claim both loyalty to tradition and commitment to equality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prescriptions_persistence_vs_reinterpretation,
    'Are the caste and gender prescriptions genuinely abandoned in the reformist reading, or do they persist in practice under a reinterpretational cover?',
    'Ethnographic study comparing how the reformist reading functions in communities where it is institutionalized (temples, educational curricula, family structures) versus communities that have adopted abolition: does the reformist reading''s reinterpretation materially change the prescriptions'' effects, or does it preserve them while changing their justification?',
    'If prescriptions persist unchanged, extraction is substantially higher than 0.58 (closer to 0.75+); the constraint would shift from tangled_rope toward snare. If prescriptions genuinely attenuate, the reading achieves its stated goal and extraction drops to 0.35-0.45. The temporal measurement series show rising theater_ratio, which suggests prescriptions persist; a resolution would confirm or refute this signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prescriptions_persistence_vs_reinterpretation, empirical, 'Whether reinterpretation is functionally equivalent to abandonment or merely reframing.').

omega_variable(
    separability_of_ethics_from_prescriptions,
    'Is the ethical core (dharma as righteous conduct) genuinely separable from the prescriptions, or is the separation itself an interpretive choice unique to the reformist reading?',
    'Comparative textual analysis: examine whether the Dharmasastra corpus itself presents ethics and prescriptions as integrated or separable. If the texts present them as inseparable (ethics flows from following prescribed duties), the separation is a reformist innovation, not a recovery. If the texts present ethics as theoretically independent, the reformist reading has textual grounding.',
    'If the separation is a reformist innovation imposed on the texts (not present in them), the claim to be recovering an ''eternal core'' is false; the reading is constructing a new ethics and falsely attributing it to the tradition. This would classify the constraint as a snare using textual authority as cover. If the texts themselves present separability, the reformist reading has defensible grounding, and the extraction measure is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_ethics_from_prescriptions, empirical, 'Whether the reformist reading recovers or invents the ethics-prescriptions separation.').

omega_variable(
    identity_lock_mechanism_internalized_vs_structural,
    'Is the identity lock that keeps lower-caste practitioners in the reformist framework structural (no other religious tradition available, legal penalties for exit) or internalized (belief that they should remain within the tradition despite oppression)?',
    'Analyze exit rates and exit narratives from communities that have adopted abolitionism or converted to other traditions versus communities within the reformist framework. If structural barriers are the primary lock, changing legal or institutional conditions should increase exit; if internalized identity is primary, exit remains low even when structural barriers relax.',
    'If lock is primarily internalized, the measured suppression (0.42) understates the constraint''s effective suppressive force—the target carries the constraint with them even if exit becomes possible. If structural, the suppression is accurate. This distinction affects whether the constraint is closer to tangled_rope (institutional enforcement possible to change) or snare (targets self-enforce the constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalized_vs_structural, empirical, 'Whether identity lock is maintained by external barriers or internalized commitment.').

omega_variable(
    reading_contest_over_textual_authority,
    'Can a single reading of Dharmasastra claim exclusive textual authority, or does the textual tradition necessarily contain multiple coherent readings?',
    'Hermeneutic analysis of the Dharmasastra corpus itself: does the text contain internal contradictions that make multiple coherent readings necessary? Do different texts in the tradition (Manu Smriti, Yajnavalkya Smriti, etc.) present incompatible norms that cannot be unified without selection? If so, all readings are partly constructed; none can claim to simply recover the text''s ''true'' meaning.',
    'If the text necessarily sustains multiple readings, the reformist reading''s claim to authority rests on institutional power (which reading institutions choose to legitimize) rather than on the text itself. This would elevate the extraction measure and clarify that the constraint is primarily about interpretive authority, not textual truth. This is a CONCEPTUAL omega because it concerns what ''textual authority'' means, not an empirical fact about the text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_over_textual_authority, conceptual, 'Whether Dharmasastra sustains only one coherent reading or necessarily contains multiple readings.').

omega_variable(
    coexistence_vs_foreclosure_with_abolitionism,
    'Does the reformist reading coexist with abolitionism as two live options within Hindu practice, or does the reformist institutional dominance functionally foreclose abolitionism by making it seem like tradition-rejection rather than tradition-interpretation?',
    'Analyze institutional and discursive dominance: count the representation of each reading in educational curricula, institutional authority statements, and public discourse. If abolitionism is systemically marginalized (not taught in religious schools, excluded from temple authorities, treated as an outside position), the reformist reading functionally forecloses it despite not logically ruling it out.',
    'If abolitionism is functionally foreclosed by institutional dominance, the reformist reading exerts an INFLUENCES relation that approaches foreclosure in practice. This would justify moving the reading_relations entry from coexists_with toward a hybrid or toward foreclosure. The constraint''s effective suppression would be higher because it prevents not just literal orthodoxy but also abolitionism from becoming an equally legitimate alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_vs_foreclosure_with_abolitionism, empirical, 'Whether reformist institutional dominance functionally forecloses abolitionism despite logical coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__reformist_contextual, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(dhar_tr_t0, observed).
narrative_ontology:measurement(dhar_tr_t8, dharmasastra_corpus__reformist_contextual, theater_ratio, 8, 0.37).
narrative_ontology:measurement_basis(dhar_tr_t8, observed).
narrative_ontology:measurement(dhar_tr_t16, dharmasastra_corpus__reformist_contextual, theater_ratio, 16, 0.42).
narrative_ontology:measurement_basis(dhar_tr_t16, observed).
narrative_ontology:measurement(dhar_tr_t25, dharmasastra_corpus__reformist_contextual, theater_ratio, 25, 0.46).
narrative_ontology:measurement_basis(dhar_tr_t25, observed).
narrative_ontology:measurement(dhar_tr_t35, dharmasastra_corpus__reformist_contextual, theater_ratio, 35, 0.48).
narrative_ontology:measurement_basis(dhar_tr_t35, observed).
narrative_ontology:measurement(dhar_tr_t50, dharmasastra_corpus__reformist_contextual, theater_ratio, 50, 0.48).
narrative_ontology:measurement_basis(dhar_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__reformist_contextual, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(dhar_be_t0, observed).
narrative_ontology:measurement(dhar_be_t8, dharmasastra_corpus__reformist_contextual, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(dhar_be_t8, observed).
narrative_ontology:measurement(dhar_be_t16, dharmasastra_corpus__reformist_contextual, base_extractiveness, 16, 0.52).
narrative_ontology:measurement_basis(dhar_be_t16, observed).
narrative_ontology:measurement(dhar_be_t25, dharmasastra_corpus__reformist_contextual, base_extractiveness, 25, 0.55).
narrative_ontology:measurement_basis(dhar_be_t25, observed).
narrative_ontology:measurement(dhar_be_t35, dharmasastra_corpus__reformist_contextual, base_extractiveness, 35, 0.57).
narrative_ontology:measurement_basis(dhar_be_t35, observed).
narrative_ontology:measurement(dhar_be_t50, dharmasastra_corpus__reformist_contextual, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(dhar_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__reformist_contextual, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(dhar_su_t0, observed).
narrative_ontology:measurement(dhar_su_t8, dharmasastra_corpus__reformist_contextual, suppression_requirement, 8, 0.37).
narrative_ontology:measurement_basis(dhar_su_t8, observed).
narrative_ontology:measurement(dhar_su_t16, dharmasastra_corpus__reformist_contextual, suppression_requirement, 16, 0.39).
narrative_ontology:measurement_basis(dhar_su_t16, observed).
narrative_ontology:measurement(dhar_su_t25, dharmasastra_corpus__reformist_contextual, suppression_requirement, 25, 0.41).
narrative_ontology:measurement_basis(dhar_su_t25, observed).
narrative_ontology:measurement(dhar_su_t35, dharmasastra_corpus__reformist_contextual, suppression_requirement, 35, 0.42).
narrative_ontology:measurement_basis(dhar_su_t35, observed).
narrative_ontology:measurement(dhar_su_t50, dharmasastra_corpus__reformist_contextual, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(dhar_su_t50, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(dhar_grid_01, dharmasastra_corpus__reformist_contextual, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(dhar_grid_02, dharmasastra_corpus__reformist_contextual, accessibility_collapse(class), 50, 0.68).
narrative_ontology:measurement(dhar_grid_03, dharmasastra_corpus__reformist_contextual, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(dhar_grid_04, dharmasastra_corpus__reformist_contextual, accessibility_collapse(individual), 50, 0.58).
narrative_ontology:measurement(dhar_grid_05, dharmasastra_corpus__reformist_contextual, accessibility_collapse(organizational), 0, 0.62).
narrative_ontology:measurement(dhar_grid_06, dharmasastra_corpus__reformist_contextual, accessibility_collapse(organizational), 50, 0.64).
narrative_ontology:measurement(dhar_grid_07, dharmasastra_corpus__reformist_contextual, accessibility_collapse(structural), 0, 0.72).
narrative_ontology:measurement(dhar_grid_08, dharmasastra_corpus__reformist_contextual, accessibility_collapse(structural), 50, 0.72).
narrative_ontology:measurement(dhar_grid_09, dharmasastra_corpus__reformist_contextual, resistance(class), 0, 0.58).
narrative_ontology:measurement(dhar_grid_10, dharmasastra_corpus__reformist_contextual, resistance(class), 50, 0.58).
narrative_ontology:measurement(dhar_grid_11, dharmasastra_corpus__reformist_contextual, resistance(individual), 0, 0.45).
narrative_ontology:measurement(dhar_grid_12, dharmasastra_corpus__reformist_contextual, resistance(individual), 50, 0.48).
narrative_ontology:measurement(dhar_grid_13, dharmasastra_corpus__reformist_contextual, resistance(organizational), 0, 0.52).
narrative_ontology:measurement(dhar_grid_14, dharmasastra_corpus__reformist_contextual, resistance(organizational), 50, 0.54).
narrative_ontology:measurement(dhar_grid_15, dharmasastra_corpus__reformist_contextual, resistance(structural), 0, 0.62).
narrative_ontology:measurement(dhar_grid_16, dharmasastra_corpus__reformist_contextual, resistance(structural), 50, 0.62).
narrative_ontology:measurement(dhar_grid_17, dharmasastra_corpus__reformist_contextual, stakes_inflation(class), 0, 0.62).
narrative_ontology:measurement(dhar_grid_18, dharmasastra_corpus__reformist_contextual, stakes_inflation(class), 50, 0.64).
narrative_ontology:measurement(dhar_grid_19, dharmasastra_corpus__reformist_contextual, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(dhar_grid_20, dharmasastra_corpus__reformist_contextual, stakes_inflation(individual), 50, 0.52).
narrative_ontology:measurement(dhar_grid_21, dharmasastra_corpus__reformist_contextual, stakes_inflation(organizational), 0, 0.55).
narrative_ontology:measurement(dhar_grid_22, dharmasastra_corpus__reformist_contextual, stakes_inflation(organizational), 50, 0.58).
narrative_ontology:measurement(dhar_grid_23, dharmasastra_corpus__reformist_contextual, stakes_inflation(structural), 0, 0.68).
narrative_ontology:measurement(dhar_grid_24, dharmasastra_corpus__reformist_contextual, stakes_inflation(structural), 50, 0.68).
narrative_ontology:measurement(dhar_grid_25, dharmasastra_corpus__reformist_contextual, suppression(class), 0, 0.42).
narrative_ontology:measurement(dhar_grid_26, dharmasastra_corpus__reformist_contextual, suppression(class), 50, 0.42).
narrative_ontology:measurement(dhar_grid_27, dharmasastra_corpus__reformist_contextual, suppression(individual), 0, 0.28).
narrative_ontology:measurement(dhar_grid_28, dharmasastra_corpus__reformist_contextual, suppression(individual), 50, 0.3).
narrative_ontology:measurement(dhar_grid_29, dharmasastra_corpus__reformist_contextual, suppression(organizational), 0, 0.35).
narrative_ontology:measurement(dhar_grid_30, dharmasastra_corpus__reformist_contextual, suppression(organizational), 50, 0.37).
narrative_ontology:measurement(dhar_grid_31, dharmasastra_corpus__reformist_contextual, suppression(structural), 0, 0.48).
narrative_ontology:measurement(dhar_grid_32, dharmasastra_corpus__reformist_contextual, suppression(structural), 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__reformist_contextual, 0.12).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% The dharmasastra_corpus kernel has three readings: reformist_contextual (this story), orthodox_literalist (separate story), and abolitionist_rejection (separate story). Each reading instantiates a distinct constraint with its own ε, beneficiary/victim structure, and type. Readings are linked via network.affects_constraints rather than merged into one story because ε-invariance requires separate stories when the same text is interpreted differently to yield different structural classifications. The reformist reading coordinates identity (Hindu-ness) while extracting acceptance of hierarchy reinterpreted as spiritual difference; the literalist reading coordinates hierarchy as cosmic order; the abolitionist reading rejects the entire framework. All three are live positions held by different communities; this story instantiates only the reformist reading as a clean, separate constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__reformist_contextual, powerless, 0.78).
constraint_indexing:directionality_override(dharmasastra_corpus__reformist_contextual, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
