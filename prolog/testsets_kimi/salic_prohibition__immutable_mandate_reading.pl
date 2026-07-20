% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__immutable_mandate_reading, []).

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
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Law as Immutable Divine/Natural Dynastic Mandate
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint instantiates the immutable_mandate_reading of the
 *   salic_prohibition kernel. It treats the Salic Law not as a revocable
 *   statute or a Frankish territorial custom, but as an irrevocable natural
 *   and divine law embedded in dynastic constitution. Under this reading,
 *   female heirs are categorically excluded from succession, challengers to
 *   female succession are structurally legitimate, and preventive war is
 *   justifiable to enforce agnatic priority. The constraint emerged in
 *   medieval and early modern Europe as a mechanism to fix dynastic
 *   inheritance, but its operation concentrated sovereignty exclusively in
 *   male lines and required escalating enforcementâincluding juridical
 *   exclusion and military actionâto maintain.
 *
 * KEY AGENTS:
 *   - agnatic_dynasts: Primary beneficiary (powerful/constrained) â collects crowns and territorial sovereignty via exclusion of female lines.
 *   - female_dynastic_heirs: Primary target (powerless/identity_locked) â structurally dispossessed of dynastic birthright by categorical gender exclusion.
 *   - dynastic_councils: Agenda-setter (institutional/constrained) â interprets and enforces the constitutional kernel but is itself bound by the tradition it administers.
 *   - realm_subjects: Secondary target (powerless/trapped) â supplies blood and treasure for enforcement wars without voice in constitutional design.
 *   - foreign_female_succession_realms: Excluded target (powerful/constrained) â excluded from interpretive community and legitimately targetable by preventive war under the reading's logic.
 *   - divine_law_jurists: Secondary beneficiary (organized/constrained) â collects authority rents from maintaining the sacred framing.
 *   - succession_law_historians: Analytical observer (analytical/analytical) â tracks the historical construction and contested scope of the law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.82).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.85).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Law as Immutable Divine/Natural Dynastic Mandate").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, 'ae5c18f9-eef2-4e77-8514-7aae007e6a0d').
narrative_ontology:cs_kernel_codification('ae5c18f9-eef2-4e77-8514-7aae007e6a0d', fixed_text).
narrative_ontology:cs_authority_grounding('ae5c18f9-eef2-4e77-8514-7aae007e6a0d', lineage).
narrative_ontology:cs_interpretation_layer_present('ae5c18f9-eef2-4e77-8514-7aae007e6a0d').
narrative_ontology:cs_reading_relation('ae5c18f9-eef2-4e77-8514-7aae007e6a0d', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('ae5c18f9-eef2-4e77-8514-7aae007e6a0d', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('ae5c18f9-eef2-4e77-8514-7aae007e6a0d', foundational, divine_agnatic_succession_mandate).
narrative_ontology:cs_axiom_status(divine_agnatic_succession_mandate, holdable).
narrative_ontology:cs_axiom_grounding('ae5c18f9-eef2-4e77-8514-7aae007e6a0d', divine_agnatic_succession_mandate, theological).
narrative_ontology:cs_axiom('ae5c18f9-eef2-4e77-8514-7aae007e6a0d', foundational, irrevocable_dynastic_constitutional_exclusion).
narrative_ontology:cs_axiom_status(irrevocable_dynastic_constitutional_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('ae5c18f9-eef2-4e77-8514-7aae007e6a0d', irrevocable_dynastic_constitutional_exclusion, conventional).
narrative_ontology:cs_reference_frame('ae5c18f9-eef2-4e77-8514-7aae007e6a0d', agnatic_divine_constitutional_order).
narrative_ontology:cs_drift_state('ae5c18f9-eef2-4e77-8514-7aae007e6a0d', early_modern_cognatic_pressure, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ae5c18f9-eef2-4e77-8514-7aae007e6a0d', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_dynasts).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, divine_law_jurists).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_dynastic_heirs).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, realm_subjects).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, foreign_female_succession_realms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Male heirs of the dynasty who exclusively inherit crowns, territories, and sovereign titles. Their claims are secured by the categorical exclusion of female lines; they bear the constraint's maintenance costs in the form of primogeniture discipline and dynastic duty but collect its primary benefit of undisputed agnatic succession.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_dynasts, beneficiary,
    powerful, generational, constrained, continental).

% Daughters and female-line descendants of the dynasty who are structurally excluded from succession regardless of capacity. Their dynastic identity is fixed; they cannot exit the exclusion except through marriage away from the realm, and even then their descendants are often excluded from the succession of their birth dynasty.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_dynastic_heirs, payer,
    powerless, biographical, identity_locked, national).

% Parlements, estates, and dynastic councils that interpret the constitutional text, issue exclusions of female claimants, and legitimate agnatic succession. They are bound by the interpretive tradition they administer and cannot revise the kernel without delegitimizing their own authority.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, dynastic_councils, agenda_setter,
    institutional, generational, constrained, national).

% Canon lawyers and jurisconsults who supply theological and natural-law arguments that agnatic succession reflects divine order. Their prestige, appointments, and scholarly authority depend on the continued acceptance of the immutable mandate reading.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, divine_law_jurists, beneficiary,
    organized, generational, constrained, national).

% The general population of the realm who supply taxes and military levies for wars fought to enforce agnatic exclusion, suppress female claimants, and wage preventive war against realms with female succession. They have no institutional voice in dynastic constitutional interpretation.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, realm_subjects, payer,
    powerless, biographical, trapped, national).

% Sovereign realms that practice female or cognatic succession. They are structurally excluded from the interpretive community that defines legitimate succession, yet they are treated as legitimate targets of preventive war under the agnatic priority doctrine and bear the military cost of the constraint's enforcement.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, foreign_female_succession_realms, excluded,
    powerful, generational, constrained, national).

% Comparative legal historians who document the contested origins of Salic Law, its probable limitation to Frankish allodial land, and the successful practice of female succession in other European jurisdictions. They operate outside the beneficiary structure of the dynastic constitution.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, succession_law_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__immutable_mandate_reading, agnatic_dynasts).
narrative_ontology:fixing_cost_class(salic_prohibition__immutable_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, unambiguous rule of dynastic descent to prevent competing claims, partition of realms, and succession civil wars by fixing succession exclusively in the agnatic male line.
% TRANSFER_FUNCTION: Moves sovereignty, territorial crowns, and dynastic continuity from female heirs and their descendants to agnatic male heirs; moves military and tax resources from realm subjects and foreign realms to the enforcement of agnatic exclusion, including preventive wars against states with female succession.
% ABSENT_VOICES: Female dynastic heirs are formally barred from the councils and parlements that interpret succession law. Foreign realms practicing female succession are treated as military targets rather than interlocutors in the constitutional tradition.
% DISAPPEARANCE_RATIONALE: If the immutable mandate vanished overnight, numerous European thrones would have passed to female lines historically; dynastic maps would reorganize around cognatic or mixed succession; the theological-juridical apparatus justifying agnatic priority would collapse; and the legal basis for preventive wars against female-succession realms would dissolve.
% FOUNDING_PROBLEM: Preventing recurrent dynastic civil war and territorial fragmentation caused by uncertain or competing succession claims among multiple eligible heirs.
% FOUNDING_PROBLEM_CORROBORATION: Comparative legal historians and political analysts outside the benefiting agnatic lines attest that cognatic and female succession functioned successfully in Spain, Portugal, and other jurisdictions without automatic fragmentation. Within the beneficiary set, dynastic councils and jurists assert the problem remains live and agnatic exclusion is the only solution; no corroboration from outside the benefiting parties supports this exclusivity claim.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint transfers full sovereignty from half the dynastic population to the other half based on gender. Suppression is higher (0.85) because persistence requires active juridical enforcement, exclusion of claimants, and military action (preventive war). Theater_ratio rises to 0.70 because the divine/natural-law justification becomes increasingly performative as historical and comparative evidence accumulates against immutability. Accessibility_collapse is moderate (0.50) because visible alternativesâsuccessful female succession in other realmsâpersist and are structurally available. Resistance is moderate (0.60) because excluded claimants and foreign powers actively contest the arrangement. The claim/metric independence is maintained: the constraint is CLAIMED as an immutable constitutional mandate while metrics describe heavily extractive, actively enforced operation requiring escalating theater.
 *
 * PERSPECTIVAL GAP:
 *   The agnatic dynast seat experiences the constraint as sacred constitutional order that guarantees stable, predictable rule and prevents ruinous partition. The female dynastic heir seat experiences the identical structure as total dispossession backed by theological-juridical edifice and military force. The dynastic council seat experiences it as a binding interpretive duty; the historian seat sees a contested text retroactively invested with divine authority. The engine computes these divergences from the structural asymmetry in beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   agnatic_dynasts are structural beneficiaries (low d): the constraint subsidizes their inheritance by removing female competitors. divine_law_jurists are secondary beneficiaries (low-moderate d): their authority rents depend on the sacred framing. female_dynastic_heirs are primary targets (high d): identity_locked, with no exit from dynastic exclusion; the extraction is total (loss of sovereignty). realm_subjects are diffuse targets (high d): trapped in the realm, bearing war taxation and levies. foreign_female_succession_realms are excluded targets (high d): outside the interpretive community but bearing the cost of preventive war legitimacy. dynastic_councils sit near symmetric-moderate d: they enforce but are themselves constrained by the constitutional tradition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problemâpreventing succession crisesâwas genuine, and the coordination function (clear rule of descent) is structurally real. However, by the early modern period the exclusion of female heirs itself generated major succession crises (War of Spanish Succession, War of Austrian Succession) that agnatic enforcement was supposed to prevent. The reading persisted beyond its coordination function because male dynasts and jurists captured substantial sovereignty and authority rents from its maintenance. Mandatrophy is not yet fully resolved in the authored intervalâthe constraint is still actively enforcedâbut the rising theater_ratio and repeated succession wars signal that the coordination rationale has substantially atrophied while extraction remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_law_constructed_status,
    'Is the Salic Law genuinely a natural or divine law constraint, or a positive legal construct retroactively invested with sacred and immutable authority to secure agnatic interests?',
    'Paleographic and historical-legal analysis of the original Salic text and its reception; comparison with other European succession systems that did not invoke divine mandate for agnatic rules.',
    'If constructed, the constraint is a false-summit mountain or tangled rope using sacred framing to mask extraction; if genuinely theological, the extraction may be interpreted as a different kind of structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_law_constructed_status, empirical, 'Whether the immutable divine status is historically authentic or retroactive justification.').

omega_variable(
    succession_coordination_or_crisis,
    'Does agnatic exclusion genuinely prevent succession crises, or does it generate them by creating a structurally legitimized class of excluded claimants who become permanent sources of dynastic instability?',
    'Comparative quantitative analysis of succession conflict frequency in agnatic versus cognatic succession regimes across European dynastic history.',
    'If agnatic regimes show equal or greater crisis frequency, the coordination function is cover for extraction; if lower, the tangled rope classification retains a stronger coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_coordination_or_crisis, empirical, 'Whether the constraint''s coordination function is genuine or performative.').

omega_variable(
    territorial_scope_ambiguity,
    'Is the immutable mandate universally binding on all dynastic constitutions, or territorially and historically limited to Frankish legal inheritance?',
    'Juridical-historical analysis of the original Salic text''s scope and the discursive mechanisms by which it was extended to non-Frankish dynastic territories.',
    'If scope is limited, the universal claim is imperial overstretch and the constraint functions as a snare on non-Frankish realms; if universal, the cognatic reading is simply wrong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_scope_ambiguity, conceptual, 'Territorial and historical scope of the Salic Law''s binding force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__immutable_mandate_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sali_tr_t8, salic_prohibition__immutable_mandate_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(sali_tr_t16, salic_prohibition__immutable_mandate_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(sali_tr_t24, salic_prohibition__immutable_mandate_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement(sali_tr_t32, salic_prohibition__immutable_mandate_reading, theater_ratio, 32, 0.62).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__immutable_mandate_reading, theater_ratio, 40, 0.7).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__immutable_mandate_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(sali_be_t8, salic_prohibition__immutable_mandate_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(sali_be_t16, salic_prohibition__immutable_mandate_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(sali_be_t24, salic_prohibition__immutable_mandate_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement(sali_be_t32, salic_prohibition__immutable_mandate_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__immutable_mandate_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__immutable_mandate_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sali_su_t8, salic_prohibition__immutable_mandate_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(sali_su_t16, salic_prohibition__immutable_mandate_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(sali_su_t24, salic_prohibition__immutable_mandate_reading, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(sali_su_t32, salic_prohibition__immutable_mandate_reading, suppression_requirement, 32, 0.82).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__immutable_mandate_reading, suppression_requirement, 40, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, resource_allocation).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% The salic_prohibition kernel decomposes into three structurally distinct readings. The immutable_mandate_reading claims universal divine-constitutional status and high extraction; the sovereign_override_reading claims positive revocability and lower extraction; the cognatic_reversion_reading claims limited Frankish applicability and functionally dissolves the constraint for non-Frankish territories. They are linked as a constraint family because they share the same historical kernel but instantiate different structural constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
