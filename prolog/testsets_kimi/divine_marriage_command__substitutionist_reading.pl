% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Manifesto as Superseding Revelation (Substitutionist Reading)
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   The substitutionist reading of the LDS divine marriage command treats the
 *   1890 Manifesto as a superseding revelation that rescinded the plural
 *   marriage doctrine of D&C 132, making monogamy the sole valid form and
 *   casting continued polygamy as apostasy. This reading was advanced by the
 *   church hierarchy to navigate existential federal coercion â property
 *   seizure, leader imprisonment, disincorporation â but frames the shift
 *   as purely theological. The constraint extracts theological legitimacy,
 *   institutional membership, and family integrity from fundamentalist
 *   Mormons who maintain the prior command, while beneficiaries gain federal
 *   legitimacy and institutional survival. It is claimed as divine
 *   coordination (ongoing revelation) but operates through active
 *   enforcement: excommunication, loss of temple rites, and social apostasy
 *   branding.
 *
 * KEY AGENTS:
 *   - church_hierarchy: agenda_setter (institutional/identity_locked/global) â sets doctrine, enforces monogamy, frames Manifesto as revelation
 *   - mainstream_adherents: beneficiary (organized/constrained/global) â gain federal acceptance and institutional unity
 *   - fundamentalist_mormons: payer/victim (moderate/identity_locked/regional) â bear excommunication and loss of theological status
 *   - critical_historians: excluded (analytical/analytical/national) â document federal coercion but are absent from theological adjudication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.78).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.85).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Manifesto as Superseding Revelation (Substitutionist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, '5ea9e563-573d-4d35-964d-a60a3cb5f014').
narrative_ontology:cs_kernel_codification('5ea9e563-573d-4d35-964d-a60a3cb5f014', fixed_text).
narrative_ontology:cs_authority_grounding('5ea9e563-573d-4d35-964d-a60a3cb5f014', extraction).
narrative_ontology:cs_interpretation_layer_present('5ea9e563-573d-4d35-964d-a60a3cb5f014').
narrative_ontology:cs_reading_relation('5ea9e563-573d-4d35-964d-a60a3cb5f014', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('5ea9e563-573d-4d35-964d-a60a3cb5f014', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('5ea9e563-573d-4d35-964d-a60a3cb5f014', foundational, monogamy_doctrinally_binding).
narrative_ontology:cs_axiom_status(monogamy_doctrinally_binding, holdable).
narrative_ontology:cs_axiom_grounding('5ea9e563-573d-4d35-964d-a60a3cb5f014', monogamy_doctrinally_binding, theological).
narrative_ontology:cs_axiom('5ea9e563-573d-4d35-964d-a60a3cb5f014', foundational, manifesto_supersedes_section_132).
narrative_ontology:cs_axiom_status(manifesto_supersedes_section_132, holdable).
narrative_ontology:cs_axiom_grounding('5ea9e563-573d-4d35-964d-a60a3cb5f014', manifesto_supersedes_section_132, theological).
narrative_ontology:cs_reference_frame('5ea9e563-573d-4d35-964d-a60a3cb5f014', ongoing_revelation_supremacy).
narrative_ontology:cs_drift_state('5ea9e563-573d-4d35-964d-a60a3cb5f014', post_historical_investigation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5ea9e563-573d-4d35-964d-a60a3cb5f014', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, mainstream_adherents).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, fundamentalist_mormons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 Manifesto as binding revelation, administers excommunication of polygamists, and enforces monogamy as the only legitimate marriage form. Their authority and institutional survival depend on maintaining that the Manifesto was divine and not a response to federal coercion.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, church_hierarchy, agenda_setter,
    institutional, generational, identity_locked, global).

% Accept the Manifesto as genuine revelation, participate in a church that gained federal legitimacy and statehood, and benefit from the suppression of a practice that brought government persecution. They experience the constraint as theological coherence and social acceptance.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, mainstream_adherents, beneficiary,
    organized, biographical, constrained, global).

% Continue to regard plural marriage as a divine commandment and are excommunicated for it. They lose temple access, family sealing privileges, and community standing, and are branded apostates by the church they still consider true. Their identity is fused with the prior doctrine.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, fundamentalist_mormons, payer,
    moderate, generational, identity_locked, regional).

% Document the federal coercion behind the Manifesto in archival and legislative records. They are not admitted to the church's theological conversation, and their findings are treated as hostile or irrelevant to revelatory authority.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, critical_historians, excluded,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__substitutionist_reading, church_hierarchy).
narrative_ontology:fixing_cost_class(divine_marriage_command__substitutionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates institutional survival and federal legal compliance by replacing a criminalized marriage practice with an acceptable norm, allowing the church to retain property, avoid leader imprisonment, and achieve statehood.
% TRANSFER_FUNCTION: Transfers theological legitimacy and institutional membership from plural-marriage practitioners to the monogamist orthodoxy. Moves the costs of doctrinal reversal â excommunication, family dissolution, loss of salvific status â onto fundamentalists while the institution gains federal legitimacy and survival.
% ABSENT_VOICES: Fundamentalist theologians who read the Manifesto as prudential suspension rather than rescission; secular historians who document federal coercion; women in plural marriages whose theological status was retroactively reclassified without their voice in the doctrinal shift.
% DISAPPEARANCE_RATIONALE: If the constraint vanished â if the Manifesto were no longer binding and monogamy were not enforced as doctrine â the church would either revert to plural marriage (inviting legal and social crisis) or fragment into competing factions. The current institutional structure depends on this specific revelatory claim to maintain coherence.
% FOUNDING_PROBLEM: Federal destruction of the church: the Edmunds-Tucker Act authorized seizure of church property, disincorporation, imprisonment of polygamist leaders, and denial of Utah statehood unless plural marriage ended.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative records (Edmunds-Tucker Act, 1887) and executive correspondence attest the coercion from outside the church. Fundamentalist historians and secular scholars corroborate that the problem was federal survival, not a theological need for monogamy. The church's own historical essays now acknowledge federal pressure in limited contexts, though they maintain the revelation framing.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint imposes severe costs on fundamentalists: excommunication, dissolution of plural families, loss of salvific ordinances, and social apostasy branding. Suppression (0.85) is higher still because the constraint's persistence depends on actively excluding polygamy as a live theological option and punishing those who practice it. Theater_ratio (0.65) reflects that a substantial share of institutional energy is devoted to performing the 'revelation' framing â the narrative must be constantly rehearsed to prevent the 'coercion' reading from gaining legitimacy. Accessibility_collapse (0.88) is very high: within the substitutionist framework, the prior command is not merely inactive but apostate, leaving no legitimate alternative. Resistance (0.45) is moderate: fundamentalist communities persist and resist, but they are marginalized and lack institutional power. Temporal measurements show extraction rising as enforcement hardened in the first half of the twentieth century, then plateauing; theater remains elevated throughout because the revelation framing requires constant performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The church_hierarchy seat experiences the constraint as genuine prophetic coordination that saved the church; the fundamentalist_mormons seat experiences it as violent theological reversal and extraction. The mainstream_adherents seat is closer to symmetric â they receive social benefits and theological coherence but pay the cost of tabooing their own tradition. The engine computes this divergence from the structural data: beneficiaries with constrained exit sit near symmetric, while identity-locked victims sit near full target.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (church_hierarchy, mainstream_adherents) derive low directionality: the constraint subsidizes their institutional survival and social legitimacy. Victims (fundamentalist_mormons) derive high directionality: the constraint extracts their membership, family structure, and theological continuity. The church_hierarchy has identity_locked exit because their authority is fused with the Manifesto's revelatory claims; admitting coercion would collapse their position, so they are structurally bound to maintain the frame. Fundamentalist_mormons are identity_locked in the opposite direction â their self-concept is constituted by adherence to the prior revelation, making exit to monogamy unthinkable. This identity fusion on both sides amplifies the effective extraction for victims and deepens the subsidy for beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â federal destruction of the church â is dead. The constraint persists not because the coordination problem is live, but because the institution cannot revise the kernel without admitting the Manifesto was coerced, which would trigger authority collapse. This is mandatrophy: the arrangement outlived its function but persists because the cost of honest revision exceeds the benefit. The classification as tangled_rope (with high theater and dead founding problem) captures that the coordination was once genuine but has become extractive inertia. The engine should note the founding_problem_status/disappearance_verdict mismatch (dead x world_rearranges) as a zombie/tangletrope flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_or_coercion,
    'Was the Manifesto a genuine divine revelation, or a strategically framed response to federal coercion?',
    'Archival discovery of internal church correspondence from 1889-1890; analysis of whether the text was drafted by lawyers or prophetic dictation; correlation with federal enforcement timelines.',
    'If coercion is established as the sole driver, the constraint''s theological legitimacy collapses and the classification shifts toward pure extraction (snare); if revelation is independently corroborated, the coordination function strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_or_coercion, empirical, 'Ambiguity about the true origin of the Manifesto').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of fundamentalist Mormons primarily structural (excommunication, legal barriers) or internalized (self-censorship, cognitive fusion with the institutional narrative among mainstream adherents)?',
    'Post-exit trajectory analysis: if suppression of polygamous practice persists after formal excommunication is removed (e.g., in jurisdictions where it is legal), the suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure because the mainstream community carries the suppressive logic even without official enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    coordination_extraction_balance,
    'Does the institutional survival coordinated by the Manifesto justify the asymmetric costs imposed on fundamentalist Mormons, or has the constraint persisted beyond its coordinating function into inertial extraction?',
    'Comparative analysis of schismatic groups that rejected the Manifesto (e.g., FLDS) versus groups that found alternative survival paths without doctrinal reversal.',
    'If alternative survival paths existed, the constraint''s coordination rationale weakens and the tangled_rope classification tilts toward snare or piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_balance, conceptual, 'Whether the constraint''s persistence is still warranted by its original coordination function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__substitutionist_reading, theater_ratio, 0, 0.8).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__substitutionist_reading, theater_ratio, 20, 0.7).
narrative_ontology:measurement(divi_tr_t40, divine_marriage_command__substitutionist_reading, theater_ratio, 40, 0.6).
narrative_ontology:measurement(divi_tr_t60, divine_marriage_command__substitutionist_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(divi_tr_t80, divine_marriage_command__substitutionist_reading, theater_ratio, 80, 0.6).
narrative_ontology:measurement(divi_tr_t100, divine_marriage_command__substitutionist_reading, theater_ratio, 100, 0.64).
narrative_ontology:measurement(divi_tr_t130, divine_marriage_command__substitutionist_reading, theater_ratio, 130, 0.65).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__substitutionist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__substitutionist_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(divi_be_t40, divine_marriage_command__substitutionist_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(divi_be_t60, divine_marriage_command__substitutionist_reading, base_extractiveness, 60, 0.74).
narrative_ontology:measurement(divi_be_t80, divine_marriage_command__substitutionist_reading, base_extractiveness, 80, 0.76).
narrative_ontology:measurement(divi_be_t100, divine_marriage_command__substitutionist_reading, base_extractiveness, 100, 0.77).
narrative_ontology:measurement(divi_be_t130, divine_marriage_command__substitutionist_reading, base_extractiveness, 130, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__substitutionist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(divi_su_t20, divine_marriage_command__substitutionist_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(divi_su_t40, divine_marriage_command__substitutionist_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(divi_su_t60, divine_marriage_command__substitutionist_reading, suppression_requirement, 60, 0.84).
narrative_ontology:measurement(divi_su_t80, divine_marriage_command__substitutionist_reading, suppression_requirement, 80, 0.85).
narrative_ontology:measurement(divi_su_t100, divine_marriage_command__substitutionist_reading, suppression_requirement, 100, 0.86).
narrative_ontology:measurement(divi_su_t130, divine_marriage_command__substitutionist_reading, suppression_requirement, 130, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is the substitutionist reading of the divine_marriage_command kernel, treating the Manifesto as a superseding revelation. It is structurally distinct from the continuationist reading (polygamy merely suspended) and the coercion_visibility reading (Manifesto as acknowledged federal response). They form a constraint family linked by shared kernel but divergent epsilon values and stakeholder directionality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
