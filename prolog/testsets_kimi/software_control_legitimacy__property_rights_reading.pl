% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__property_rights_reading, []).

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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software Control as Property Right (Property Rights Reading)
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates the property_rights_reading of the
 *   software_control_legitimacy kernel. Under this reading, software creators
 *   possess a legitimate property-like authority to restrict use,
 *   modification, and distribution of their works. The constraint coordinates
 *   commercial software production by promising return on investment, while
 *   simultaneously extracting from FOSS advocates and developers who are
 *   denied the freedom to modify, share, and integrate proprietary code. It
 *   is actively enforced through copyright statutes, end-user license
 *   agreements, digital rights management, and trade-agreement harmonization.
 *   The kernel is contested: sibling readings frame software control as user
 *   freedom, pragmatic methodology, or commons governance. This story
 *   isolates the property-rights reading as a single Îµ-invariant constraint.
 *
 * KEY AGENTS:
 *   - software_vendors: agenda_setter (institutional/arbitrage) â write licenses, enforce terms, capture revenue
 *   - commercial_investors: beneficiary (powerful/mobile) â allocate capital contingent on exclusivity
 *   - end_users: payer (organized/constrained) â purchase access but surrender control over computing environment
 *   - foss_advocates: payer (organized/constrained) â promote software freedom but are structurally blocked by legal restrictions
 *   - foss_developers: payer (moderate/constrained) â build open tools but cannot legally interface with proprietary components
 *   - digital_commons_movement: excluded (organized/constrained) â argue for sharing-default but are kept out of IP policy forums
 *   - competition_regulators: observer (institutional/analytical) â evaluate whether licensing is anti-competitive or legitimate IP exercise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.55).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.65).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Property Right (Property Rights Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, '34327d20-1090-4da1-ae00-33e82eceeba9').
narrative_ontology:cs_kernel_codification('34327d20-1090-4da1-ae00-33e82eceeba9', formalized).
narrative_ontology:cs_authority_grounding('34327d20-1090-4da1-ae00-33e82eceeba9', lineage).
narrative_ontology:cs_interpretation_layer_present('34327d20-1090-4da1-ae00-33e82eceeba9').
narrative_ontology:cs_reading_relation('34327d20-1090-4da1-ae00-33e82eceeba9', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('34327d20-1090-4da1-ae00-33e82eceeba9', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('34327d20-1090-4da1-ae00-33e82eceeba9', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('34327d20-1090-4da1-ae00-33e82eceeba9', foundational, creation_entails_control_rights).
narrative_ontology:cs_axiom_status(creation_entails_control_rights, holdable).
narrative_ontology:cs_axiom_grounding('34327d20-1090-4da1-ae00-33e82eceeba9', creation_entails_control_rights, deontological).
narrative_ontology:cs_axiom('34327d20-1090-4da1-ae00-33e82eceeba9', foundational, investment_protection_enables_sustainability).
narrative_ontology:cs_axiom_status(investment_protection_enables_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('34327d20-1090-4da1-ae00-33e82eceeba9', investment_protection_enables_sustainability, instrumental).
narrative_ontology:cs_reference_frame('34327d20-1090-4da1-ae00-33e82eceeba9', statutory_property_rights_framework).
narrative_ontology:cs_drift_state('34327d20-1090-4da1-ae00-33e82eceeba9', digital_reproduction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('34327d20-1090-4da1-ae00-33e82eceeba9', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, commercial_investors).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write licensing terms, enforce EULAs, deploy DRM, and lobby for copyright expansion. Capture revenue through software sales, subscriptions, and restrictive licensing. Could pivot to service or open-core models if property rights weakened, but current returns depend on exclusivity.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Allocate venture and equity capital to software ventures contingent on intellectual property protection and exclusive control over distribution. Returns depend on the legal enforceability of restrictions on copying and modification.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, commercial_investors, beneficiary,
    powerful, biographical, mobile, global).

% Purchase licenses accepting restrictions on use, modification, and redistribution. Receive functional software in exchange but surrender control over their computing environment. Cannot legally patch, share, or audit the code they depend on without vendor permission.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, end_users, payer,
    organized, biographical, constrained, global).

% Promote software freedom as an ethical and practical imperative. Are structurally blocked from integrating proprietary code into commons-based ecosystems by copyright and license restrictions. Their preferred mode of software production and distribution is criminalized or contractually excluded by the property rights regime.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_advocates, payer,
    organized, generational, constrained, global).

% Build open-source tools, libraries, and operating systems. Cannot legally reverse engineer, interface with, or redistribute proprietary components, limiting interoperability and fragmenting the software ecosystem from their seat.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_developers, payer,
    moderate, biographical, constrained, global).

% Argue for an unrestricted sharing-default for digital goods and collective stewardship of software infrastructure. Structurally excluded from IP policy forums dominated by trade associations and property-rights framings; their proposals for commons-based copyright reform rarely reach drafting tables.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, digital_commons_movement, excluded,
    organized, generational, constrained, global).

% Evaluate whether software licensing practices constitute anti-competitive conduct or legitimate exercise of intellectual property rights. They hear from vendors, users, and FOSS representatives, and can impose remedies that alter the constraint's enforcement scope.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, competition_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__property_rights_reading, software_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables sustained commercial investment in software by protecting against immediate free-riding; creates a market where developers and firms can expect to capture returns on development costs through exclusive distribution rights.
% TRANSFER_FUNCTION: Moves money from end_users to software_vendors and commercial_investors via licensing fees; moves control over computing from users and FOSS communities to creators and vendors via legal restrictions on use, modification, and distribution.
% ABSENT_VOICES: The digital_commons_movement and public-domain advocates are structurally excluded from trade-agreement and legislative drafting processes; they would argue for a sharing-default and against the enclosure of digital infrastructure but are not seated at the negotiating table.
% DISAPPEARANCE_RATIONALE: If the property-rights constraint vanished overnight, commercial software business models would shift dramatically toward services, subscriptions, and open-core models; FOSS integration and interoperability would expand; investment patterns would reorient away from exclusivity-dependent ventures; and the legal landscape of digital goods would reorganize around access rather than restriction.
% FOUNDING_PROBLEM: Early software development required significant upfront investment with near-zero marginal cost of reproduction, creating a free-rider problem that threatened commercial viability and incentives for complex, polished software production.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of early computing attest to market fragility in the absence of IP protection from outside the current beneficiary set. Conversely, legal scholars studying open-source production and public-interest technologists attest that alternative sustainability models have proven viable at scale. No universally accepted neutral arbiter exists; the founding problem is disputed by seated parties.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate: restricting user freedoms is extractive, but not as severe as physical coercion or debt peonage. Suppression (0.65) reflects the active legal and technical enforcement required to prevent copying, modification, and redistribution. Theater_ratio (0.40) captures the performative aspect of IP enforcement campaigns and DRM that exceed actual economic protection needs. Accessibility_collapse (0.45) reflects that FOSS alternatives exist but face network-effect and interoperability barriers. Resistance (0.58) registers the sustained FOSS movement, piracy, and legislative lobbying against enclosure. The temporal series show a gradual intensification as digital reproduction technology matured and enforcement machinery hardened.
 *
 * PERSPECTIVAL GAP:
 *   The vendor seat experiences this constraint as necessary coordination: without exclusivity, investment collapses and commercial software disappears. The FOSS seats experience the same structure as extraction: their preferred production model is criminalized or contractually barred, and the 'investment' story is cover for enclosure. The engine computes this divergence from the structural data â the authored claim (tangled_rope) accepts both readings as structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Software_vendors and commercial_investors are structural beneficiaries: the constraint subsidizes their revenue model by restricting alternatives (low d, damped Ï). End_users, foss_advocates, and foss_developers are structural targets: they bear the cost of restricted freedoms, legal liability, and ecosystem fragmentation (high d, amplified Ï). The digital_commons_movement is excluded rather than coordinated â its absence from policy forums is a structural feature of the constraint's reproduction. The asymmetry is clear: one side captures rent from exclusion while the other side loses autonomy.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a rope because asymmetric extraction is present: FOSS advocates and developers are net losers under the regime, not beneficiaries. It is not a snare because the coordination function (enabling commercial investment in complex software) is genuine and not merely cover: significant software production has historically depended on this model. It is not a mountain because the property rights frame is constructed law, not natural necessity. The classification as tangled_rope captures the hybridity: real coordination layered with asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_as_natural_vs_constructed,
    'Is software property a natural extension of creator rights or a legally constructed monopoly?',
    'Comparative analysis of jurisdictions with different IP regimes and natural experiments in software production without proprietary rights.',
    'If purely constructed, the constraint''s legitimacy rests on contingent policy choices and the coordination story weakens; if natural, the extraction is more like a necessary cost of production incentives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_as_natural_vs_constructed, conceptual, 'Whether software property rights are natural or constructed.').

omega_variable(
    coordination_extraction_separability,
    'Can the investment-coordination function of proprietary software be separated from the freedom-restriction extraction?',
    'Analysis of alternative models (open core, dual licensing, public funding, subscription services) that sustain investment without restricting user freedoms.',
    'If separable, the property-rights reading is a tangled rope where extraction exceeds coordination cost; if inseparable, the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether coordination and extraction are structurally separable.').

omega_variable(
    kernel_reading_relationship,
    'Does the property-rights reading foreclose the freedom-imperative reading, or do they coexist as incommensurable frameworks?',
    'Examination of whether any single legal framework has successfully held both strong creator property rights and strong user freedom imperatives simultaneously.',
    'Determines whether the kernel is a logical contradiction to be resolved or a persistent pluralism to be managed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'Structural relationship between property-rights and freedom-imperative readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prop_rights_tr_t0, software_control_legitimacy__property_rights_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prop_rights_tr_t10, software_control_legitimacy__property_rights_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(prop_rights_tr_t20, software_control_legitimacy__property_rights_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(prop_rights_tr_t30, software_control_legitimacy__property_rights_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(prop_rights_tr_t40, software_control_legitimacy__property_rights_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(prop_rights_tr_t50, software_control_legitimacy__property_rights_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(prop_rights_be_t0, software_control_legitimacy__property_rights_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prop_rights_be_t10, software_control_legitimacy__property_rights_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(prop_rights_be_t20, software_control_legitimacy__property_rights_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(prop_rights_be_t30, software_control_legitimacy__property_rights_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(prop_rights_be_t40, software_control_legitimacy__property_rights_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(prop_rights_be_t50, software_control_legitimacy__property_rights_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(prop_rights_su_t0, software_control_legitimacy__property_rights_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(prop_rights_su_t10, software_control_legitimacy__property_rights_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(prop_rights_su_t20, software_control_legitimacy__property_rights_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(prop_rights_su_t30, software_control_legitimacy__property_rights_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(prop_rights_su_t40, software_control_legitimacy__property_rights_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(prop_rights_su_t50, software_control_legitimacy__property_rights_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, commons_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'software control legitimacy' conflates four structurally distinct claims. Per the Îµ-invariance principle, each reading instantiates a separate constraint with its own Îµ, beneficiary/victim structure, and classification. They are linked as a constraint family rather than collapsed into one ambiguous story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
