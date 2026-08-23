% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Ritual Symbolic Survival and Rabbinic Boundary Control
 *   domain: religious/collective_memory
 *
 * SUMMARY:
 *   This constraint story instantiates the symbol_survival_reading of the
 *   catastrophe_memory_survival kernel: the claim that Jewish collective
 *   survival after catastrophe is secured primarily through ritual practice
 *   that preserves identity and boundary-norms via symbolic experience. In
 *   this reading, survival IS continuity of practice itself. The constraint
 *   is structurally extractive toward secularized Jews who lose transmission
 *   and identity legitimacy under this framework, while concentrating
 *   interpretive control in rabbinic authority. It carries a genuine
 *   coordination function (collective memory preservation, intergenerational
 *   continuity) but operates asymmetrically: the coordinated pay compliance
 *   costs and identity burdens, while the agenda-setter captures the
 *   authority to define legitimate existence.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: Primary beneficiary and agenda-setter (institutional/constrained) â maintains interpretive control over ritual legitimacy and the power to define Jewish survival
 *   - secularized_jews: Primary target (moderate/identity_locked) â bear identity erasure and loss of transmission under the ritual-survival framework
 *   - practicing_community: Secondary beneficiary and payer (organized/constrained) â receive identity continuity in exchange for compliance costs and submission to rabbinic interpretation
 *   - non_rabbinic_movements: Excluded voice (moderate/constrained) â offer alternative survival framings but are structurally marginalized by the ritual-monopoly definition
 *   - memory_studies_scholars: Analytical observer (analytical/analytical) â compare this survival mechanism against cross-cultural alternatives outside the theological frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.8).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.68).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Ritual Symbolic Survival and Rabbinic Boundary Control").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, 'b7135036-0efe-475c-8b05-b4e98e34adc3').
narrative_ontology:cs_kernel_codification('b7135036-0efe-475c-8b05-b4e98e34adc3', formalized).
narrative_ontology:cs_authority_grounding('b7135036-0efe-475c-8b05-b4e98e34adc3', lineage).
narrative_ontology:cs_interpretation_layer_present('b7135036-0efe-475c-8b05-b4e98e34adc3').
narrative_ontology:cs_reading_relation('b7135036-0efe-475c-8b05-b4e98e34adc3', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7135036-0efe-475c-8b05-b4e98e34adc3', catastrophe_memory_survival__hybrid_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('b7135036-0efe-475c-8b05-b4e98e34adc3', foundational, ritual_continuity_is_survival).
narrative_ontology:cs_axiom_status(ritual_continuity_is_survival, holdable).
narrative_ontology:cs_axiom_grounding('b7135036-0efe-475c-8b05-b4e98e34adc3', ritual_continuity_is_survival, theological).
narrative_ontology:cs_axiom('b7135036-0efe-475c-8b05-b4e98e34adc3', foundational, rabbinic_interpretive_monopoly).
narrative_ontology:cs_axiom_status(rabbinic_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('b7135036-0efe-475c-8b05-b4e98e34adc3', rabbinic_interpretive_monopoly, conventional).
narrative_ontology:cs_reference_frame('b7135036-0efe-475c-8b05-b4e98e34adc3', rabbinic_ritual_continuity_framework).
narrative_ontology:cs_drift_state('b7135036-0efe-475c-8b05-b4e98e34adc3', post_emancipation_secularization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b7135036-0efe-475c-8b05-b4e98e34adc3', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, practicing_community).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_jews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, practicing_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and interprets halakhic norms that define valid ritual practice. Sets the criteria for Jewish continuity and boundary maintenance. Derives institutional legitimacy from the claim that survival depends on ritual continuity under rabbinic interpretation. Cannot easily abandon this framework without undermining its own authority and the institutional structure it maintains.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    institutional, generational, constrained, global).

% Identify as Jewish but do not observe rabbinically defined ritual practice. Under this constraint's framing, they are failing to transmit authentic Jewish survival and their Jewishness is rendered incomplete or illegitimate. They bear the cost of identity erosion and exclusion from communal recognition, yet cannot fully exit the ethnic and cultural identity category that the constraint defines as dependent on practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    moderate, biographical, identity_locked, global).

% Observes ritual practice and receives the benefits of recognized Jewish identity, intergenerational continuity, and communal belonging. Pays compliance costs in time, resources, and autonomy of interpretation. Their Jewish legitimacy is recognized but conditional on continued performance of the rituals that the authority defines as essential.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, practicing_community, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__symbol_survival_reading, practicing_community, payer).

% Offer alternative frameworks for Jewish continuity (cultural, ethical, national, secular-humanist) that do not depend on rabbinic ritual. Structurally excluded from the conversation about legitimate survival because the constraint defines survival as ritual continuity under rabbinic authority, rendering their alternative frameworks invisible or deviant.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, non_rabbinic_movements, excluded,
    moderate, biographical, constrained, national).

% Study collective memory and post-traumatic survival mechanisms across cultures. Observe that ritual is one of many possible survival vectors (including language, politics, narrative, territory) and note when a single vector is monopolized by an authority structure that benefits from defining all alternatives as non-survival.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, memory_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__symbol_survival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves Jewish collective identity and intergenerational continuity across catastrophic historical ruptures by encoding shared symbolic experience through ritual practice, creating a bounded community of memory that persists without political sovereignty or territorial concentration.
% TRANSFER_FUNCTION: Moves authority to define legitimate Jewish identity and survival from dispersed cultural, linguistic, and political vectors to centralized rabbinic interpretive institutions; moves the burden of identity maintenance â time, practice, compliance, and subordination to halakhic norms â onto individuals and families who must perform continuity to be recognized as surviving Jews.
% ABSENT_VOICES: Secularized Jews who maintain ethnic, cultural, or political Jewish identities without ritual observance are structurally absent from the survival narrative; non-rabbinic Jewish movements (Reconstructionist, secular-humanist, Yiddishist, cultural Zionist) are marginalized because their existence contradicts the premise that survival requires rabbinic ritual form.
% DISAPPEARANCE_RATIONALE: If the equation of survival with ritual continuity vanished overnight, Jewish identity boundaries would expand dramatically to include secular and cultural forms; rabbinic interpretive monopoly would lose its gatekeeping function; intermarriage and assimilation would be reclassified as continuity variants rather than terminal threats; communal resources, institutional prestige, and educational systems would shift from ritual institutions to cultural, political, and academic structures.
% FOUNDING_PROBLEM: Jewish collective survival after the destruction of the Second Temple, and later after the Shoah and diasporic dispersion, in the absence of political sovereignty, territorial concentration, or military power.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and Orthodox historiography attest the problem is live and solved by ritual continuity. Secular Jewish historians, Zionist political thinkers, and Yiddishist cultural movements attest that Jewish survival has always had multiple vectors (political, linguistic, cultural, ethnic) and that the ritual-framing is a rabbinic reduction of a more robust peoplehood. Memory studies scholars outside the Jewish communal debate corroborate that post-traumatic collectives routinely generate multiple survival strategies beyond ritual.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.8, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.80) because defining survival as ritual continuity extracts identity legitimacy from those who do not practice, forcing them into an identity-locked payer position where exit from ritual is coded as exit from survival. Suppression (0.68) is substantial because alternative definitions of Jewishness (ethnic, cultural, political, linguistic) are actively marginalized by rabbinic gatekeeping and communal recognition rules. Theater ratio (0.52) is moderate-high: ritual performance genuinely encodes memory and coordinates collective identity, but an increasing share of enforcement energy is devoted to performative boundary-maintenance that serves rabbinic authority more than communal survival. Accessibility collapse (0.70) reflects that once the ritual-survival frame is accepted, alternatives appear as assimilation or disappearance rather than valid continuity. Resistance (0.45) is moderate: secularized Jews and alternative movements contest the definition, but they lack institutional power to change the authoritative frame.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic authority seat experiences this constraint as sacred stewardship of an embattled peoplehood, a necessary bulwark against assimilation after catastrophe. The secularized Jew seat experiences it as an identity trap: their Jewishness is recognized as real enough to create obligation but denied as legitimate enough to count as survival. The engine computes this divergence from the structural data â beneficiary/victim roles, exit options, and scope â without requiring the claim to adjudicate the dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority is the structural beneficiary: it receives low directionality because the constraint subsidizes its interpretive monopoly and institutional position. Secularized Jews are the structural target: they receive high directionality because the constraint extracts identity legitimacy from them and their exclusion is the enforcement object. The practicing community sits near the middle â they benefit from recognized continuity but pay compliance costs, and their exit is constrained by the same identity-lock mechanism that punishes secularization.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â how to survive catastrophic loss of sovereignty and territory â was genuinely live in the rabbinic era and remained acute after the Shoah. The mandatrophy question is whether the solution (ritual continuity as survival) has become extractive: the ritual form is now defended not only for memory but because it underwrites rabbinic authority's power to define who counts. The status is contested because some argue the problem is still live (antisemitism, assimilation pressure), while others argue the ritual-monopoly framing now serves institutional power more than the survival of the people, who persist through multiple non-ritual vectors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_vector_exclusivity,
    'Does Jewish collective survival depend exclusively on ritual continuity, or are ethnic, linguistic, political, and cultural continuity equally valid vectors of peoplehood?',
    'Comparative historical analysis of Jewish communities that survived or dissolved with varying degrees of ritual observance, alongside ethnographic study of secular Jewish identity formation.',
    'If non-ritual vectors are empirically valid, the constraint''s extraction is higher than its coordination function suggests, and the rabbinic monopoly is a gatekeeping structure rather than a necessary survival mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_vector_exclusivity, conceptual, 'Whether ritual continuity is the only valid survival vector').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the marginalization of secular Jewish identity structural (institutional non-recognition, exclusion from communal resources and legal status) or internalized (secular Jews accepting their own inauthenticity under the ritual frame)?',
    'Ethnographic study of secular Jewish identity formation and post-exit trajectory: if suppression of Jewish identity claims persists after structural barriers are removed, the mechanism is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure because the target carries the constraint with them after institutional exit; reform would require cultural intervention, not just institutional opening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    authority_framing_alternative,
    'Is rabbinic authority best framed as lineage-based transmission of a fixed kernel, or as an extraction structure that stabilizes the kernel to maintain institutional power?',
    'Comparative analysis of rabbinic authority''s resource flows, boundary-setting behavior, and resistance to kernel revision across different Jewish communities; if authority blocks revisions that would diminish its role regardless of communal benefit, the extraction framing is stronger.',
    'If the extraction framing is adopted, the constraint''s directionality for rabbinic authority shifts toward a higher d value, treating the authority as partly implicated in the extraction rather than as a pure beneficiary, and effective extraction may be higher than the lineage framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_alternative, conceptual, 'Alternative CS framing of rabbinic authority as extraction vs lineage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 40, 0.47).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 40, 0.77).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 50, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
